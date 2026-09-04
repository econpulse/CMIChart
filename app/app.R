# app.R - LUKB Morgeninfo Chart Generator

# Lade alle modularen Skripte aus R/
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {
  r_files <- list.files("app/R", pattern = "\\.R$", full.names = TRUE)
}
invisible(lapply(r_files, source))

# Datenbank initialisieren
init_db()

# UI-Definition
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  fluidRow(
    # Linke Spalte: Datenimport, DB-Abfrage & Vorlagen
    column(
      width = 3,
      card(
        card_header("Data Handling"),
        fileInput("btn_file1", "Excel-Datei hochladen", multiple = FALSE, accept = c(".xlsx")),
        textInput("textInput_dbticker", "DB ticker"),
        textInput("textInput_dblabel", "Label ticker(;)"),
        actionButton("btn_pulldb", "Get DB data"),
        textInput("textInput_chart_name", "Chart Name"),
        downloadButton('btn_downloadPlot', 'Download Plot'),
        div(
          style = "display: flex; gap: 5px; margin-top: 5px; margin-bottom: 5px;",
          actionButton("btn_save_chart", "Save Chart", class = "btn-primary"),
          actionButton("btn_manage_charts", "Vorlagen verwalten", class = "btn-secondary", icon = icon("folder-open"))
        )
      )
    ),
    
    # Rechte Spalte: Chart Preview & Steuerungspanels
    column(
      width = 9,
      card(
        card_header("Chart Preview"),
        div(
          style = "display: flex; justify-content: center;",
          plotOutput("contents", width = "600px", height = "400px")
        )
      ),
      card(
        card_header("Chart Controls"),
        navset_pill_list(
          nav_panel("X-Axis",
                    textInput("textInput_date_start", "Startdatum TT.MM.JJJJ", value = paste0("01.01.", year(Sys.Date()) - 4)),
                    textInput("textInput_date_end", "Enddatum TT.MM.JJJJ"),
                    textInput("textInput_date_breaks", "X-Achse-Frequenz (z.B. '1 weeks')", ""),
                    textInput("textInput_date_labels", "X-Achse-Label (z.B. '%d %m %Y')", "%b %Y")
          ),
          nav_panel("Y-Axis",
                    textInput("textInput_y_nachkomma", "Y-Achse-Nachkommastellen", "Auto"),
                    textInput("textInput_y_limits", "Min und Max der y-Achse"),
                    textInput("textInput_manual_y_breaks", "Manuelle y-Breaks")
          ),
          nav_panel("Gridlines",
                    textInput("textInput_horizon_lines", "Horizontale Linie(n) bei"),
                    textInput("textInput_vertical_lines", "Vertikale Linie(n) bei (Format: JJJJ-MM-TT)")
          ),
          nav_panel("Facets",
                    selectInput("selectInput_facet", label = "Minicharts",
                                choices = c("keine" = "none", "Fixiert" = "fixed", "Y-flex" = "free_y", "X-flex" = "free_x", "Ganz flex" = "free"))
          ),
          nav_panel("Legend",
                    checkboxInput("checkboxInput_show_legend", "Legende zeigen", value = TRUE),
                    selectInput("selectInput_legendposition", label = "Position der Legende", choices = c("bottom", "right", "none", "topleft"), selected = "bottom"),
                    textInput("textInput_legendrows", label = "#Zeilen in Legende", value = "1")
          ),
          nav_panel("Chart Types",
                    selectizeInput("selectizeInput_series_selection", "Auswahl der Serien", choices = NULL, multiple = TRUE,
                                   options = list(plugins = list("drag_drop"))),
                    radioButtons("radioInput_chart_category", "Diagrammart",
                                 choices = c("Zeitreihendiagramm", "Saisonalität", "Kategoriendiagramm"),
                                 selected = "Zeitreihendiagramm"),
                    conditionalPanel(
                      condition = "input.radioInput_chart_category == 'Saisonalität'",
                      radioButtons("radioInput_season_type", "Saison-Variante",
                                   choices = c(
                                     "Min-Max-Bereich (Vorjahre) + Aktuelles Jahr" = "minmax",
                                     "Alle Jahre (Vorjahre grau, Aktuelles Jahr farbig)" = "all_years"
                                   ),
                                   selected = "minmax")
                    ),
                    conditionalPanel(
                      condition = "input.radioInput_chart_category == 'Zeitreihendiagramm'",
                      checkboxGroupButtons("checkboxGroupButtons_series_geom_type", "Auswahl der Zeitreihen-Typen",
                                           choices = c("Linie", "Stufen", "Balken", "Punkt", "Fläche"),
                                           selected = "Linie"),
                      checkboxInput("checkboxInput_position_stack", "Zeitreihen stapeln", value = FALSE),
                      checkboxInput("checkboxInput_index_100", "Auf Startwert 100 indexieren", value = FALSE)
                    ),
                    conditionalPanel(
                      condition = "input.radioInput_chart_category == 'Zeitreihendiagramm' || input.radioInput_chart_category == 'Saisonalität'",
                      checkboxInput("checkboxInput_show_lastpoint", "Letzten Wert hervorheben", value = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.radioInput_chart_category == 'Kategoriendiagramm'",
                      radioButtons("radioInput_category_type", "Kategorie-Typ",
                                   choices = c("Kuchendiagramm", "Balkendiagramm"),
                                   selected = "Kuchendiagramm"),
                      conditionalPanel(
                        condition = "input.radioInput_category_type == 'Kuchendiagramm'",
                        sliderInput("sliderInput_pie_rotation", "Kuchendiagramm Rotation (Grad)", min = 0, max = 360, value = 0, step = 5)
                      ),
                      conditionalPanel(
                        condition = "input.radioInput_category_type == 'Balkendiagramm'",
                        radioButtons("radioInput_bar_orientation", "Ausrichtung",
                                     choices = c("Vertikal", "Horizontal"),
                                     selected = "Horizontal")
                      )
                    )
          ),
          nav_panel("Zeitreihen transformieren",
                    uiOutput("transformation_controls")
          ),
          nav_panel("Chart-Titel & Farben",
                    textInput("textInput_subtitle", "Charttitel"),
                    selectInput("selectInput_colors", "Farbskala", c("LUKB Private" = "pb", "LUKB Corporate" = "cd"))
          )
        )
      )
    )
  )
)

# Server-Definition
server <- function(input, output, session) {
  
  # ReactiveValues zur Speicherung der Daten
  source_data <- reactiveValues(
    excel = tibble(label = NA, date = NA, value = NA),
    db = tibble(label = NA, date = NA, value = NA)
  )
  
  safe_labels <- function(df) {
    if (is.null(df) || !("label" %in% names(df))) return(character(0))
    df[["label"]]
  }
  
  # Benutzerdefinierte Zeitreihen
  custom_series <- reactiveVal(list())
  
  # Temporärer Speicher für geladene Inputs zur Erhaltung bei Re-Renderings
  loaded_inputs <- reactiveVal(NULL)
  
  # Verfügbare Serien-Labels (inkl. berechneter Custom-Serien)
  available_labels <- reactive({
    raw_lbls <- unique(c(safe_labels(source_data$db), safe_labels(source_data$excel)))
    raw_lbls <- na.omit(raw_lbls)
    cust_lbls <- names(custom_series())
    c(raw_lbls, cust_lbls)
  })
  
  # Aktualisierung des Serien-Auswahl-Dropdowns
  observe({
    labels <- available_labels()
    current_selected <- input$selectizeInput_series_selection
    new_selected <- intersect(current_selected, labels)
    if (length(current_selected) == 0 || length(new_selected) == 0) {
      new_selected <- labels
    }
    
    updateSelectizeInput(
      session, "selectizeInput_series_selection",
      choices = labels,
      selected = new_selected
    )
  })
  
  # Custom-Zeitreihe hinzufügen
  observeEvent(input$btn_add_custom, {
    name <- str_squish(input$textInput_custom_name)
    formula_str <- str_squish(input$textInput_custom_formula)
    
    if (name == "") {
      showNotification("Bitte geben Sie einen Namen für die neue Zeitreihe ein.", type = "error")
      return()
    }
    if (formula_str == "") {
      showNotification("Bitte geben Sie eine Formel ein.", type = "error")
      return()
    }
    
    existing <- available_labels()
    if (name %in% existing) {
      showNotification("Dieser Name existiert bereits.", type = "error")
      return()
    }
    
    labels_raw <- unique(c(safe_labels(source_data$db), safe_labels(source_data$excel)))
    labels_raw <- na.omit(labels_raw)
    label_map <- setNames(LETTERS[1:length(labels_raw)], labels_raw)
    
    formula_upper <- toupper(formula_str)
    if (!is_safe_formula(formula_upper, allowed_vars = as.character(label_map))) {
      showNotification("Ungültige oder unsichere Formel. Erlaubt sind A, B, C... sowie ACH, PCH, ABS, MAV, LAG, LOG.", type = "error")
      return()
    }
    
    current <- custom_series()
    current[[name]] <- list(name = name, formula = formula_str)
    custom_series(current)
    
    updateTextInput(session, "textInput_custom_name", value = "")
    updateTextInput(session, "textInput_custom_formula", value = "")
    showNotification(paste0("Zeitreihe '", name, "' hinzugefügt."), type = "message")
  })
  
  # Dropdown für Custom-Serien-Löschung aktualisieren
  observe({
    cust_names <- names(custom_series())
    updateSelectInput(session, "selectInput_delete_custom", choices = cust_names)
  })
  
  # Custom-Zeitreihe löschen
  observeEvent(input$btn_delete_custom, {
    selected <- input$selectInput_delete_custom
    req(selected)
    current <- custom_series()
    current[[selected]] <- NULL
    custom_series(current)
    showNotification(paste0("Zeitreihe '", selected, "' entfernt."), type = "message")
  })
  
  output$has_custom_series <- reactive({
    length(custom_series()) > 0
  })
  outputOptions(output, "has_custom_series", suspendWhenHidden = FALSE)
  
  # Dynamische UI für Zeitreihentransformationen rendern
  output$transformation_controls <- renderUI({
    raw_labels <- unique(c(safe_labels(source_data$db), safe_labels(source_data$excel)))
    raw_labels <- na.omit(raw_labels)
    
    if (length(raw_labels) == 0) {
      return(p("Keine Zeitreihen geladen. Bitte laden Sie eine Excel-Datei hoch oder rufen Sie DB-Daten ab.", style = "color: #777; font-style: italic;"))
    }
    
    label_map <- setNames(LETTERS[1:length(raw_labels)], raw_labels)
    
    # Legende (A = Name, B = Name, ...)
    legend_items <- lapply(names(label_map), function(lbl) {
      letter <- label_map[[lbl]]
      tags$li(
        tags$b(letter, style = "color: #005A36;"), " = ", lbl,
        style = "margin-bottom: 5px; list-style-type: none;"
      )
    })
    
    # Eingabefelder für Original-Serien
    formula_inputs <- lapply(names(label_map), function(lbl) {
      letter <- label_map[[lbl]]
      input_id <- paste0("textInput_formula_", letter)
      
      current_val <- NULL
      if (!is.null(loaded_inputs()) && !is.null(loaded_inputs()[[input_id]])) {
        current_val <- loaded_inputs()[[input_id]]
      } else if (!is.null(input[[input_id]])) {
        current_val <- input[[input_id]]
      } else {
        current_val <- letter
      }
      
      div(
        style = "margin-bottom: 10px;",
        tags$label(paste0(letter, " (", lbl, ")"), `for` = input_id, style = "font-weight: bold; font-size: 0.9rem; margin-bottom: 3px;"),
        textInput(input_id, label = NULL, value = current_val, placeholder = paste0("z.B. PCH(", letter, ", 12)"))
      )
    })
    
    custom_ui <- div(
      tags$h6("Neue Zeitreihe erstellen", style = "color: #005A36; font-weight: bold; margin-top: 0px;"),
      fluidRow(
        column(width = 6, textInput("textInput_custom_name", "Name (z.B. Spread)", value = "")),
        column(width = 6, textInput("textInput_custom_formula", "Formel (z.B. A - B)", value = ""))
      ),
      actionButton("btn_add_custom", "Hinzufügen", class = "btn-primary btn-sm", style = "margin-bottom: 15px;"),
      
      conditionalPanel(
        condition = "output.has_custom_series",
        tags$h6("Custom-Zeitreihe löschen", style = "color: #d9534f; font-weight: bold; margin-top: 10px; border-top: 1px solid #eee; padding-top: 10px;"),
        fluidRow(
          column(width = 8, selectInput("selectInput_delete_custom", label = NULL, choices = NULL)),
          column(width = 4, actionButton("btn_delete_custom", "Löschen", class = "btn-danger btn-sm", style = "margin-top: 0px;"))
        )
      )
    )
    
    fluidRow(
      column(
        width = 6,
        tags$h6("Original-Zeitreihen anpassen", style = "color: #005A36; font-weight: bold; margin-bottom: 10px;"),
        tags$ul(legend_items, style = "padding-left: 0; margin-bottom: 15px;"),
        formula_inputs
      ),
      column(
        width = 6,
        custom_ui
      )
    )
  })
  
  # Trigger für Vorlagentabellen-Updates
  trigger_charts_update <- reactiveVal(0)
  
  # Modal zur Verwaltung gespeicherter Charts anzeigen
  show_manage_modal <- function() {
    showModal(modalDialog(
      title = "Saved Charts (Vorlagen) verwalten",
      size = "l",
      easyClose = TRUE,
      DTOutput("modal_charts_table"),
      footer = tagList(
        actionButton("btn_modal_delete", "", class = "btn-danger", icon = icon("trash"), style = "margin-right: auto;"),
        actionButton("btn_modal_load", "Ausgewählte Vorlage laden", class = "btn-primary", icon = icon("download")),
        modalButton("Schliessen")
      )
    ))
  }
  
  observeEvent(input$btn_manage_charts, {
    show_manage_modal()
  })
  
  # DT Tabelle im Vorlagen-Modal rendern
  output$modal_charts_table <- renderDT({
    trigger_charts_update()
    df <- get_saved_charts_df()
    datatable(
      df,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        language = list(
          search = "Suchen:",
          lengthMenu = "_MENU_ Einträge anzeigen",
          info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen",
          infoEmpty = "Keine Einträge vorhanden",
          infoFiltered = "(gefiltert aus _MAX_ Einträgen)",
          zeroRecords = "Keine passenden Vorlagen gefunden",
          paginate = list(first = "Erste", previous = "Zurück", `next` = "Weiter", last = "Letzte")
        )
      )
    )
  })
  
  # Chart in DB speichern
  observeEvent(input$btn_save_chart, {
    chart_name <- str_squish(input$textInput_chart_name)
    if (chart_name == "") {
      showNotification("Bitte geben Sie einen Chart Namen ein.", type = "error")
      return()
    }
    
    list_of_inputs <- reactiveValuesToList(input, all.names = TRUE)
    data_list <- list(
      db = source_data$db, 
      excel = source_data$excel,
      custom_series = custom_series()
    )
    
    save_chart_db(chart_name, list_of_inputs, data_list)
    trigger_charts_update(trigger_charts_update() + 1)
    showNotification(paste0("Chart '", chart_name, "' wurde in der Datenbank gespeichert."), type = "message")
  })
  
  # Ausgewählte Vorlage aus Modal laden
  observeEvent(input$btn_modal_load, {
    req(input$modal_charts_table_rows_selected)
    df <- get_saved_charts_df()
    selected_name <- df$Name[input$modal_charts_table_rows_selected]
    req(selected_name)
    
    saved_chart <- load_chart_db(selected_name)
    req(saved_chart)
    
    restored_data <- saved_chart$data
    saved_inputs <- saved_chart$inputs
    
    loaded_inputs(saved_inputs)
    session$onFlushed(function() {
      loaded_inputs(NULL)
    }, once = TRUE)
    
    if (!is.null(restored_data$custom_series)) {
      custom_series(restored_data$custom_series)
    } else {
      custom_series(list())
    }
    
    db_ticker_val <- saved_inputs[["textInput_dbticker"]]
    db_label_val <- saved_inputs[["textInput_dblabel"]]
    
    if (!is.null(db_ticker_val) && str_squish(db_ticker_val) != "") {
      tryCatch({
        ticker <- str_split(db_ticker_val, ";")[[1]] |> toupper()
        db_data <- get_db_data(ticker)
        labels <- str_split(db_label_val, ";")[[1]] |> str_squish()
        length(labels) <- length(ticker)
        ticker_label_df <- tibble(ticker = ticker, label = labels) |> 
          mutate(label = case_when(is.na(label) | label == "" ~ ticker, TRUE ~ label))
        
        source_data$db <- db_data |>
          left_join(ticker_label_df, by = "ticker") |>
          select(label, date, value)
        showNotification("Neueste Datenbank-Daten wurden geladen.", type = "message")
      }, error = function(e) {
        showNotification(paste("Automatisches DB-Update fehlgeschlagen:", e$message), type = "warning")
        if (!is.null(restored_data$db) && nrow(restored_data$db) > 0) {
          db_df <- as_tibble(restored_data$db)
          if ("date" %in% names(db_df)) db_df$date <- as.Date(db_df$date)
          source_data$db <- db_df
        } else {
          source_data$db <- tibble(label = NA, date = NA, value = NA)
        }
      })
    } else {
      if (!is.null(restored_data$db) && nrow(restored_data$db) > 0) {
        db_df <- as_tibble(restored_data$db)
        if ("date" %in% names(db_df)) db_df$date <- as.Date(db_df$date)
        source_data$db <- db_df
      } else {
        source_data$db <- tibble(label = NA, date = NA, value = NA)
      }
    }
    
    if (!is.null(restored_data$excel) && nrow(restored_data$excel) > 0) {
      excel_df <- as_tibble(restored_data$excel)
      if ("date" %in% names(excel_df)) excel_df$date <- as.Date(excel_df$date)
      source_data$excel <- excel_df
    } else {
      source_data$excel <- tibble(label = NA, date = NA, value = NA)
    }
    
    raw_lbls <- unique(c(safe_labels(source_data$db), safe_labels(source_data$excel)))
    raw_lbls <- na.omit(raw_lbls)
    cust_lbls <- names(custom_series())
    all_labels <- c(raw_lbls, cust_lbls)
    
    freezeReactiveValue(input, "selectizeInput_series_selection")
    updateSelectizeInput(
      session, "selectizeInput_series_selection",
      choices = all_labels,
      selected = saved_chart$inputs$selectizeInput_series_selection
    )
    
    updateTextInput(session, "textInput_chart_name", value = selected_name)
    
    for (name in names(saved_inputs)) {
      val <- saved_inputs[[name]]
      if (name == "selectizeInput_series_selection") next
      
      if (grepl("checkboxIn|checkboxGroup", name)) {
        if (name == "checkboxGroupButtons_series_geom_type") {
          updateCheckboxGroupButtons(session, name, selected = val)
        } else {
          updateCheckboxInput(session, name, value = as.logical(val))
        }
      } else if (grepl("selectizeInput|selectInput", name)) {
        updateSelectInput(session, name, selected = val)
      } else if (grepl("textInput", name)) {
        updateTextInput(session, name, value = as.character(val))
      } else if (grepl("sliderInput", name)) {
        updateSliderInput(session, name, value = as.numeric(val))
      } else if (grepl("radioInput", name)) {
        updateRadioButtons(session, name, selected = as.character(val))
      }
    }
    
    removeModal()
    showNotification(paste0("Chart '", selected_name, "' erfolgreich geladen."), type = "message")
  })
  
  # Vorlage löschen
  temp_delete_name <- reactiveVal(NULL)
  
  observeEvent(input$btn_modal_delete, {
    req(input$modal_charts_table_rows_selected)
    df <- get_saved_charts_df()
    selected_name <- df$Name[input$modal_charts_table_rows_selected]
    req(selected_name)
    
    temp_delete_name(selected_name)
    showModal(modalDialog(
      title = "Vorlage löschen",
      paste0("Möchten Sie die Vorlage '", selected_name, "' wirklich unwiderruflich löschen?"),
      footer = tagList(
        actionButton("btn_modal_delete_confirm", "Ja, löschen", class = "btn-danger"),
        actionButton("btn_modal_delete_cancel", "Abbrechen")
      )
    ))
  })
  
  observeEvent(input$btn_modal_delete_confirm, {
    selected_name <- temp_delete_name()
    req(selected_name)
    
    delete_chart_db(selected_name)
    temp_delete_name(NULL)
    trigger_charts_update(trigger_charts_update() + 1)
    
    removeModal()
    show_manage_modal()
    showNotification(paste0("Chart '", selected_name, "' wurde gelöscht."), type = "message")
  })
  
  observeEvent(input$btn_modal_delete_cancel, {
    temp_delete_name(NULL)
    removeModal()
    show_manage_modal()
  })
  
  # Excel-Upload Observer
  observeEvent(input$btn_file1, {
    tryCatch({
      data <- read_excel(input$btn_file1$datapath)
      codenames <- which(grepl("code|name|date", tolower(colnames(data))))
      
      df <- map_df(seq(1, length(codenames), 1), function(i) {
        start_col <- codenames[i]
        end_col <- ifelse(i < length(codenames), codenames[i + 1] - 1, ncol(data))
        
        temp <- data[, c(seq(start_col, end_col, 1))] %>%
          pivot_longer(c(2:ncol(.)), names_to = "label", values_transform = list(value = as.numeric)) %>%
          rename(date = 1)
        
        if (is.numeric(temp$date)) {
          temp$date <- as.Date(temp$date, origin = "1899-12-30")
        } else {
          temp$date <- as.Date(temp$date)
        }
        temp %>%
          arrange(label, date) %>%
          filter(!is.na(value))
      })
      
      source_data$excel <- df
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  # DB-Abfrage Observer
  observeEvent(input$btn_pulldb, {
    ticker <- str_split(input$textInput_dbticker, ";")[[1]] |> toupper()
    db_data <- get_db_data(ticker)
    labels <- str_split(input$textInput_dblabel, ";")[[1]] |> str_squish()
    length(labels) <- length(ticker)
    ticker_label_df <- tibble(ticker = ticker, label = labels) |> 
      mutate(label = case_when(is.na(label) | label == "" ~ ticker, TRUE ~ label))
    
    source_data$db <- db_data |>
      left_join(ticker_label_df, by = "ticker") |>
      select(label, date, value)
  })
  
  # Datenverarbeitung und Plot-Generierung
  make_plot_from_data <- function() {
    df_raw <- bind_rows(source_data$db, source_data$excel)
    req(df_raw)
    req("value" %in% names(df_raw))
    
    raw_labels <- unique(na.omit(c(safe_labels(source_data$db), safe_labels(source_data$excel))))
    req(length(raw_labels) > 0)
    
    label_map <- setNames(LETTERS[1:length(raw_labels)], raw_labels)
    original_formula_map <- list()
    for (lbl in names(label_map)) {
      letter <- label_map[[lbl]]
      input_id <- paste0("textInput_formula_", letter)
      original_formula_map[[letter]] <- input[[input_id]]
    }
    
    df <- transform_chart_data(
      df_raw = df_raw,
      original_formula_map = original_formula_map,
      custom_series_list = custom_series(),
      selected_series = input$selectizeInput_series_selection
    )
    
    req(df)
    req(nrow(df) > 0)
    
    selected_palette <- get_chart_palette(input$selectInput_colors)
    render_lukb_chart(df = df, input = input, fill_colors = selected_palette)
  }
  
  # Plot Output
  output$contents <- renderPlot({
    make_plot_from_data()
  })
  
  # Plot Download Handler
  output$btn_downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      showtext_opts(dpi = 300)
      ggsave(file, make_plot_from_data(), width = 17.4, height = 8.7, units = "cm")
      showtext_opts(dpi = 96)
    }
  )
}

shinyApp(ui = ui, server = server)