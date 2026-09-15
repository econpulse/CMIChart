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
  useShinyjs(),
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
        actionButton("btn_edit_data", "Edit Data", class = "btn-outline-primary", icon = icon("table"), style = "margin-top: 8px; margin-bottom: 5px; width: 100%;"),
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
                                   selected = "minmax"),
                      checkboxInput("checkboxInput_season_index_100", "Auf Startwert 100 indexieren (1. Jan = 100)", value = FALSE)
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
                    selectInput("selectInput_colors", "Farbskala", c("LUKB Corporate" = "cd", "LUKB Private" = "pb"), selected = "cd")
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
    db = tibble(label = NA, date = NA, value = NA),
    edited = NULL
  )
  
  safe_labels <- function(df) {
    if (is.null(df) || !("label" %in% names(df))) return(character(0))
    df[["label"]]
  }
  
  # Aktuell aktiver Rohdatensatz (editiert oder aus DB/Excel)
  get_current_raw_data <- reactive({
    if (!is.null(source_data$edited) && nrow(source_data$edited) > 0) {
      source_data$edited
    } else {
      bind_rows(source_data$db, source_data$excel)
    }
  })
  
  # Button 'Edit Data' nur aktivieren wenn überhaupt Daten vorhanden sind
  observe({
    raw_df <- get_current_raw_data()
    has_data <- !is.null(raw_df) && nrow(raw_df) > 0 && "value" %in% names(raw_df) && any(!is.na(raw_df$value))
    shinyjs::toggleState("btn_edit_data", condition = has_data)
  })
  
  # Keep track of transformations
  series_transformations <- reactiveValues()
  
  # Verfügbare Serien-Labels
  available_labels <- reactive({
    raw_df <- get_current_raw_data()
    raw_lbls <- unique(safe_labels(raw_df))
    na.omit(raw_lbls)
  })
  
  observe({
    labels <- available_labels()
    for (lbl in labels) {
      if (is.null(series_transformations[[lbl]])) {
        series_transformations[[lbl]] <- list(type = "Rohwert", lag = 12, offset = 0)
      }
    }
  })
  
  observe({
    labels <- available_labels()
    for (lbl in labels) {
      sanitized <- sanitize_id(lbl)
      type_id <- paste0("trans_type_", sanitized)
      lag_id <- paste0("trans_lag_", sanitized)
      offset_id <- paste0("trans_offset_", sanitized)
      
      if (!is.null(input[[type_id]])) {
        current <- isolate(series_transformations[[lbl]])
        new_type <- input[[type_id]]
        new_lag <- input[[lag_id]]
        if (is.null(new_lag) || is.na(new_lag)) {
          new_lag <- 12
        }
        new_offset <- input[[offset_id]]
        if (is.null(new_offset) || is.na(new_offset)) {
          new_offset <- 0
        }
        
        if (is.null(current) || current$type != new_type || current$lag != new_lag || is.null(current$offset) || current$offset != new_offset) {
          series_transformations[[lbl]] <- list(
            type = new_type,
            lag = as.numeric(new_lag),
            offset = as.numeric(new_offset)
          )
        }
      }
    }
  })
  
  # Temporärer Speicher für geladene Inputs zur Erhaltung bei Re-Renderings
  loaded_inputs <- reactiveVal(NULL)
  
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
  
  # Dynamische UI für Zeitreihentransformationen rendern
  output$transformation_controls <- renderUI({
    all_labels <- available_labels()
    
    current_selected <- input$selectizeInput_series_selection
    labels <- if (!is.null(current_selected)) {
      intersect(current_selected, all_labels)
    } else {
      all_labels
    }
    
    if (length(labels) == 0) {
      return(p("Keine Zeitreihen ausgewählt oder geladen. Bitte laden Sie Daten hoch und wählen Sie unter 'Chart Types' die gewünschten Serien aus.", style = "color: #777; font-style: italic;"))
    }
    
    ui_elements <- lapply(labels, function(lbl) {
      sanitized <- sanitize_id(lbl)
      
      current_val <- series_transformations[[lbl]]
      selected_type <- if (!is.null(current_val)) current_val$type else "Rohwert"
      selected_lag <- if (!is.null(current_val)) current_val$lag else 12
      selected_offset <- if (!is.null(current_val) && !is.null(current_val$offset)) current_val$offset else 0
      
      div(
        style = "border-bottom: 1px solid #eee; padding-bottom: 15px; margin-bottom: 15px;",
        tags$h5(lbl, style = "color: #005A36; font-weight: bold; margin-bottom: 10px;"),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = paste0("trans_type_", sanitized),
              label = "Transformation",
              choices = c(
                "Rohwert" = "Rohwert",
                "Relative Veränderung (%)" = "pct_change",
                "Absolute Veränderung" = "abs_change",
                "Konstante addieren" = "add_constant"
              ),
              selected = selected_type
            )
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = sprintf("input['trans_type_%s'] == 'pct_change' || input['trans_type_%s'] == 'abs_change'", sanitized, sanitized),
              numericInput(
                inputId = paste0("trans_lag_", sanitized),
                label = "Perioden / Lag",
                value = selected_lag,
                min = 1,
                step = 1
              )
            ),
            conditionalPanel(
              condition = sprintf("input['trans_type_%s'] == 'add_constant'", sanitized),
              numericInput(
                inputId = paste0("trans_offset_", sanitized),
                label = "Konstanter Wert",
                value = selected_offset,
                step = 0.1
              )
            )
          )
        )
      )
    })
    
    do.call(tagList, ui_elements)
  })
  
  # Modal zur Datenbearbeitung (In-App Daten hinzufügen / editieren)
  edit_table_data <- reactiveVal(NULL)
  
  show_edit_data_modal <- function() {
    raw_df <- get_current_raw_data() %>% filter(!is.na(value), !is.na(date), !is.na(label))
    if (nrow(raw_df) == 0) {
      showNotification("Keine Daten zum Bearbeiten vorhanden.", type = "warning")
      return()
    }
    
    df_wide <- raw_df %>%
      mutate(date = as.character(date)) %>%
      pivot_wider(names_from = label, values_from = value, values_fn = mean) %>%
      arrange(date)
    
    edit_table_data(df_wide)
    
    showModal(modalDialog(
      title = tagList(icon("table"), " Edit Data"),
      size = "xl",
      easyClose = FALSE,
      div(
        style = "margin-bottom: 12px;",
        p(
          style = "color: #555; margin-bottom: 8px;",
          "Hier können Sie Datumsangaben (Format: JJJJ-MM-TT) und Werte der einzelnen Serien direkt editieren sowie am Tabellenende neue Zeilen anfügen. ",
          "Die Tabelle öffnet standardmässig beim ", strong("Tail (neueste Werte)"), "; durch Hochscrollen gelangen Sie zurück in die Vergangenheit.",
          tags$br(),
          tags$small(
            style = "color: #777;",
            "Hinweis: Spaltennamen sind in der Ansicht auf 15 Zeichen begrenzt. Neue Zeilen können am Tabellenende eingegeben, via Rechtsklick ('Row below') oder über den Button 'Neue Zeile' angefügt werden. ",
            "Die Änderungen verbleiben nur in der laufenden App-Sitzung und werden weder in die Datenbank zurückgeschrieben noch als Vorlagen-Input abgespeichert."
          )
        ),
        div(
          style = "display: flex; gap: 8px; margin-top: 10px;",
          actionButton("btn_add_row_hot", "Neue Zeile am Ende anfügen", icon = icon("plus"), class = "btn-sm btn-outline-secondary"),
          actionButton("btn_reset_edited_data", "Auf Originaldaten zurücksetzen", icon = icon("rotate-left"), class = "btn-sm btn-outline-danger")
        )
      ),
      rHandsontableOutput("hot_edit_table", height = "450px"),
      footer = tagList(
        actionButton("btn_apply_edit_data", "Änderungen übernehmen", class = "btn-success", icon = icon("check")),
        modalButton("Abbrechen")
      )
    ))
  }
  
  observeEvent(input$btn_edit_data, {
    show_edit_data_modal()
  })
  
  output$hot_edit_table <- renderRHandsontable({
    df <- edit_table_data()
    req(df)
    
    # Spaltennamen für die Anzeige auf maximal 15 Zeichen begrenzen
    orig_names <- names(df)
    display_headers <- if (length(orig_names) > 1) {
      c("date", substr(orig_names[-1], 1, 15))
    } else {
      orig_names
    }
    
    hot <- rhandsontable(
      df, 
      colHeaders = display_headers,
      stretchH = "all", 
      height = 430,
      minSpareRows = 1
    ) %>%
      hot_col(1, type = "date", dateFormat = "YYYY-MM-DD") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    
    if (ncol(df) > 1) {
      for (i in 2:ncol(df)) {
        hot <- hot %>% hot_col(i, type = "numeric")
      }
    }
    
    # Automatisch zum Tabellenende (neueste Werte / Tail) scrollen
    hot <- hot %>% htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot;
        var scrollToTail = function() {
          if (hot && hot.countRows && hot.countRows() > 0) {
            hot.scrollViewportTo(hot.countRows() - 1, 0);
            hot.render();
          }
        };
        setTimeout(scrollToTail, 50);
        setTimeout(scrollToTail, 200);
        setTimeout(scrollToTail, 450);
      }
    ")
    
    hot
  })
  
  observeEvent(input$btn_add_row_hot, {
    current_hot <- if (!is.null(input$hot_edit_table)) {
      hot_to_r(input$hot_edit_table)
    } else {
      edit_table_data()
    }
    req(current_hot)
    
    new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(current_hot)))
    names(new_row) <- names(current_hot)
    valid_dates <- na.omit(suppressWarnings(as.Date(current_hot$date)))
    if (length(valid_dates) > 0) {
      new_row$date <- as.character(max(valid_dates) + 1)
    } else {
      new_row$date <- as.character(Sys.Date())
    }
    
    edit_table_data(bind_rows(current_hot, new_row))
  })
  
  observeEvent(input$btn_reset_edited_data, {
    raw_orig <- bind_rows(source_data$db, source_data$excel) %>% filter(!is.na(value), !is.na(date), !is.na(label))
    if (nrow(raw_orig) > 0) {
      df_wide <- raw_orig %>%
        mutate(date = as.character(date)) %>%
        pivot_wider(names_from = label, values_from = value, values_fn = mean) %>%
        arrange(date)
      edit_table_data(df_wide)
      showNotification("Tabelle auf Originaldaten zurückgesetzt.", type = "message")
    }
  })
  
  observeEvent(input$btn_apply_edit_data, {
    req(input$hot_edit_table)
    hot_df <- hot_to_r(input$hot_edit_table)
    req(hot_df)
    
    # Originale Spaltennamen wiederherstellen (falls Display-Header gekürzt wurden)
    orig_cols <- names(edit_table_data())
    if (!is.null(orig_cols) && ncol(hot_df) == length(orig_cols)) {
      names(hot_df) <- orig_cols
    }
    
    hot_df_clean <- hot_df %>%
      filter(!is.na(date) & str_squish(as.character(date)) != "")
    
    parsed_dates <- suppressWarnings(as.Date(hot_df_clean$date))
    if (anyNA(parsed_dates)) {
      parsed_dates_alt <- suppressWarnings(as.Date(hot_df_clean$date, format = "%d.%m.%Y"))
      parsed_dates[is.na(parsed_dates)] <- parsed_dates_alt[is.na(parsed_dates)]
    }
    hot_df_clean$date <- parsed_dates
    hot_df_clean <- hot_df_clean %>% filter(!is.na(date))
    
    series_cols <- setdiff(names(hot_df_clean), "date")
    for (col in series_cols) {
      hot_df_clean[[col]] <- as.numeric(hot_df_clean[[col]])
    }
    
    hot_df_long <- hot_df_clean %>%
      pivot_longer(cols = all_of(series_cols), names_to = "label", values_to = "value") %>%
      filter(!is.na(value)) %>%
      arrange(label, date)
    
    if (nrow(hot_df_long) == 0) {
      showNotification("Keine gültigen Datenzeilen vorhanden.", type = "error")
      return()
    }
    
    source_data$edited <- hot_df_long
    removeModal()
    showNotification("Editierten Daten erfolgreich übernommen (nur in App).", type = "message")
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
      excel = source_data$excel
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
    
    db_ticker_val <- saved_inputs[["textInput_dbticker"]]
    db_label_val <- saved_inputs[["textInput_dblabel"]]
    
    if (!is.null(db_ticker_val) && str_squish(db_ticker_val) != "") {
      tryCatch({
        ticker <- str_split(db_ticker_val, ";")[[1]] |> str_squish() |> toupper()
        ticker <- ticker[ticker != ""]
        req(length(ticker) > 0)
        
        db_data <- get_db_data(ticker)
        if (!is.null(db_data) && nrow(db_data) > 0) {
          db_data <- db_data |> mutate(ticker = toupper(str_squish(ticker)))
          
          labels_raw <- if (!is.null(db_label_val) && !is.na(db_label_val)) {
            str_split(db_label_val, ";")[[1]] |> str_squish()
          } else {
            character(0)
          }
          
          ticker_label_df <- tibble(
            ticker = ticker,
            label_assigned = if (length(labels_raw) > 0) {
              c(labels_raw, rep("", max(0, length(ticker) - length(labels_raw))))[seq_along(ticker)]
            } else {
              rep("", length(ticker))
            }
          ) |>
            mutate(label = case_when(
              is.na(label_assigned) | label_assigned == "" ~ ticker,
              TRUE ~ label_assigned
            ))
          
          source_data$db <- db_data |>
            left_join(ticker_label_df |> select(ticker, label), by = "ticker") |>
            mutate(label = coalesce(label, ticker)) |>
            select(label, date, value)
          showNotification("Neueste Datenbank-Daten wurden geladen.", type = "message")
        } else {
          stop("Keine Daten für Ticker gefunden")
        }
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
    source_data$edited <- NULL
    
    raw_lbls <- available_labels()
    
    # Transformations-Einstellungen aus Vorlage wiederherstellen
    for (lbl in raw_lbls) {
      sanitized <- sanitize_id(lbl)
      saved_type <- saved_inputs[[paste0("trans_type_", sanitized)]]
      saved_lag <- saved_inputs[[paste0("trans_lag_", sanitized)]]
      saved_offset <- saved_inputs[[paste0("trans_offset_", sanitized)]]
      if (!is.null(saved_type)) {
        series_transformations[[lbl]] <- list(
          type = saved_type,
          lag = if (is.null(saved_lag)) 12 else as.numeric(saved_lag),
          offset = if (is.null(saved_offset)) 0 else as.numeric(saved_offset)
        )
      }
    }
    
    freezeReactiveValue(input, "selectizeInput_series_selection")
    updateSelectizeInput(
      session, "selectizeInput_series_selection",
      choices = raw_lbls,
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
      } else if (grepl("selectizeInput|selectInput|trans_type_", name)) {
        updateSelectInput(session, name, selected = val)
      } else if (grepl("numericInput|trans_lag_|trans_offset_", name)) {
        updateNumericInput(session, name, value = as.numeric(val))
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
      
      source_data$edited <- NULL
      source_data$excel <- df
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  # DB-Abfrage Observer
  observeEvent(input$btn_pulldb, {
    ticker_input <- input$textInput_dbticker
    req(ticker_input)
    
    ticker <- str_split(ticker_input, ";")[[1]] |> 
      str_squish() |> 
      toupper()
    ticker <- ticker[ticker != ""]
    req(length(ticker) > 0)
    
    db_data <- get_db_data(ticker)
    if (is.null(db_data) || nrow(db_data) == 0) {
      showNotification("Keine Daten für die angegebenen Ticker gefunden.", type = "warning")
      return()
    }
    
    db_data <- db_data |> mutate(ticker = toupper(str_squish(ticker)))
    
    label_input <- input$textInput_dblabel
    labels_raw <- if (!is.null(label_input) && !is.na(label_input)) {
      str_split(label_input, ";")[[1]] |> str_squish()
    } else {
      character(0)
    }
    
    ticker_label_df <- tibble(
      ticker = ticker,
      label_assigned = if (length(labels_raw) > 0) {
        c(labels_raw, rep("", max(0, length(ticker) - length(labels_raw))))[seq_along(ticker)]
      } else {
        rep("", length(ticker))
      }
    ) |>
      mutate(label = case_when(
        is.na(label_assigned) | label_assigned == "" ~ ticker,
        TRUE ~ label_assigned
      ))
    
    source_data$edited <- NULL
    source_data$db <- db_data |>
      left_join(ticker_label_df |> select(ticker, label), by = "ticker") |>
      mutate(label = coalesce(label, ticker)) |>
      select(label, date, value)
  })
  
  # Datenverarbeitung und Plot-Generierung
  make_plot_from_data <- function() {
    df_raw <- get_current_raw_data()
    req(df_raw)
    req("value" %in% names(df_raw))
    
    raw_labels <- available_labels()
    req(length(raw_labels) > 0)
    
    df <- transform_chart_data(
      df_raw = df_raw,
      series_transformations = reactiveValuesToList(series_transformations),
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