# utils_plot.R
# ggplot2-Visualisierungen: Zeitreihen-, Saisonalitäts-, Kuchen- und Balkendiagramme

# Erstellt Polygone für 2D-Kuchendiagramme
create_pie_polygons <- function(df_pie, radius = 1) {
  polygons <- purrr::map_df(1:nrow(df_pie), function(i) {
    row <- df_pie[i, ]
    theta <- seq(row$start_angle, row$end_angle, length.out = 30)
    tibble(
      label = row$label,
      x = c(0, radius * cos(theta), 0),
      y = c(0, radius * sin(theta), 0)
    )
  })
  polygons$label <- factor(polygons$label, levels = levels(df_pie$label))
  polygons
}

# Hilfsfunktion: Interpoliert fehlende Tage (z.B. Wochenenden, Feiertage) innerhalb eines Jahres
interpolate_season_year <- function(df_yr, is_latest_year = FALSE) {
  if (is.null(df_yr) || nrow(df_yr) == 0) return(df_yr)
  
  # Duplikate auf Tagesebene mitteln
  df_agg <- df_yr %>%
    group_by(dummy_date) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(dummy_date)
  
  if (nrow(df_agg) < 2) {
    return(tibble(
      dummy_date = df_agg$dummy_date,
      value = df_agg$value
    ))
  }
  
  min_d <- min(df_agg$dummy_date)
  max_d <- max(df_agg$dummy_date)
  
  # Für Jahre, die nahe an Jahresanfang reichen, ab 1. Januar interpolieren
  if (min_d <= as.Date("2024-01-10")) min_d <- as.Date("2024-01-01")
  # Für Vorjahre, die nahe an Jahresende reichen, bis 31. Dezember interpolieren
  if (!is_latest_year) {
    if (max_d >= as.Date("2024-12-20")) max_d <- as.Date("2024-12-31")
  }
  
  grid_dates <- seq.Date(min_d, max_d, by = "1 day")
  
  interp_res <- stats::approx(
    x = as.numeric(df_agg$dummy_date),
    y = as.numeric(df_agg$value),
    xout = as.numeric(grid_dates),
    rule = 2
  )
  
  tibble(
    dummy_date = grid_dates,
    value = interp_res$y
  )
}

# Saisonalitäts-Diagramm (Saison-Chart über gemapptes Kalenderjahr)
render_season_chart <- function(df, input, fill_colors = lukb_colors) {
  main_color <- fill_colors[1]
  
  # Datumsbereich filtern
  date_start <- if (!is.null(input$textInput_date_start) && input$textInput_date_start != "") {
    as.Date(input$textInput_date_start, "%d.%m.%Y")
  } else {
    min(df$date, na.rm = TRUE)
  }
  date_end <- if (!is.null(input$textInput_date_end) && input$textInput_date_end != "") {
    as.Date(input$textInput_date_end, "%d.%m.%Y")
  } else {
    max(df$date, na.rm = TRUE)
  }
  
  df_filtered <- df %>% filter(between(date, date_start, date_end))
  if (nrow(df_filtered) == 0) return(ggplot() + theme_void())
  
  # Datumsmapping auf das Standard-Schaltjahr 2024 (inkl. 29. Feb)
  df_season <- df_filtered %>%
    mutate(
      year_num = as.numeric(format(date, "%Y")),
      year = factor(format(date, "%Y")),
      dummy_date = as.Date(paste0("2024-", format(date, "%m-%d")), format = "%Y-%m-%d")
    ) %>%
    filter(!is.na(dummy_date)) %>%
    arrange(year_num, dummy_date)
  
  # Optional: Jedes Jahr auf Startwert 100 indexieren (1. Jan / erster verfügbarer Wert = 100)
  if (!is.null(input$checkboxInput_season_index_100) && isTRUE(input$checkboxInput_season_index_100)) {
    df_season <- df_season %>%
      group_by(label, year_num) %>%
      arrange(date) %>%
      mutate(
        first_val = first(value[!is.na(value)]),
        value = if_else(!is.na(first_val) & first_val != 0, (value / first_val) * 100, value)
      ) %>%
      ungroup()
  }
  
  years_available <- sort(unique(df_season$year_num))
  latest_year_num <- max(years_available)
  latest_year_str <- as.character(latest_year_num)
  
  # Fehlende Tage (Wochenenden, Feiertage) pro Jahr und Serie interpolieren
  df_season_interp <- df_season %>%
    group_by(label, year_num, year) %>%
    group_modify(~ interpolate_season_year(.x, is_latest_year = (.y$year_num == latest_year_num))) %>%
    ungroup()
  
  df_prior <- df_season_interp %>% filter(year_num < latest_year_num)
  df_latest <- df_season_interp %>% filter(year_num == latest_year_num)
  
  # Achsenparameter
  date_breaks <- if (!is.null(input$textInput_date_breaks) && input$textInput_date_breaks != "") {
    input$textInput_date_breaks
  } else {
    "1 month"
  }
  
  date_labels <- if (!is.null(input$textInput_date_labels) && input$textInput_date_labels != "") {
    input$textInput_date_labels
  } else {
    "%b"
  }
  
  y_breaks <- if (!is.null(input$textInput_manual_y_breaks) && input$textInput_manual_y_breaks != "") {
    str_split(input$textInput_manual_y_breaks, " |;|,")[[1]] %>% as.numeric()
  } else {
    waiver()
  }
  
  y_limits <- if (!is.null(input$textInput_y_limits) && input$textInput_y_limits != "") {
    str_split(input$textInput_y_limits, " |;|,")[[1]] %>% as.numeric() %>% c(Inf) %>% .[c(1, 2)]
  } else {
    NULL
  }
  
  season_variant <- if (!is.null(input$radioInput_season_type)) input$radioInput_season_type else "minmax"
  
  p <- ggplot()
  
  if (season_variant == "minmax" || grepl("Min-Max", season_variant, ignore.case = TRUE)) {
    # Variante 1: Grauer Min-Max-Bereich der Vorjahre + Linie für aktuelles Jahr
    if (nrow(df_prior) > 0) {
      df_minmax <- df_prior %>%
        group_by(dummy_date) %>%
        summarise(
          ymin = min(value, na.rm = TRUE),
          ymax = max(value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(dummy_date)
      
      prior_min_year <- min(df_prior$year_num)
      prior_max_year <- max(df_prior$year_num)
      ribbon_label <- if (prior_min_year == prior_max_year) {
        paste0("Spanne (", prior_min_year, ")")
      } else {
        paste0("Min-Max (", prior_min_year, "\u2013", prior_max_year, ")")
      }
      
      p <- p + geom_ribbon(
        data = df_minmax,
        aes(x = dummy_date, ymin = ymin, ymax = ymax, fill = ribbon_label),
        alpha = 0.35
      )
    }
    
    if (nrow(df_latest) > 0) {
      p <- p + geom_line(
        data = df_latest,
        aes(x = dummy_date, y = value, color = latest_year_str),
        linewidth = 1.2
      )
    }
    
    ribbon_label_name <- if (exists("ribbon_label")) ribbon_label else "Min-Max (Vorjahre)"
    p <- p +
      scale_fill_manual(
        name = NULL,
        values = setNames(c("#8E99A2"), ribbon_label_name)
      ) +
      scale_color_manual(
        name = NULL,
        values = setNames(c(main_color), latest_year_str)
      )
      
  } else {
    # Variante 2: Alle Jahre (Vorjahre hellgrau, aktuelles Jahr farbig)
    if (nrow(df_prior) > 0) {
      prior_min_year <- min(df_prior$year_num)
      prior_max_year <- max(df_prior$year_num)
      prior_label <- if (prior_min_year == prior_max_year) {
        as.character(prior_min_year)
      } else {
        paste0(prior_min_year, "\u2013", prior_max_year)
      }
      
      p <- p + geom_line(
        data = df_prior,
        aes(x = dummy_date, y = value, group = year, color = prior_label),
        linewidth = 0.75,
        alpha = 0.8
      )
      
      if (nrow(df_latest) > 0) {
        p <- p + geom_line(
          data = df_latest,
          aes(x = dummy_date, y = value, color = latest_year_str),
          linewidth = 1.3
        )
      }
      
      p <- p + scale_color_manual(
        name = NULL,
        values = setNames(c("#B0BEC5", main_color), c(prior_label, latest_year_str)),
        breaks = c(prior_label, latest_year_str)
      )
    } else {
      if (nrow(df_latest) > 0) {
        p <- p + geom_line(
          data = df_latest,
          aes(x = dummy_date, y = value, color = latest_year_str),
          linewidth = 1.3
        )
      }
      
      p <- p + scale_color_manual(
        name = NULL,
        values = setNames(c(main_color), latest_year_str)
      )
    }
  }
  
  # Letzten Punkt hervorheben
  if (isTRUE(input$checkboxInput_show_lastpoint) && nrow(df_latest) > 0) {
    df_last <- df_latest %>% filter(dummy_date == max(dummy_date, na.rm = TRUE))
    p <- p + geom_point(
      data = df_last,
      aes(x = dummy_date, y = value),
      fill = "red", color = "black", shape = 21, size = 2, inherit.aes = FALSE
    )
  }
  
  # Horizontale / Vertikale Hilfslinien
  if (!is.null(input$textInput_horizon_lines) && input$textInput_horizon_lines != "") {
    p <- p + geom_hline(yintercept = str_split(input$textInput_horizon_lines, " |;|,")[[1]] %>% as.numeric(), linetype = 2)
  }
  
  # Facets
  if (!is.null(input$selectInput_facet) && input$selectInput_facet != "none" && length(unique(df$label)) > 1) {
    p <- p + facet_wrap(~label, nrow = 1, scales = input$selectInput_facet)
  }
  
  # Formatting & Theme
  p <- p +
    theme_minimal(base_size = 14) +
    theme(
      plot.subtitle = element_text(size = rel(1), hjust = 0, margin = margin(0, 0, 1, 0, "lines")),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = scales::label_comma(
        big.mark = "'",
        if (as.character(input$textInput_y_nachkomma) != "Auto") accuracy = 10^(-as.numeric(input$textInput_y_nachkomma))
      )
    ) +
    coord_cartesian(ylim = y_limits) +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    guides(
      color = guide_legend(nrow = as.numeric(input$textInput_legendrows)),
      fill = guide_legend(nrow = as.numeric(input$textInput_legendrows))
    ) +
    labs(
      subtitle = paste0(" ", input$textInput_subtitle),
      caption = "Quelle: LSEG, Luzerner Kantonalbank"
    )
  
  if (!is.null(input$selectInput_legendposition)) {
    if (input$selectInput_legendposition == "topleft") {
      p <- p + theme(legend.position = c(0, 1), legend.justification = c(0, 1))
    } else {
      p <- p + theme(legend.position = input$selectInput_legendposition)
    }
  }
  
  if (!is.null(input$checkboxInput_show_legend) && !input$checkboxInput_show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

# Haupt-Renderingfunktion für alle Diagrammtypen
render_lukb_chart <- function(df, input, fill_colors = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Palette ermitteln falls nicht explizit übergeben
  if (is.null(fill_colors)) {
    palette_choice <- if (!is.null(input$selectInput_colors)) input$selectInput_colors else "pb"
    fill_colors <- get_chart_palette(palette_choice)
  }
  
  # Farbreihenfolge auflösen
  colors_ordered <- fill_colors[c(1, 3, 2, 4:length(fill_colors))]
  
  if (input$radioInput_chart_category == "Saisonalität") {
    return(render_season_chart(df, input, fill_colors))
  } else if (input$radioInput_chart_category == "Kategoriendiagramm") {
    
    date_end <- if (!is.null(input$textInput_date_end) && input$textInput_date_end != "") {
      as.Date(input$textInput_date_end, "%d.%m.%Y")
    } else {
      max(df$date)
    }
    
    df_cat <- df %>%
      filter(date <= date_end) %>%
      group_by(label) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      mutate(share = value / sum(value, na.rm = TRUE))
    
    if (input$radioInput_category_type == "Kuchendiagramm") {
      rotation_deg <- if (!is.null(input$sliderInput_pie_rotation)) input$sliderInput_pie_rotation else 0
      rotation_rad <- (rotation_deg * pi) / 180
      
      df_pie <- df_cat %>%
        mutate(
          end_angle = 2 * pi * cumsum(share) + rotation_rad,
          start_angle = dplyr::lag(end_angle, default = rotation_rad),
          mid_angle = (start_angle + end_angle) / 2,
          edge_x = cos(mid_angle),
          edge_y = sin(mid_angle),
          percent_str = paste0(label, "\n", scales::percent(share, accuracy = 0.1))
        )
      
      pie_polys <- create_pie_polygons(df_pie, radius = 1)
      
      df_right <- df_pie %>% filter(cos(mid_angle) >= 0)
      df_left <- df_pie %>% filter(cos(mid_angle) < 0)
      
      p <- ggplot() +
        geom_polygon(data = pie_polys, aes(x = x, y = y, fill = label), color = "white")
      
      if (nrow(df_right) > 0) {
        p <- p + ggrepel::geom_text_repel(
          data = df_right,
          aes(x = edge_x, y = edge_y, label = percent_str),
          nudge_x = 1.4 - df_right$edge_x,
          hjust = 0,
          direction = "y",
          xlim = c(1.2, 2.0),
          segment.color = "grey50",
          segment.size = 0.4,
          min.segment.length = 0,
          size = 3.5
        )
      }
      
      if (nrow(df_left) > 0) {
        p <- p + ggrepel::geom_text_repel(
          data = df_left,
          aes(x = edge_x, y = edge_y, label = percent_str),
          nudge_x = -1.4 - df_left$edge_x,
          hjust = 1,
          direction = "y",
          xlim = c(-2.0, -1.2),
          segment.color = "grey50",
          segment.size = 0.4,
          min.segment.length = 0,
          size = 3.5
        )
      }
      
      p <- p +
        coord_fixed(xlim = c(-2.2, 2.2), ylim = c(-1.2, 1.2), clip = "off") +
        theme_minimal(base_size = 14) +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.subtitle = element_text(size = rel(1), hjust = 0, margin = margin(0, 0, 1, 0, "lines"))
        ) +
        scale_fill_manual(values = colors_ordered) +
        guides(fill = guide_legend(nrow = as.numeric(input$textInput_legendrows))) +
        labs(subtitle = paste0(" ", input$textInput_subtitle), caption = "Quelle: LSEG, Luzerner Kantonalbank")
      
      if (input$selectInput_legendposition == "topleft") {
        p <- p + theme(legend.position = c(0, 1), legend.justification = c(0, 1))
      } else {
        p <- p + theme(legend.position = input$selectInput_legendposition)
      }
      if (!input$checkboxInput_show_legend) p <- p + theme(legend.position = "none")
      
      return(p)
      
    } else {
      # Balkendiagramm (Category Bar Chart)
      if (input$radioInput_bar_orientation == "Horizontal") {
        p <- ggplot(df_cat, aes(x = value, y = label, fill = label)) +
          geom_col(width = 0.7, show.legend = FALSE) +
          geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
          geom_text(aes(label = scales::comma(value, big.mark = "'"),
                        hjust = ifelse(value >= 0, -0.2, 1.2)),
                    size = 4) +
          theme_minimal(base_size = 14) +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_text(color = "black"),
            plot.subtitle = element_text(size = rel(1), hjust = 0, margin = margin(0, 0, 1, 0, "lines"))
          ) +
          scale_fill_manual(values = colors_ordered) +
          scale_x_continuous(expand = expansion(mult = c(0.1, 0.15)))
      } else {
        p <- ggplot(df_cat, aes(x = label, y = value, fill = label)) +
          geom_col(width = 0.7, show.legend = FALSE) +
          geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
          geom_text(aes(label = scales::comma(value, big.mark = "'"),
                        vjust = ifelse(value >= 0, -0.5, 1.5)),
                    size = 4) +
          theme_minimal(base_size = 14) +
          theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(color = "black"),
            plot.subtitle = element_text(size = rel(1), hjust = 0, margin = margin(0, 0, 1, 0, "lines"))
          ) +
          scale_fill_manual(values = colors_ordered) +
          scale_y_continuous(expand = expansion(mult = c(0.1, 0.15)))
      }
      
      p <- p +
        labs(subtitle = paste0(" ", input$textInput_subtitle), caption = "Quelle: LSEG, Luzerner Kantonalbank")
      
      return(p)
    }
    
  } else {
    # Zeitreihendiagramm
    date_start <- if (input$textInput_date_start == "") min(df$date) else as.Date(input$textInput_date_start, "%d.%m.%Y")
    date_end <- if (input$textInput_date_end == "") max(df$date) else as.Date(input$textInput_date_end, "%d.%m.%Y")
    date_breaks <- if (input$textInput_date_breaks == "") waiver() else input$textInput_date_breaks
    date_labels <- if (input$textInput_date_labels == "") waiver() else input$textInput_date_labels
    position_in <- if (input$checkboxInput_position_stack) position_stack() else position_dodge(width = 0.9)
    
    y_breaks <- if (input$textInput_manual_y_breaks != "") {
      str_split(input$textInput_manual_y_breaks, " |;|,")[[1]] %>% as.numeric()
    } else {
      waiver()
    }
    
    y_limits <- if (input$textInput_y_limits != "") {
      str_split(input$textInput_y_limits, " |;|,")[[1]] %>% as.numeric() %>% c(Inf) %>% .[c(1, 2)]
    } else {
      NULL
    }
    
    df_plot <- df %>% filter(between(date, date_start, date_end))
    
    if (!is.null(input$checkboxInput_index_100) && input$checkboxInput_index_100) {
      df_plot <- df_plot %>%
        group_by(label) %>%
        arrange(date) %>%
        mutate(
          first_val = first(value[!is.na(value)]),
          value = if_else(!is.na(first_val) & first_val != 0, (value / first_val) * 100, value)
        ) %>%
        ungroup()
    }
    
    p <- ggplot(df_plot, aes(x = date, y = value, group = label, color = label, fill = label)) +
      geom_point(data = . %>% filter(label == "###¨¨"))
    
    if (input$textInput_horizon_lines != "") {
      p <- p + geom_hline(yintercept = str_split(input$textInput_horizon_lines, " |;|,")[[1]] %>% as.numeric(), linetype = 2)
    }
    if (input$textInput_vertical_lines != "") {
      p <- p + geom_vline(xintercept = as.Date(str_split(input$textInput_vertical_lines, " |;|,")[[1]]), linetype = 2)
    }
    
    if ("Fläche" %in% input$checkboxGroupButtons_series_geom_type) {
      p <- p + geom_area(position = position_in, alpha = 0.5, show.legend = FALSE)
    }
    if ("Balken" %in% input$checkboxGroupButtons_series_geom_type) {
      p <- p + geom_col(position = position_in, alpha = 0.8, show.legend = FALSE)
    }
    if ("Stufen" %in% input$checkboxGroupButtons_series_geom_type) {
      p <- p + geom_step(linewidth = 1, position = position_in, show.legend = FALSE)
    }
    if ("Linie" %in% input$checkboxGroupButtons_series_geom_type || length(input$checkboxGroupButtons_series_geom_type) == 0) {
      p <- p + geom_line(linewidth = 1, position = position_in, show.legend = FALSE)
    }
    if ("Punkt" %in% input$checkboxGroupButtons_series_geom_type) {
      p <- p + geom_point(size = 1.5, position = position_in, show.legend = FALSE)
    }
    
    if (input$checkboxInput_show_lastpoint) {
      p <- p + geom_point(data = . %>% group_by(label) %>% filter(date == max(date)), fill = "red", color = "black", shape = 21, size = 2, position = position_in)
    }
    
    if (input$selectInput_facet != "none") {
      p <- p + facet_wrap(~label, nrow = 1, scales = input$selectInput_facet)
    }
    
    p <- p +
      theme_minimal(base_size = 14) +
      theme(
        plot.subtitle = element_text(size = rel(1), hjust = 0, margin = margin(0, 0, 1, 0, "lines")),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")
      ) +
      scale_color_manual(values = colors_ordered) +
      scale_fill_manual(values = colors_ordered) +
      scale_y_continuous(
        breaks = y_breaks,
        labels = scales::label_comma(
          big.mark = "'",
          if (as.character(input$textInput_y_nachkomma) != "Auto") accuracy = 10^(-as.numeric(input$textInput_y_nachkomma))
        )
      ) +
      coord_cartesian(ylim = y_limits) +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
      guides(
        col = guide_legend(nrow = as.numeric(input$textInput_legendrows)),
        fill = guide_legend(nrow = as.numeric(input$textInput_legendrows))
      ) +
      labs(subtitle = paste0(" ", input$textInput_subtitle), caption = "Quelle: LSEG, Luzerner Kantonalbank")
    
    if (input$selectInput_legendposition == "topleft") {
      p <- p + theme(legend.position = c(0, 1), legend.justification = c(0, 1))
    } else {
      p <- p + theme(legend.position = input$selectInput_legendposition)
    }
    if (!input$checkboxInput_show_legend) p <- p + theme(legend.position = "none")
    
    return(p)
  }
}
