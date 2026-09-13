# utils_transform.R
# Zeitreihen-Transformationen (Rohwert, Relative Veränderung, Absolute Veränderung)

#' Hilfsfunktion zur kalendarischen Lag-Ermittlung
#' Erkennt monatliche Zeitreihen (höchstens ein Wert pro Kalendermonat) und führt einen
#' kalendarischen Monats-Lag durch, damit fehlende Monate in den Rohdaten nicht zu
#' Versetzungen im Zeitraster führen. Für andere Frequenzen (z.B. Tagesdaten) wird auf einen Positions-Lag zurückgegriffen.
calc_series_lag <- function(dates, values, lag_val) {
  if (length(dates) == 0 || length(values) == 0) return(numeric(0))
  
  d_vec <- as.Date(dates)
  ym_vec <- format(d_vec, "%Y-%m")
  
  # Monatliche Frequenz prüfen (keine doppelten Monate in derselben Serie)
  is_monthly <- !anyDuplicated(ym_vec)
  
  if (is_monthly) {
    df_lookup <- tibble(
      ym = ym_vec,
      val = values
    )
    
    cur_y <- as.integer(format(d_vec, "%Y"))
    cur_m <- as.integer(format(d_vec, "%m"))
    
    target_idx <- (cur_y * 12 + cur_m) - as.integer(lag_val)
    target_y <- (target_idx - 1) %/% 12
    target_m <- ((target_idx - 1) %% 12) + 1
    target_ym <- sprintf("%04d-%02d", target_y, target_m)
    
    match_idx <- match(target_ym, df_lookup$ym)
    return(df_lookup$val[match_idx])
  } else {
    return(dplyr::lag(values, as.integer(lag_val)))
  }
}

#' Führt Zeitreihen-Transformationen (Rohwert, %-Veränderung, Abs-Veränderung) durch
#' @param df_raw Data frame mit den Rohdaten (Spalten: date, label, value)
#' @param series_transformations Liste oder reactiveValues mit Transformationseinstellungen pro Label
#' @param selected_series Vektor der aktuell ausgewählten Serien
#' @return Transformierter Data frame im Long-Format
transform_chart_data <- function(df_raw, series_transformations, selected_series = NULL) {
  if (is.null(df_raw) || nrow(df_raw) == 0 || !("value" %in% names(df_raw))) {
    return(NULL)
  }
  
  df_clean <- df_raw %>% filter(!is.na(value))
  if (nrow(df_clean) == 0) return(NULL)
  
  labels_present <- unique(df_clean$label)
  labels_present <- na.omit(labels_present)
  if (length(labels_present) == 0) return(NULL)
  
  # Transformationen pro Zeitreihe anwenden
  df_list <- lapply(labels_present, function(lbl) {
    sub_df <- df_clean %>% 
      filter(label == lbl) %>% 
      arrange(date)
    
    trans <- series_transformations[[lbl]]
    
    if (!is.null(trans) && !is.null(trans$type) && trans$type != "Rohwert") {
      if (trans$type == "pct_change" || trans$type == "abs_change") {
        lag_val <- trans$lag
        if (is.null(lag_val) || is.na(lag_val) || lag_val < 1) lag_val <- 1
        lag_val <- as.numeric(lag_val)
        
        l <- calc_series_lag(sub_df$date, sub_df$value, lag_val)
        
        if (trans$type == "pct_change") {
          sub_df$value <- (sub_df$value - l) / l * 100
        } else {
          sub_df$value <- sub_df$value - l
        }
      } else if (trans$type == "add_constant") {
        offset <- trans$offset
        if (is.null(offset) || is.na(offset)) offset <- 0
        sub_df$value <- sub_df$value + as.numeric(offset)
      }
    }
    
    sub_df
  })
  
  df_trans <- bind_rows(df_list) %>%
    filter(!is.na(value))
  
  # Reihenfolge und Filterung gemäss Serienauswahl
  if (!is.null(selected_series) && length(selected_series) > 0) {
    df_trans <- df_trans %>%
      filter(label %in% selected_series) %>%
      mutate(label = factor(label, levels = selected_series))
  }
  
  df_trans
}

