# utils_transform.R
# Zeitreihen-Mathematik, Formelvalidierung und Daten-Transformation

# Mathematische Zeitreihenoperatoren
ACH <- function(x, k = 1) {
  if (is.null(k) || is.na(k) || k < 1) k <- 1
  x - dplyr::lag(x, k)
}

PCH <- function(x, k = 1) {
  if (is.null(k) || is.na(k) || k < 1) k <- 1
  l <- dplyr::lag(x, k)
  (x - l) / l * 100
}

ABS <- function(x) {
  abs(x)
}

MAV <- function(x, k = 5) {
  if (is.null(k) || is.na(k) || k <= 0) return(x)
  if (length(x) < k) return(rep(NA_real_, length(x)))
  as.numeric(stats::filter(x, rep(1/k, k), sides = 1))
}

LAG <- function(x, k = 1) {
  if (is.null(k) || is.na(k) || k < 1) k <- 1
  dplyr::lag(x, k)
}

LOG <- function(x) {
  log(x)
}

# Sicherheitsvalidator für benutzerdefinierte mathematische Formeln
is_safe_formula <- function(formula_str, allowed_vars = c("X")) {
  if (is.null(formula_str) || length(formula_str) == 0 || is.na(formula_str)) return(FALSE)
  f <- stringr::str_squish(formula_str)
  if (f == "") return(FALSE)
  
  # Erlaubte Funktionsnamen und Variablen
  allowed_words <- c("ACH", "PCH", "ABS", "MAV", "LAG", "LOG", allowed_vars)
  
  for (word in allowed_words) {
    f <- gsub(paste0("\\b", word, "\\b"), " ", f, ignore.case = TRUE)
  }
  
  # Erlaube Zahlen und grundlegende Rechenzeichen
  f <- gsub("[0-9.]+", " ", f)
  f <- gsub("[+*/^(),-]", " ", f)
  
  # Gültig, wenn nur noch Leerzeichen übrig sind
  return(stringr::str_squish(f) == "")
}

# Führt Reshaping, Formelauswertungen und Filterung für alle Zeitreihen aus
transform_chart_data <- function(df_raw, original_formula_map, custom_series_list, selected_series) {
  if (is.null(df_raw) || nrow(df_raw) == 0 || !("value" %in% names(df_raw))) {
    return(NULL)
  }
  
  df_clean <- df_raw %>% filter(!is.na(value))
  if (nrow(df_clean) == 0) return(NULL)
  
  # 1. Labels mappen auf A, B, C...
  raw_labels <- unique(df_clean$label)
  raw_labels <- na.omit(raw_labels)
  if (length(raw_labels) == 0) return(NULL)
  
  label_map <- setNames(LETTERS[1:length(raw_labels)], raw_labels)
  
  # 2. Wide format zur zeitlichen Ausrichtung
  df_wide <- df_clean %>%
    select(date, label, value) %>%
    pivot_wider(names_from = label, values_from = value) %>%
    arrange(date)
  
  # Spalten nach A, B, C umbenennen
  names(df_wide) <- c("date", label_map[names(df_wide)[-1]])
  
  # Fehlende Werte auffüllen für Formelberechnungen (down-up)
  df_wide_filled <- df_wide %>%
    fill(-date, .direction = "downup")
  
  # 3. Auswertungsumgebung vorbereiten
  eval_env_filled <- list(
    ACH = ACH,
    PCH = PCH,
    ABS = ABS,
    MAV = MAV,
    LAG = LAG,
    LOG = LOG
  )
  for (col in names(df_wide_filled)) {
    eval_env_filled[[col]] <- df_wide_filled[[col]]
  }
  
  # 4. Formeln auswerten
  df_results <- tibble(date = df_wide$date)
  
  # Original-Serien
  for (lbl in names(label_map)) {
    letter <- label_map[[lbl]]
    formula_str <- original_formula_map[[letter]]
    if (is.null(formula_str) || stringr::str_squish(formula_str) == "") {
      formula_str <- letter
    }
    formula_upper <- toupper(formula_str)
    
    if (formula_upper == letter || formula_upper == "X") {
      df_results[[lbl]] <- df_wide[[letter]]
    } else {
      allowed_vars <- unique(c("X", as.character(label_map)))
      if (is_safe_formula(formula_upper, allowed_vars = allowed_vars)) {
        local_env <- eval_env_filled
        local_env$X <- df_wide_filled[[letter]]
        
        res_val <- tryCatch({
          eval(parse(text = formula_upper), envir = local_env)
        }, error = function(e) {
          rep(NA_real_, nrow(df_wide))
        })
        df_results[[lbl]] <- res_val
      } else {
        df_results[[lbl]] <- rep(NA_real_, nrow(df_wide))
      }
    }
  }
  
  # Custom-Serien
  if (!is.null(custom_series_list) && length(custom_series_list) > 0) {
    for (name in names(custom_series_list)) {
      formula_str <- custom_series_list[[name]]$formula
      formula_upper <- toupper(formula_str)
      if (is_safe_formula(formula_upper, allowed_vars = as.character(label_map))) {
        res_val <- tryCatch({
          eval(parse(text = formula_upper), envir = eval_env_filled)
        }, error = function(e) {
          rep(NA_real_, nrow(df_wide))
        })
        df_results[[name]] <- res_val
      } else {
        df_results[[name]] <- rep(NA_real_, nrow(df_wide))
      }
    }
  }
  
  # 5. Long format & Filterung
  df_long <- df_results %>%
    pivot_longer(cols = -date, names_to = "label", values_to = "value") %>%
    filter(!is.na(value))
  
  if (!is.null(selected_series) && length(selected_series) > 0) {
    df_long <- df_long %>%
      filter(label %in% selected_series) %>%
      mutate(label = factor(label, levels = selected_series))
  }
  
  df_long
}
