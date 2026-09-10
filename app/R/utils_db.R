# utils_db.R
# Datenbankverbindungen, CRUD-Operationen für Chart-Vorlagen und Abfragen

# Escapes single quotes for SQL string literals
escape_sql_string <- function(s) {
  if (is.null(s) || length(s) == 0 || anyNA(s)) return("NULL")
  s_escaped <- gsub("'", "''", as.character(s[1]))
  paste0("'", s_escaped, "'")
}

# Safely parses a JSON string, fallback to empty list on error or empty input
safe_from_json <- function(txt, fallback = list()) {
  if (is.null(txt) || length(txt) == 0 || anyNA(txt)) {
    return(fallback)
  }
  txt_str <- as.character(txt[1])
  if (txt_str == "" || txt_str == "NA" || txt_str == "NULL") {
    return(fallback)
  }
  tryCatch({
    fromJSON(txt_str)
  }, error = function(e) {
    warning("JSON-Parsing fehlgeschlagen: ", e$message)
    fallback
  })
}

init_db <- function() {
  if (db_mode == "SQLite") {
    conn <- dbConnect(SQLite(), db_path)
    dbExecute(conn, "CREATE TABLE IF NOT EXISTS saved_charts (
      chart_name TEXT PRIMARY KEY,
      inputs_json TEXT,
      data_json TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")
    dbDisconnect(conn)
  } else {
    conn <- lukb_con()
    table_name <- "temp_michartdb"
    if (!dbExistsTable(conn, table_name)) {
      tryCatch({
        dbExecute(conn, paste0("CREATE TABLE ", table_name, " (
          chart_name NVARCHAR(255) PRIMARY KEY,
          inputs_json NVARCHAR(MAX),
          data_json NVARCHAR(MAX),
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )"))
      }, error = function(e) {
        warning("Konnte Tabelle ", table_name, " in MSSQL nicht erstellen: ", e$message)
      })
    } else {
      # Proaktiv existierende Spalten auf NVARCHAR(MAX) umstellen
      tryCatch({
        dbExecute(conn, paste0("ALTER TABLE ", table_name, " ALTER COLUMN chart_name NVARCHAR(255) NOT NULL"))
        dbExecute(conn, paste0("ALTER TABLE ", table_name, " ALTER COLUMN inputs_json NVARCHAR(MAX)"))
        dbExecute(conn, paste0("ALTER TABLE ", table_name, " ALTER COLUMN data_json NVARCHAR(MAX)"))
      }, error = function(e) {
        # Falls Alter Table fehlschlägt, leise ignorieren
      })
    }
    dbDisconnect(conn)
  }
}

save_chart_db <- function(name, inputs_list, data_list) {
  exclude_keys <- c(
    "btn_save_chart", "btn_delete_chart", "btn_file1", "btn_inputs", 
    "btn_getinputs", "btn_pulldb", "select_saved_chart", "btn_downloadPlot",
    "btn_edit_data", "hot_edit_table", "btn_apply_edit_data", "btn_add_row_hot",
    "btn_reset_edited_data"
  )
  clean_list <- inputs_list[!names(inputs_list) %in% exclude_keys]
  inputs_json <- toJSON(clean_list, auto_unbox = TRUE)
  data_json <- toJSON(data_list)
  
  if (db_mode == "SQLite") {
    conn <- dbConnect(SQLite(), db_path)
    dbExecute(conn, 
              "INSERT OR REPLACE INTO saved_charts (chart_name, inputs_json, data_json) VALUES (?, ?, ?)",
              params = list(name, inputs_json, data_json))
    dbDisconnect(conn)
  } else {
    conn <- lukb_con()
    table_name <- "temp_michartdb"
    tryCatch({
      esc_name <- escape_sql_string(enc2utf8(name))
      esc_inputs <- escape_sql_string(enc2utf8(inputs_json))
      esc_data <- escape_sql_string(enc2utf8(data_json))
      
      dbExecute(conn, paste0("DELETE FROM ", table_name, " WHERE chart_name = ", esc_name))
      
      sql_insert <- paste0(
        "INSERT INTO ", table_name, " (chart_name, inputs_json, data_json) ",
        "VALUES (", esc_name, ", ", esc_inputs, ", ", esc_data, ")"
      )
      dbExecute(conn, sql_insert)
    }, error = function(e) {
      stop("Fehler beim Speichern in MSSQL: ", e$message)
    })
    dbDisconnect(conn)
  }
}

get_saved_charts_df <- function() {
  if (db_mode == "SQLite") {
    conn <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(conn))
    if (!dbExistsTable(conn, "saved_charts")) {
      return(data.frame(Name = character(0), Erstellungsdatum = character(0), stringsAsFactors = FALSE))
    }
    res <- dbGetQuery(conn, "SELECT chart_name as Name, datetime(created_at, 'localtime') as Erstellungsdatum FROM saved_charts ORDER BY Name")
    res
  } else {
    conn <- lukb_con()
    on.exit(dbDisconnect(conn))
    table_name <- "temp_michartdb"
    if (!dbExistsTable(conn, table_name)) {
      return(data.frame(Name = character(0), Erstellungsdatum = character(0), stringsAsFactors = FALSE))
    }
    res <- dbGetQuery(conn, paste0("SELECT chart_name as Name, created_at as Erstellungsdatum FROM ", table_name, " ORDER BY Name"))
    if (nrow(res) > 0 && "Erstellungsdatum" %in% names(res)) {
      res$Erstellungsdatum <- as.character(res$Erstellungsdatum)
    }
    res
  }
}

load_chart_db <- function(name) {
  if (db_mode == "SQLite") {
    conn <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(conn))
    res <- dbGetQuery(conn, "SELECT inputs_json, data_json FROM saved_charts WHERE chart_name = ?", params = list(name))
    if (nrow(res) == 0) return(NULL)
    list(
      inputs = safe_from_json(res$inputs_json[1]),
      data = safe_from_json(res$data_json[1])
    )
  } else {
    conn <- lukb_con()
    on.exit(dbDisconnect(conn))
    table_name <- "temp_michartdb"
    esc_name <- escape_sql_string(enc2utf8(name))
    res <- dbGetQuery(conn, paste0("SELECT inputs_json, data_json FROM ", table_name, " WHERE chart_name = ", esc_name))
    if (nrow(res) == 0) return(NULL)
    list(
      inputs = safe_from_json(res$inputs_json[1]),
      data = safe_from_json(res$data_json[1])
    )
  }
}

delete_chart_db <- function(name) {
  if (db_mode == "SQLite") {
    conn <- dbConnect(SQLite(), db_path)
    dbExecute(conn, "DELETE FROM saved_charts WHERE chart_name = ?", params = list(name))
    dbDisconnect(conn)
  } else {
    conn <- lukb_con()
    table_name <- "temp_michartdb"
    esc_name <- escape_sql_string(enc2utf8(name))
    dbExecute(conn, paste0("DELETE FROM ", table_name, " WHERE chart_name = ", esc_name))
    dbDisconnect(conn)
  }
}

get_db_data <- function(ticker_in) {
  ticker_in <- toupper(stringr::str_squish(ticker_in))
  ticker_in <- ticker_in[ticker_in != ""]
  if (length(ticker_in) == 0) {
    return(tibble(ticker = character(0), date = as.Date(character(0)), value = numeric(0)))
  }
  
  dplyr::tbl(lukb_con(), "temp_db")  %>%
    dplyr::filter(ticker %in% ticker_in) %>%
    dplyr::arrange(ticker, date) %>%
    dplyr::collect()  %>%
    dplyr::mutate(
      ticker = toupper(stringr::str_squish(ticker)),
      date = as.Date(date, origin = "1970-01-01")
    )
}
