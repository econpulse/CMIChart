# 00_config.R
# Globale Einstellungen, Bibliotheken, Farben und DB-Konfiguration

Sys.setlocale("LC_ALL", "de_CH.utf8")

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(readxl)
library(bslib)
library(showtext)
library(RSQLite)
library(jsonlite)
library(DT)
library(rhandsontable)
library(shinyjs)

# Datenbankeinstellungen
db_mode <- "SQLite" # "SQLite" oder "MSSQL"
db_path <- "charts.db"

lukb_con <- function() {
  DBI::dbConnect(odbc::odbc(), "myDB")
}

# LUKB Farbpalette
if (!exists("lukb_colors")) {
  lukb_colors <- c("#005A36", "#A3D9C9", "#009F4D", "#002B19", "#4D9276", "#1A8057")
}

# Liefert die gewünschte Farbpalette basierend auf der Benutzerauswahl
get_chart_palette <- function(palette_key = "cd") {
  if (is.null(palette_key) || palette_key == "" || palette_key == "cd") {
    # LUKB Corporate (Standard, Platzhalter: umgekehrte LUKB-Farbpalette)
    return(rev(lukb_colors))
  } else if (palette_key == "pb") {
    # LUKB Private
    return(lukb_colors)
  } else {
    # Fallback
    return(rev(lukb_colors))
  }
}


# Hilfsfunktion zur Ermittlung dunkler Farben
is_color_dark <- function(color) {
  if (is.null(color) || is.na(color) || color == "") return(FALSE)
  tryCatch({
    rgb_val <- col2rgb(color)
    lum <- 0.299 * rgb_val[1, 1] + 0.587 * rgb_val[2, 1] + 0.114 * rgb_val[3, 1]
    return(lum < 128)
  }, error = function(e) {
    return(FALSE)
  })
}

# Hilfsfunktion zur Bereinigung von Label-IDs
sanitize_id <- function(label) {
  paste0("series_", gsub("[^a-zA-Z0-9_]", "_", label))
}

# Hilfsfunktion für Inline-Textinputs
textInputRow <- function(inputId, label, value = "") {
  div(
    style = "display:inline-block", class = "form-group shiny-input-container1",
    tags$label(label, `for` = inputId, class = "control-label"),
    tags$input(id = inputId, type = "text", value = value, class = "form-control shiny-bound-input")
  )
}

