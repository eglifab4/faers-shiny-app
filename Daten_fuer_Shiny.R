# =============================================================
# Daten_fuer_Shiny.R
# -------------------------------------------------------------
# Bereitet die Daten fuer die Shiny App vor.
# Liest pro Entitaet (DEMO, DRUG, INDI, OUTC, REAC, RPSR, THER)
# die letzten 2 Jahre aus den entpackten .txt-Dateien ein,
# saeubert die Spaltennamen und speichert pro Entitaet eine
# kombinierte Tabelle als .rds in app_data/.
#
# Die App laedt dann beim Start nur diese 7 Dateien (schnell).
#
# Voraussetzung:
#   - Daten herunterladen.R wurde ausgefuehrt
#   - In Organisation_processed_data/<QUARTAL>/ liegen
#     DEMO.txt, DRUG.txt, INDI.txt, OUTC.txt, REAC.txt,
#     RPSR.txt, THER.txt
# =============================================================

library(data.table)
library(here)

find_faers_file <- function(q_folder, entity) {

  pattern <- paste0("^", entity, ".*\\.txt$")

  files <- list.files(
    q_folder,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) return(NULL)

  ascii_files <- files[grepl("ascii", files, ignore.case = TRUE)]

  if (length(ascii_files) > 0) return(ascii_files[1])

  files[1]
}


# -------------------------------------------------------------
# 1) Pfade und Konfiguration
# -------------------------------------------------------------
raw_ordner <- here("Organisation_Rohdaten", "faers_data")
app_data_ordner  <- here("app_data")

dir.create(app_data_ordner, recursive = TRUE, showWarnings = FALSE)

# Wie viele Quartale (8 = letzte 2 Jahre)
N_QUARTALE <- 8

# Welche Entitaeten verarbeiten
entitaeten <- c("DEMO", "DRUG", "INDI", "OUTC", "REAC", "RPSR", "THER")

# -------------------------------------------------------------
# 2) Welche Quartale verarbeiten?
# -------------------------------------------------------------
alle_q_ordner <- list.dirs(raw_ordner,
                           recursive = FALSE,
                           full.names = TRUE)

# Nach Name sortieren (alphabetisch = chronologisch, weil "2024Q2" < "2024Q3")
alle_q_ordner <- alle_q_ordner[order(basename(alle_q_ordner))]

# Letzte N_QUARTALE nehmen
letzte_q_ordner <- tail(alle_q_ordner, N_QUARTALE)

message("Verarbeite ", length(letzte_q_ordner), " Quartale:")
message("  Erstes: ", basename(letzte_q_ordner[1]))
message("  Letztes: ", basename(letzte_q_ordner[length(letzte_q_ordner)]))

# -------------------------------------------------------------
# 3) Einlese-Funktion (mit fread + Spaltennamen-Saeuberung)
# -------------------------------------------------------------
read_entity_quartal <- function(entity, q_ordner) {

  q_name <- basename(q_ordner)

  file_pfad <- find_faers_file(q_ordner, entity)

  if (is.null(file_pfad) || !file.exists(file_pfad)) {
    return(NULL)
  }

  dt <- tryCatch({
    fread(
      file_pfad,
      sep = "$",
      header = TRUE,
      fill = TRUE,
      quote = "",
      na.strings = c("", "NA", "."),
      colClasses = "character",
      encoding = "Latin-1",
      showProgress = FALSE
    )
  }, error = function(e) {
    message("FEHLER: ", entity, " in ", q_name)
    return(NULL)
  })

  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  setnames(dt, names(dt),
           tolower(sub("^[^A-Za-z0-9_]+", "", names(dt))))

  dt[, quartal := q_name]
  dt[, jahr := substr(q_name, 1, 4)]

  dt
}

# -------------------------------------------------------------
# 4) Pro Entitaet: alle Quartale einlesen + zusammenfuegen
# -------------------------------------------------------------
read_entity_alle <- function(entity) {
  message("\n>>> ", entity)

  # Pro Quartal einlesen
  alle_dt <- vector("list", length(letzte_q_ordner))
  for (i in seq_along(letzte_q_ordner)) {
    q_ordner <- letzte_q_ordner[i]
    q_name <- basename(q_ordner)
    message("  [", i, "/", length(letzte_q_ordner), "] ", q_name)
    alle_dt[[i]] <- read_entity_quartal(entity, q_ordner)
  }

  # NULL-Eintraege rausfiltern (z.B. wenn .txt fehlt)
  alle_dt <- Filter(Negate(is.null), alle_dt)

  if (length(alle_dt) == 0) {
    message("  -> Keine Daten gefunden!")
    return(NULL)
  }

  # Zusammenfuegen (fill = TRUE, falls Spalten unterschiedlich)
  combined <- rbindlist(alle_dt, fill = TRUE, use.names = TRUE)

  message("  -> ", format(nrow(combined), big.mark = "'"),
          " Zeilen, ", ncol(combined), " Spalten")

  combined
}

# -------------------------------------------------------------
# 5) Schleife ueber alle Entitaeten
# -------------------------------------------------------------
for (entity in entitaeten) {

  dt <- read_entity_alle(entity)

  if (is.null(dt)) next

  out_pfad <- file.path(app_data_ordner, paste0(entity, ".rds"))
  saveRDS(dt, out_pfad)

  message("  -> Gespeichert: ", basename(out_pfad),
          " (", round(file.info(out_pfad)$size / 1024^2, 1), " MB)")

  # Speicher freigeben fuer naechste Entitaet
  rm(dt)
  gc(verbose = FALSE)
}

# -------------------------------------------------------------
# 6) Zusammenfassung
# -------------------------------------------------------------
message("\n========== FERTIG ==========")
message("App-Daten gespeichert in: ", app_data_ordner)

dateien <- list.files(app_data_ordner, pattern = "\\.rds$", full.names = TRUE)
gesamt_mb <- round(sum(file.info(dateien)$size) / 1024^2, 1)

message("Anzahl Dateien:  ", length(dateien))
message("Gesamtgroesse:   ", gesamt_mb, " MB")
message("\nDateien:")
for (f in dateien) {
  groesse <- round(file.info(f)$size / 1024^2, 1)
  message("  ", sprintf("%-12s", basename(f)), groesse, " MB")
}
