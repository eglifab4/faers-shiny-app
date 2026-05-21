# Daten_fuer_Shiny.R
# Bereitet die FAERS-Daten fuer die Shiny-App vor und speichert sie als .fst.

library(data.table)
library(here)
library(fst)

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


# Pfade und Konfiguration
raw_ordner <- here("Organisation_Rohdaten", "faers_data")
app_data_ordner  <- here("app_data")

dir.create(app_data_ordner, recursive = TRUE, showWarnings = FALSE)

# Wie viele Quartale (8 = letzte 2 Jahre)
N_QUARTALE <- 8

# Welche Entitaeten verarbeiten
entitaeten <- c("DEMO", "DRUG", "INDI", "OUTC", "REAC", "RPSR", "THER")


# Quartal-Ordner sortieren und die letzten N_QUARTALE nehmen
alle_q_ordner <- list.dirs(raw_ordner,
                           recursive = FALSE,
                           full.names = TRUE)

alle_q_ordner <- alle_q_ordner[order(basename(alle_q_ordner))]
letzte_q_ordner <- tail(alle_q_ordner, N_QUARTALE)

message("Verarbeite ", length(letzte_q_ordner), " Quartale:")
message("  Erstes: ", basename(letzte_q_ordner[1]))
message("  Letztes: ", basename(letzte_q_ordner[length(letzte_q_ordner)]))


# Einlese-Funktion: ein Quartal, eine Entitaet
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

  # Spaltennamen lowercase, BOM-Reste entfernen
  setnames(dt, names(dt),
           tolower(sub("^[^A-Za-z0-9_]+", "", names(dt))))

  dt[, quartal := q_name]
  dt[, jahr := substr(q_name, 1, 4)]

  dt
}


# Pro Entitaet alle Quartale einlesen und zusammenfuegen
read_entity_alle <- function(entity) {
  message("\n>>> ", entity)

  alle_dt <- vector("list", length(letzte_q_ordner))
  for (i in seq_along(letzte_q_ordner)) {
    q_ordner <- letzte_q_ordner[i]
    q_name <- basename(q_ordner)
    message("  [", i, "/", length(letzte_q_ordner), "] ", q_name)
    alle_dt[[i]] <- read_entity_quartal(entity, q_ordner)
  }

  alle_dt <- Filter(Negate(is.null), alle_dt)

  if (length(alle_dt) == 0) {
    message("  -> Keine Daten gefunden!")
    return(NULL)
  }

  # fill = TRUE faengt unterschiedliche Spalten zwischen Quartalen ab
  combined <- rbindlist(alle_dt, fill = TRUE, use.names = TRUE)

  message("  -> ", format(nrow(combined), big.mark = "'"),
          " Zeilen, ", ncol(combined), " Spalten")

  combined
}


# Schleife ueber alle Entitaeten
for (entity in entitaeten) {

  dt <- read_entity_alle(entity)

  if (is.null(dt)) next

  out_pfad <- file.path(app_data_ordner, paste0(entity, ".fst"))
  write_fst(dt, out_pfad, compress = 50)

  message("  -> Gespeichert: ", basename(out_pfad),
          " (", round(file.info(out_pfad)$size / 1024^2, 1), " MB)")

  rm(dt)
  gc(verbose = FALSE)
}


# Zusammenfassung
message("\nFertig.")
message("App-Daten gespeichert in: ", app_data_ordner)

dateien <- list.files(app_data_ordner, pattern = "\\.fst$", full.names = TRUE)
gesamt_mb <- round(sum(file.info(dateien)$size) / 1024^2, 1)

message("Anzahl Dateien:  ", length(dateien))
message("Gesamtgroesse:   ", gesamt_mb, " MB")
message("\nDateien:")
for (f in dateien) {
  groesse <- round(file.info(f)$size / 1024^2, 1)
  message("  ", sprintf("%-12s", basename(f)), groesse, " MB")
}
