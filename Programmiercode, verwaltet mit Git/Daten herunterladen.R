# FAERS-Daten Downloader

# Dieses Skript:
# 1. Liest die FDA-FAERS-Webseite ein
# 2. Extrahiert alle verfügbaren ZIP-Dateien
# 3. Filtert nur ASCII-FAERS-Dateien ab 2012
# 4. Lädt fehlende Quartale herunter
# 5. Entpackt die Daten automatisch

# Datenquelle:
# https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html

# Benötigte Pakete:
# - rvest   -> Webscraping
# - stringr -> String-Verarbeitung


library(rvest)
library(stringr)

# URL der offiziellen FDA-FAERS-Seite
url <- "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html"

# HTML-Seite laden
page <- read_html(url)

# Alle href-Links aus der Webseite extrahieren
links <- page |>
  html_elements("a") |>
  html_attr("href")


# NA-Werte entfernen
links <- links[!is.na(links)]

# Nur ASCII-FAERS-ZIP-Dateien auswählen
zip_links <- links[
  str_detect(
    links,
    regex("faers_ascii_.*\\.zip$", ignore_case = TRUE)
  )
]

# Funktion:
# Extrahiert das Jahr aus dem Dateinamen

extract_year <- function(x) {
  as.integer(str_extract(x, "20[0-9]{2}"))
}

# Nur ab 2012 (Aufgabenstellung Praktikum 3, Woche 1)
# Ab Q3 2014 hat sich die Datenstruktur geaendert -
# wird in Daten_fuer_Shiny.R durch fill = TRUE im rbindlist abgefangen
zip_links <- zip_links[
  !is.na(extract_year(zip_links)) &
    extract_year(zip_links) >= 2012
]

zip_links <- unique(zip_links)

# Zielordner für Downloads und entpackte Daten
base_dir <- "Organisation_Rohdaten/faers_data"


# Ordner erstellen, falls nicht vorhanden
dir.create(
  base_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Download-Timeout erhöhen
# Große ZIP-Dateien können mehrere Minuten dauern.
options(timeout = 600)

# Extrahiert die Quartals-ID aus dem Dateinamen

extract_id <- function(x) {
  str_extract(tolower(x), "20[0-9]{2}q[1-4]")
}


existing_ids <- extract_id(
  list.files(base_dir)
)


# Download und Entpacken aller fehlenden Dateien
for (link in zip_links) {
  
  # vollständige URL
  if (!str_detect(link, "^http")) {
    file_url <- paste0("https://fis.fda.gov", link)
  } else {
    file_url <- link
  }
  
  file_name <- basename(file_url)
  
  id <- extract_id(file_name)
  
  # Überspringen, falls Quartal bereits existiert
  if (!is.na(id) && id %in% existing_ids) {
    message("Bereits vorhanden: ", id)
    next
  }

  # Lokaler Speicherpfad der ZIP-Datei
  zip_path <- file.path(base_dir, file_name)
  
  # Zielordner zum Entpacken
  out_dir <- file.path(
    base_dir,
    str_replace(file_name, "\\.zip$", "")
  )
  
  message("⬇ Download: ", file_name)
  
  # Falls Download oder Entpacken fehlschlägt,
  # werden unvollständige Dateien gelöscht.
  tryCatch({
    
    # ZIP-Datei herunterladen
    # mode = "wb":
    # notwendig für Binärdateien wie ZIP
    # method = "libcurl":
    # stabilerer Download
    
    download.file(
      file_url,
      destfile = zip_path,
      mode = "wb",
      method = "libcurl"
    )
    
    # Zielordner erstellen
    dir.create(out_dir, showWarnings = FALSE)
    
    # ZIP-Datei entpacken
    unzip(zip_path, exdir = out_dir)
    
  }, error = function(e) {
    
    # Fehlermeldung ausgeben
    message("Fehler bei: ", file_name)
    
    # Beschädigte ZIP-Datei löschen
    if (file.exists(zip_path)) file.remove(zip_path)
    # Unvollständigen Ordner löschen
    if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
  })
}