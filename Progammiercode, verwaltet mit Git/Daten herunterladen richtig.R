#Paket

library(stringr)

options(timeout = 600,                      
        download.file.method = "libcurl")

project_root <- "C:/Users/StartKlar/OneDrive - ZHAW/Dokumente/ZHAW/Datenprozessierung mit R/Praktikum 3"
raw_ordner   <- file.path(project_root, "data", "raw")
processed_ordner <- file.path(project_root, "data", "processed")


dir.create(raw_ordner, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_ordner, showWarnings = FALSE, recursive = TRUE)


heute_jahr    <- as.integer(format(Sys.Date(), "%Y"))
heute_quartal <- ceiling(as.integer(format(Sys.Date(), "%m")) / 3)

quartale <- expand.grid(jahr = 2012:heute_jahr, quartal = 1:4)
quartale <- quartale[order(quartale$jahr, quartale$quartal), ]

#Ewiges wieterlaufen verhinndern
quartale <- quartale[quartale$jahr < heute_jahr |
                       (quartale$jahr == heute_jahr & quartale$quartal <= heute_quartal), ]

message("Erwartete Quartale: ", nrow(quartale))

#Herunterladen
base_url <- "https://fis.fda.gov/content/Exports/"

for (i in seq_len(nrow(quartale))) {
  jahr <- quartale$jahr[i]
  q    <- quartale$quartal[i]
  geladen <- FALSE
  
  # Beide Schreibweisen (q und Q) probieren, weil sie nicht immer gleich sind
  for (schreibweise in c("Q", "q")) {
    fname <- sprintf("faers_ascii_%d%s%d.zip", jahr, schreibweise, q)
    url   <- paste0(base_url, fname)
    ziel  <- file.path(raw_ordner, fname)
    
    # falls schon vorhanden
    alt_fname <- sprintf("faers_ascii_%d%s%d.zip", jahr,
                         ifelse(schreibweise == "Q", "q", "Q"), q)
    if (file.exists(ziel) || file.exists(file.path(raw_ordner, alt_fname))) {
      message("Schon da: ", jahr, "Q", q)
      geladen <- TRUE
      break
    }
    
    erfolg <- tryCatch({
      download.file(url, destfile = ziel, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE,
    warning = function(w) FALSE)
    
    # Prüfen, ob die Datei wirklich existiert und vollständig ist
    if (erfolg && file.exists(ziel) && file.size(ziel) > 30000000) {
      message("Geladen: ", fname, " (",
              round(file.size(ziel) / 1024 / 1024, 1), " MB)")
      geladen <- TRUE
      break
    } else if (file.exists(ziel)) {
      file.remove(ziel)   
    }
  }
  
  if (!geladen) {
    message("Nicht gefunden: ", jahr, "Q", q)
  }
  
  Sys.sleep(3) 
}


# 4) Liste aller ZIPs neu einlesen (nach dem Aufräumen)
zip_dateien <- list.files(raw_ordner, pattern = "\\.zip$", full.names = TRUE)

tabellen <- c("DEMO", "DRUG", "INDI", "OUTC", "REAC", "RPSR", "THER")

# 5) Schleife über alle 54 Quartale
for (zip_pfad in zip_dateien) {
  # Quartal-Label aus Filename extrahieren, z.B. "faers_ascii_2013Q1.zip" -> "2013Q1"
  fname   <- basename(zip_pfad)
  quartal <- toupper(regmatches(fname, regexpr("\\d{4}[Qq]\\d", fname)))
  
  ziel_ordner <- file.path(processed_ordner, quartal)
  
  # Skip wenn schon fertig (alle 7 Tabellen schon da)
  if (all(file.exists(file.path(ziel_ordner, paste0(tabellen, ".txt"))))) {
    message("Schon fertig: ", quartal)
    next
  }
  
  dir.create(ziel_ordner, showWarnings = FALSE, recursive = TRUE)
  
  # In temporären Ordner entpacken (wird nach jedem Quartal weggeräumt)
  tmp_ordner <- tempfile()
  dir.create(tmp_ordner)
  unzip(zip_pfad, exdir = tmp_ordner)
  
  # Alle .txt-Files finden (egal ob in ascii/ oder ASCII/, in Deleted/, etc.)
  alle_txt <- list.files(tmp_ordner, pattern = "\\.txt$",
                         recursive = TRUE, full.names = TRUE,
                         ignore.case = TRUE)
  
  
  # Pro Tabelle: das passende File finden und unter generischem Namen ablegen

  for (tab in tabellen) {
    treffer <- alle_txt[grepl(paste0("^", tab, "\\d"), basename(alle_txt),
                              ignore.case = TRUE)]
    if (length(treffer) >= 1) {
      file.copy(treffer[1],
                file.path(ziel_ordner, paste0(tab, ".txt")),
                overwrite = TRUE)
    }
  }
  
  # DELETE-File (falls vorhanden, ab Q3 2014)
  delete_file <- alle_txt[grepl("^DELETE", basename(alle_txt), ignore.case = TRUE)]
  if (length(delete_file) >= 1) {
    file.copy(delete_file[1],
              file.path(ziel_ordner, "DELETE.txt"),
              overwrite = TRUE)
  }
  
  unlink(tmp_ordner, recursive = TRUE)
  message("Fertig: ", quartal)
}


