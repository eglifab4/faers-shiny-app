# =============================================================
# Daten_aggregieren.R  (v2 - robust)
# -------------------------------------------------------------
# Liest die bereits entpackten .txt-Dateien (DEMO, DRUG, REAC)
# aus Organisation_processed_data/<QUARTAL>/, aggregiert sie
# pro primaryid und speichert pro Quartal eine .rds-Datei.
#
# Verbesserungen gegenueber v1:
#   - Saeubert BOM-Reste am Anfang von Spaltennamen
#     (z.B. "i..primaryid" -> "primaryid")
#   - prod_ai (Wirkstoff) ist optional - alte Quartale haben sie nicht
#   - Pro Quartal Fehlerbehandlung mit tryCatch -> Schleife laeuft
#     auch dann weiter, wenn ein Quartal kaputt ist
#   - Gibt am Ende eine Zusammenfassung aus
# =============================================================

library(dplyr)
library(here)

# -------------------------------------------------------------
# 1) Pfade
# -------------------------------------------------------------
processed_ordner <- here("Organisation_processed_data")

quartal_ordner <- list.dirs(processed_ordner,
                            recursive = FALSE,
                            full.names = TRUE)

message("Gefundene Quartale: ", length(quartal_ordner))

# -------------------------------------------------------------
# 2) Lese-Funktion mit Spaltennamen-Saeuberung
# -------------------------------------------------------------
read_faers <- function(path) {

  df <- read.table(
    path,
    sep = "$",
    header = TRUE,
    fill = TRUE,
    quote = "",
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "."),
    comment.char = "",
    fileEncoding = "latin1")

  # BOM-Reste / X.-Praefixe am Anfang von Spaltennamen entfernen
  # z.B. "i..primaryid" oder "X.primaryid" -> "primaryid"
  names(df) <- sub("^[^A-Za-z0-9_]+", "", names(df))
  names(df) <- tolower(names(df))

  df
}

# -------------------------------------------------------------
# 3) Verarbeitungs-Funktion pro Quartal (mit Fehlerbehandlung)
# -------------------------------------------------------------
aggregiere_quartal <- function(q_ordner) {

  q_name <- basename(q_ordner)
  out_pfad <- file.path(q_ordner, paste0(q_name, "_clean.rds"))

  # Bereits aggregiert? -> ueberspringen
  if (file.exists(out_pfad)) {
    message("[SKIP]    ", q_name, " (schon aggregiert)")
    return(invisible(list(quartal = q_name, status = "skip")))
  }

  demo_pfad <- file.path(q_ordner, "DEMO.txt")
  drug_pfad <- file.path(q_ordner, "DRUG.txt")
  reac_pfad <- file.path(q_ordner, "REAC.txt")

  if (!all(file.exists(demo_pfad, drug_pfad, reac_pfad))) {
    message("[FEHLT]   ", q_name, " (DEMO/DRUG/REAC.txt fehlt)")
    return(invisible(list(quartal = q_name, status = "missing")))
  }

  # Verarbeitung in tryCatch -> Fehler stoppt nicht die ganze Schleife
  result <- tryCatch({

    message("Verarbeite: ", q_name)

    demo <- read_faers(demo_pfad)
    drug <- read_faers(drug_pfad)
    reac <- read_faers(reac_pfad)

    # Sicherheitscheck: primaryid muss in allen drei Tabellen vorhanden sein
    for (tab_name in c("demo", "drug", "reac")) {
      tab <- get(tab_name)
      if (!"primaryid" %in% names(tab)) {
        stop("Spalte 'primaryid' fehlt in ", toupper(tab_name),
             " - vorhandene Spalten: ",
             paste(names(tab), collapse = ", "))
      }
    }

    demo$primaryid <- as.character(demo$primaryid)
    drug$primaryid <- as.character(drug$primaryid)
    reac$primaryid <- as.character(reac$primaryid)

    # DRUG: aggregieren - prod_ai ist optional (fehlt in alten Quartalen)
    if ("prod_ai" %in% names(drug)) {
      drug_summary <- drug %>%
        group_by(primaryid) %>%
        summarise(
          n_drugs     = n(),
          drugs       = paste(unique(drugname), collapse = "; "),
          ingredients = paste(unique(prod_ai),  collapse = "; "),
          .groups = "drop")
    } else {
      drug_summary <- drug %>%
        group_by(primaryid) %>%
        summarise(
          n_drugs     = n(),
          drugs       = paste(unique(drugname), collapse = "; "),
          ingredients = NA_character_,
          .groups = "drop")
    }

    # REAC: aggregieren
    reac_summary <- reac %>%
      group_by(primaryid) %>%
      summarise(
        n_reac    = n(),
        reactions = paste(unique(pt), collapse = "; "),
        .groups = "drop")

    # Joins
    faers_final <- demo %>%
      left_join(drug_summary, by = "primaryid") %>%
      left_join(reac_summary, by = "primaryid")

    saveRDS(faers_final, out_pfad)
    message("[OK]      ", q_name, " (", nrow(faers_final), " Faelle)")

    list(quartal = q_name, status = "ok", n = nrow(faers_final))

  }, error = function(e) {
    message("[FEHLER]  ", q_name, ": ", conditionMessage(e))
    list(quartal = q_name, status = "error", error = conditionMessage(e))
  })

  invisible(result)
}

# -------------------------------------------------------------
# 4) Schleife ueber alle Quartale
# -------------------------------------------------------------
results <- lapply(quartal_ordner, aggregiere_quartal)

# -------------------------------------------------------------
# 5) Zusammenfassung am Ende
# -------------------------------------------------------------
status_zaehlen <- table(sapply(results, function(x) x$status))

message("\n========== Zusammenfassung ==========")
message("Quartale insgesamt:   ", length(results))
message("Erfolgreich (ok):     ", ifelse("ok"      %in% names(status_zaehlen), status_zaehlen["ok"],      0))
message("Uebersprungen (skip): ", ifelse("skip"    %in% names(status_zaehlen), status_zaehlen["skip"],    0))
message("Dateien fehlen:       ", ifelse("missing" %in% names(status_zaehlen), status_zaehlen["missing"], 0))
message("Fehler:               ", ifelse("error"   %in% names(status_zaehlen), status_zaehlen["error"],   0))

# Liste der fehlerhaften Quartale anzeigen
fehler_quartale <- Filter(function(x) x$status == "error", results)
if (length(fehler_quartale) > 0) {
  message("\nFehlerhafte Quartale:")
  for (f in fehler_quartale) {
    message("  - ", f$quartal, ": ", f$error)
  }
}
