# =============================================================
# master merge.R  (v2 - robust gegen Typkonflikte)
# -------------------------------------------------------------
# Fuehrt alle aggregierten Quartals-RDS-Dateien zu einem
# Master-Datensatz (faers_master.rds) zusammen.
#
# Verbesserungen gegenueber v1:
#   - Konvertiert ALLE Spalten beim Einlesen zu character.
#     Damit verschwinden Typkonflikte (z.B. "age" mal integer,
#     mal character), die sonst bind_rows() crashen lassen.
#   - Gibt Speicher zwischendurch frei (rm + gc), um RAM-Spitzen
#     zu reduzieren.
#   - Ignoriert eine eventuell schon vorhandene faers_master.rds,
#     damit man das Skript gefahrlos mehrfach starten kann.
#   - Fortschritts-Meldungen pro Quartal.
# =============================================================

library(dplyr)
library(here)

# -------------------------------------------------------------
# 1) Alle Quartals-RDS finden
# -------------------------------------------------------------
rds_files <- list.files(
  here("Organisation_processed_data"),
  pattern = "\\.rds$",
  recursive = TRUE,
  full.names = TRUE)

# Master-Datei (falls schon vorhanden) ausschliessen
rds_files <- rds_files[!grepl("faers_master", basename(rds_files))]

message("Gefundene Quartals-RDS: ", length(rds_files))

# -------------------------------------------------------------
# 2) Lade-Funktion: konvertiert alle Spalten zu character
#    -> verhindert Typkonflikte beim bind_rows()
# -------------------------------------------------------------
load_and_unify <- function(f) {
  df <- readRDS(f)
  df[] <- lapply(df, as.character)
  df
}

# -------------------------------------------------------------
# 3) Alle Quartale laden (mit Fortschrittsanzeige)
# -------------------------------------------------------------
message("Lade alle Quartale (kann ein paar Minuten dauern)...")

all_dfs <- vector("list", length(rds_files))

for (i in seq_along(rds_files)) {
  message("  [", i, "/", length(rds_files), "] ",
          basename(rds_files[i]))
  all_dfs[[i]] <- load_and_unify(rds_files[i])
}

# -------------------------------------------------------------
# 4) Alles zusammenstapeln
# -------------------------------------------------------------
message("Stapele zu einem Datensatz...")
all_data <- bind_rows(all_dfs)

# Liste freigeben -> RAM sparen
rm(all_dfs)
gc()

message("Anzahl Zeilen vor distinct: ", nrow(all_data))

# -------------------------------------------------------------
# 5) Duplikate entfernen (nach primaryid)
# -------------------------------------------------------------
message("Entferne Duplikate ueber primaryid...")
all_data <- all_data %>%
  distinct(primaryid, .keep_all = TRUE)

message("Anzahl Zeilen nach distinct: ", nrow(all_data))

# -------------------------------------------------------------
# 6) Speichern
# -------------------------------------------------------------
out_path <- here("Organisation_processed_data", "faers_master.rds")

message("Speichere Master-Datensatz...")
saveRDS(all_data, out_path)

message("\n========== FERTIG ==========")
message("Gespeichert: ", out_path)
message("Zeilen:      ", nrow(all_data))
message("Spalten:     ", ncol(all_data))
message("Groesse:     ",
        round(file.info(out_path)$size / 1024^2, 1), " MB")
