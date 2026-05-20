# Data_Process_LNW3

Dieses Repository enthält die Skripte und Dokumentation zur Verarbeitung
und Analyse von FAERS-Rohdaten der FDA für eine Shiny-App.

Ziel ist es, die Daten reproduzierbar vorzubereiten und in der Shiny-App
darzustellen.


## Grundstruktur

```
Data_Process_LNW3/
├── Organisation_Rohdaten/              # Hier werden die Rohdaten (ZIPs) abgelegt
├── app_data/                            # Hier werden die aufbereiteten Daten (.fst) abgelegt
├── Dokumentation/                       # Vollstaendige Projektdokumentation
│   └── Dokumentation.md                 # Hauptdokumentation, dort steht alles Wichtige
├── Programmiercode, verwaltet mit Git/  # Skripte zur Datenverarbeitung und App
│   ├── Daten herunterladen.R            # Lädt FAERS-ZIPs ab 2012 von der FDA-Webseite
│   ├── Daten_fuer_Shiny.R               # Bereitet die Daten als .fst-Dateien auf
│   └── app1.R                            # Startet die Shiny-App
├── archiv/                              # Frühere Skript-Versionen (zur Nachvollziehbarkeit)
├── .gitignore                            # Schliesst grosse Datenordner vom Repo aus
└── README.md
```

Struktur und Dateinamen müssen exakt eingehalten werden, damit die
Skripte funktionieren.


## Schnellstart

1. Repository klonen
2. `LNW3_Data_Process.Rproj` in RStudio öffnen
3. R-Pakete einmalig installieren:

```r
install.packages(c("rvest", "stringr", "data.table", "dplyr",
                   "here", "fst", "shiny", "ggplot2", "DT"))
```

4. Skripte in dieser Reihenfolge ausführen:

| Skript | Was es tut |
|---|---|
| `Daten herunterladen.R` | Lädt alle FAERS-Quartale ab 2012 von der FDA und entpackt sie in `Organisation_Rohdaten/`. Begrenzung auf ab 2012, da die Aufgabenstellung dies vorgibt. |
| `Daten_fuer_Shiny.R` | Verarbeitet die heruntergeladenen Daten und speichert sie in 7 `.fst`-Dateien in `app_data/`. |
| `app1.R` | Startet die Shiny-App und zeigt Visualisierungen und analytische Grafiken anhand der aufbereiteten Daten. |


## Weitere Informationen

Eine vollständige Dokumentation mit Projektübersicht, Pipeline-Beschreibung,
FAERS-Datenstruktur, Designentscheidungen und Anleitung zum Ausführen
befindet sich unter `Dokumentation/Dokumentation.md`.
