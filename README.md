# Data\_Process\_LNW3



Dieses Repository enthält die Skripte und Dokumentation zur Verarbeitung und Analyse von Rohdaten für eine Shiny app.

Ziel ist es, die Daten reproduzierbar vorzubereiten und in die Shiny app vollständig zu laden.



Grundstruktur

├── Organisation\_Rohdaten/			   # Hier werden die Rohen daten platziert
├── app\_data					   # Hier werden die bearbeiteten Daten abgelegt
├── Dokumentation/        			   # Methodik, Beschreibungen, Notizen
├── Hauptbericht/         			   # Finale Auswertung / Bericht
├── Programmiercode, verwaltet mit git/            # Skripte zur Datenverarbeitung und Analyse
│   ├── Daten herunterladen.R        		   # R-Skript für herunterladen und ablegen in Organisation Rohdaten
│   ├── Daten\_fuer\_shiny.R   			   # verarbeitet Daten aus Organisation Rohdaten und erstellt ordner app\_data
│   └── app1.R					   # Startet eine Shiny app anhand von Daten in app\_data

├── .gitignore            			   # Schließt Datenordner aus
└── README.md





Struktur und Dateinamen müssen exakt eingehalten werden, damit die Skripte funktionieren.



install.packages(c("rvest","stringr","data.table","here","shiny","ggplot2","DT"))



"Daten herunterladen.R" - Dieses Skript lädt alle vorhandenen Quartale ab 2016 Q 1 runter. Dies wird begrenzt dam

Daten\_fuer\_shiny.R	- Dieses Skript verarbeitet die heruntergeladenen Daten in Organisation\_Rohdaten und speichert diese in 7 				  verschiedenen Datensets in app\_data

app1.R			- Dieses Skript startet eine Shiny app und zeigt visualisationen und Analytische Grafiken anhand der verarbeiteten 			  7 Datensets aus app\_data



