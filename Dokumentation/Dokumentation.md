# Dokumentation – Praktikum 3: FAERS Adverse Event Reporting

ZHAW PM2, FS 2026

## 1. Übersicht

Dieses Projekt verarbeitet öffentlich verfügbare Daten des FDA Adverse
Event Reporting System (FAERS) und stellt sie in einer interaktiven
Shiny-App zur Auswertung bereit. Die App erlaubt es, für ein gewähltes
Medikament gemeldete Nebenwirkungen, Therapieverläufe und Outcomes nach
Alter, Geschlecht und der Rolle des Medikaments zu filtern.

FAERS stellt Gesundheitsdaten dar. Primär geht es darum, die
Nebenwirkungen unterschiedlichster Medikamente zu erfassen. Zudem gibt
es zu diesen Meldungen noch alle möglichen Zusatzinformationen wie
demografische Daten, Therapieverlauf, Auswirkungen und vieles mehr.

**Datenquelle:** FAERS Quarterly Data Files der FDA
(https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html)

**Betrachteter Zeitraum:** Daten ab 2012 (entsprechend Aufgabenstellung).
Für die Shiny-App werden aus Performance-Gründen die letzten 8 Quartale
(2 Jahre) verwendet.

**Endergebnis:** Eine Shiny-App mit 8 Statistiken, davon 6 gemäss
Aufgabenstellung und 2 selbst gewählte Zusatz-Statistiken.


## 2. Projektstruktur

Das Repository ist nach der Vorgabe der Aufgabenstellung organisiert:

```
Data_Process_LNW3/
├── Organisation_Rohdaten/                 # Heruntergeladene ZIPs (FAERS-Quartale)
├── Organisation_processed_data/           # Aufbereitete .txt-Dateien (optional, alter Workflow)
├── app_data/                              # Vom Skript erzeugte .fst-Dateien für die App
├── Programmiercode, verwaltet mit Git/    # Aktiver R-Code
│   ├── Daten herunterladen.R              # 1) Lädt ZIPs von der FDA-Webseite
│   ├── Daten_fuer_Shiny.R                 # 2) Bereitet die Daten als .fst auf
│   └── app1.R                             # 3) Shiny-App mit allen Auswertungen
├── archiv/                                # Frühere Skript-Versionen (zur Nachvollziehbarkeit)
├── Dokumentation/                         # Diese Dokumentation + ursprünglicher Plan
├── README.md                              # Kurzbeschreibung des Repos
└── LNW3_Data_Process.Rproj                # RStudio-Projektdatei
```

Die grossen Datenordner (`Organisation_Rohdaten/`, `app_data/`) werden
durch die `.gitignore` vom Repository ausgeschlossen, da sie zusammen
GB's gross sind.


## 3. Verarbeitungsablauf

Die Verarbeitung läuft in drei Schritten ab. Jedes Skript deckt
bestimmte Teilaufgaben aus der Aufgabenstellung ab.

### Schritt 1: `Daten herunterladen.R`
*Aufgabenbezug: Woche 1, Aufgabe 2a und 2b (Digitale Datenakquise + Entpacken)*

Lädt von der FDA-Webseite alle FAERS-Quartals-ZIPs ab dem Jahr 2012
herunter (`rvest`, `html_elements`, `html_attr`, `download.file`) und
entpackt sie mit `unzip()` in `Organisation_Rohdaten/faers_data/`. Jeder
Quartal-Ordner behält die ursprüngliche FAERS-Struktur mit einem
`ascii/`-Unterordner.

Bereits heruntergeladene Quartale werden übersprungen, das Skript kann
also mehrfach gestartet werden ohne alles neu zu laden.

### Schritt 2: `Daten_fuer_Shiny.R`
*Aufgabenbezug: Woche 2, Aufgaben 1a, 1b und 1c*

**Aufgabe 1a (Einlesefunktion):**
Die Funktion `read_entity_quartal(entity, q_ordner)` liest mit `fread()`
für eine vorgegebene Entität (DEMO, DRUG, INDI, OUTC, REAC, RPSR, THER)
die Daten eines einzelnen Quartals ein. Die Hilfsfunktion `find_faers_file()`
sucht die passende `.txt`-Datei rekursiv im Quartal-Ordner, unabhängig
von der konkreten Verschachtelung (z.B. `ascii/`-Unterordner).

**Aufgabe 1b (Zusammenführen pro Entität):**
Pro Entität werden die letzten 8 Quartale (= 2 Jahre) eingelesen und mit
`rbindlist(fill = TRUE, use.names = TRUE)` zu einer Tabelle zusammengeführt.
Die Spalten `quartal` und `jahr` werden hinzugefügt.

Die Datenstruktur hat sich zwischen 2014Q2 und 2014Q3 geändert (neue
und umbenannte Variablen, siehe SummaryofChanges2014Q3QDE.pdf). Durch
`fill = TRUE` werden fehlende Spalten in einzelnen Quartalen automatisch
mit `NA` aufgefüllt, sodass auch ältere Quartale problemlos integriert
werden können.

**Aufgabe 1c (Schnelles Speicherformat):**
Die zusammengeführten Tabellen werden als `.fst`-Dateien in `app_data/`
gespeichert (eine Datei pro Entität). Das Format `.fst` sollte deutlich
schneller als RDS laden und ist gleichwertig zu den in der Aufgabenstellung
genannten Formaten `feather` und `parquet`.

### Schritt 3: `app1.R`
*Aufgabenbezug: Woche 2, Aufgabe 1d (Vereinigungs-Funktion) + Woche 3 (Shiny-App)*

**Aufgabe 1d (Funktion pro Medikament):**
Die Funktion `get_drug_data()` in der App vereinigt für ein gewähltes
Medikament die Tabellen Drug, Demographic, Outcome, Therapy, Indication
und Reaction. Sie startet mit der Drug-Tabelle (gefiltert nach `drugname`),
erweitert die Auswahl auf die vollständigen Sequenzen (alle Einträge mit
derselben `primaryid`) und vereinigt diese mit Demographic. Aus der
Outcome-Tabelle wird pro `primaryid` nur der letzte Eintrag verwendet.
Anschliessend werden Therapy, Indication und Reaction angehängt.

**Woche 3 (Shiny-App):**
Die App liest die `.fst`-Dateien beim Start einmalig in den
Arbeitsspeicher und stellt alle Auswertungen über
ein einheitliches User-Interface zur Verfügung.


## 4. FAERS-Datenstruktur

Die FAERS-Daten bestehen aus sieben Tabellen pro Quartal, die über
die Schlüsselspalte primaryid miteinander verknüpft sind:

| Tabelle | Inhalt | Wichtige Spalten |
|---|---|---|
| DEMO | Patient/Meldung | primaryid, age, sex, event_dt, reporter_country |
| DRUG | Medikament(e) | primaryid, drug_seq, drugname, prod_ai, role_cod, dose_amt |
| THER | Therapieverlauf | primaryid, dsg_drug_seq, start_dt, end_dt, dur |
| INDI | Indikation | primaryid, indi_drug_seq, indi_pt |
| REAC | Reaktion | primaryid, pt, drug_rec_act |
| OUTC | Outcome | primaryid, outc_cod |
| RPSR | Meldequelle | primaryid, rpsr_cod |

Eine Meldung (primaryid) kann mehrere Medikamente enthalten (DRUG hat
mehrere Zeilen pro primaryid), die jeweils eine eigene drug_seq-Nummer
haben. THER und INDI sind über diese drug_seq mit der DRUG-Tabelle
verknüpft: THER.dsg_drug_seq und INDI.indi_drug_seq verweisen jeweils
auf die DRUG.drug_seq desselben Falls.

Ab dem dritten Quartal 2014 hat die FDA die FAERS-Struktur überarbeitet.
Es wurden Spalten umbenannt, hinzugefügt oder entfernt. Fehlende Spalten
in einzelnen Quartalen werden mit `NA` aufgefüllt.


## 5. Zusatz-Statistiken

**Grafik 7. Zeit zwischen Therapiebeginn und Ereignisdatum**

In diesem Histogramm wollen wir herausfinden, wie stark die zeitlichen
Unterschiede sind, bei welcher die Nebenwirkungen eintrafen. Primär will
man damit erkennen, ob das Medikament kurzfristige Symptome oder eher
Spätfolgen verursacht. Viele Daten waren leider inkonsistent. Zu erklären
ist das damit, dass die Schreibweise der Daten in den verschiedenen
Ländern unterschiedlich ist. Es ist schwer zu sagen, welche Daten wirklich
korrekt sind, aber mit der Medianlinie sollte eine aussagekräftige Zahl
entstehen.

**Grafik 8. Altersverteilung nach Geschlecht**

Überlagertes Histogramm der Altersverteilung, getrennt nach Männern und
Frauen. Zeigt das demografische Profil der Patienten, bei denen das
Medikament zu einer Meldung geführt hat. Berücksichtigt nur Fälle mit
gültiger Altersangabe (0–120 Jahre) und bekanntem Geschlecht (M oder F).
Mit dieser Grafik soll herausgefunden werden, ob die Medikamente Frauen
oder Männern verschrieben bekommen.


## 6. Bekannte Limitierungen

**Datenqualität:**
FAERS ist ein freiwilliges Meldesystem. Die Daten enthalten
Eingabefehler, unvollständige Angaben und können nicht als
populationsbezogene Statistik interpretiert werden. Eine Meldung
beweist keine Kausalität, nur Vermutungen.

**Datumsangaben:**
Datumsfelder wie `event_dt` und `start_dt` sind im Format YYYYMMDD
gespeichert, aber teilweise unvollständig (z.B. nur YYYY oder YYYYMM)
oder offensichtlich fehlerhaft (z.B. Ereignis vor Therapiebeginn).
Solche Werte werden je nach Grafik gefiltert oder als NA behandelt.

**Zeitlicher Umfang in der App:**
Aus Performance-Gründen werden in der App nur die letzten 8 Quartale
verwendet. Längerfristige Trends über mehrere Jahre sind damit nicht
sichtbar — dafür wäre eine Erweiterung von `Daten_fuer_Shiny.R`
notwendig (`N_QUARTALE` erhöhen). Es würde die Datei jedoch sehr
langsam machen.

**Medikamenten-Suche:**
Die Auswahl in der App beschränkt sich auf die Top 200 häufigsten
Medikamentennamen. Seltene Medikamente sind nicht direkt wählbar.
Diese Einschränkung wurde aus Performance-Gründen gewählt, da der
`selectizeInput` mit allen Tausenden Medikamentennamen ebenfalls
träge wird.


## 7. Anleitung zum Ausführen

Damit der Verarbeitungsablauf reproduzierbar ist, sind die folgenden
Schritte einzuhalten:

### Schritt 1: Repository einrichten

```bash
git clone https://github.zhaw.ch/koenisal/Data_Process_LNW3.git
```

In RStudio die Datei `LNW3_Data_Process.Rproj` öffnen, damit `here()`
das Projektverzeichnis korrekt erkennt.

### Schritt 2: R-Pakete installieren

In der R-Console einmalig ausführen:

```r
install.packages(c("rvest", "stringr", "data.table", "dplyr",
                   "here", "fst", "shiny", "ggplot2", "DT"))
```

### Schritt 3: Daten herunterladen

```r
source("Programmiercode, verwaltet mit Git/Daten herunterladen.R")
```

Lädt alle FAERS-Quartals-ZIPs ab 2012 herunter und entpackt sie nach
`Organisation_Rohdaten/faers_data/`. Dauer: je nach Verbindung mehrere
Stunden. Das Skript kann unterbrochen und erneut gestartet werden —
bereits vorhandene Quartale werden übersprungen.

### Schritt 4: Daten für die Shiny-App aufbereiten

```r
source("Programmiercode, verwaltet mit Git/Daten_fuer_Shiny.R")
```

Erzeugt sieben `.fst`-Dateien in `app_data/`. Dauer: ca. 5–10 Minuten.

### Schritt 5: Shiny-App starten

In RStudio die Datei `Programmiercode, verwaltet mit Git/app1.R`
öffnen und oben rechts auf **"Run App"** klicken.

Alternativ in der Console:

```r
shiny::runApp("Programmiercode, verwaltet mit Git/app1.R")
```

Die App lädt die `.fst`-Dateien in den Arbeitsspeicher (ca. 5–15
Sekunden) und öffnet sich anschliessend im Browser.


## 8. Referenzen

**Datenquelle:**
FDA Adverse Event Reporting System (FAERS) Quarterly Data Files,
https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html

**FDA-Dokumentation:**
- "ASC_NTS.DOC" – Strukturbeschreibung der Quartals-Daten (FDA, 2016)
- "SummaryofChanges2014Q3QDE.pdf" – Änderungen ab Q3 2014

**Verwendete R-Pakete:**
- `shiny`, `DT` – User Interface
- `ggplot2` – Grafiken
- `data.table`, `dplyr` – Datenverarbeitung
- `fst` – schnelles Speicherformat
- `rvest`, `stringr` – Web-Scraping beim Download
- `here` – relative Pfade im Projekt
