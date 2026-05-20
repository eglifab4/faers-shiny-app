# =============================================================
# app.R - FAERS Adverse Event Explorer (Shiny App)
# -------------------------------------------------------------
# Praktikum 3 - PM2 ZHAW 2026
#
# Voraussetzung:
#   In app_data/ liegen DEMO.fst, DRUG.fst, INDI.fst,
#   OUTC.fst, REAC.fst, RPSR.fst, THER.fst (durch
#   Daten_fuer_Shiny.R erzeugt).
#
# Starten:
#   In RStudio: oben rechts "Run App" klicken
#   Oder Console:  shiny::runApp("app1.R")
# =============================================================

library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(here)
library(fst)

# =============================================================
# 1) DATEN LADEN (einmal beim App-Start)
# -------------------------------------------------------------
# Wir nutzen das fst-Format statt RDS, weil es ~10-20x schneller
# lädt. Das entspricht auch der Empfehlung in der Aufgabenstellung
# (Praktikum 3, Aufgabe 1c: feather / parquet / fst).
# =============================================================
message("App startet... Daten werden geladen...")
t_start <- Sys.time()

app_data_dir <- here("app_data")

demo <- read_fst(file.path(app_data_dir, "DEMO.fst"), as.data.table = TRUE)
drug <- read_fst(file.path(app_data_dir, "DRUG.fst"), as.data.table = TRUE)
indi <- read_fst(file.path(app_data_dir, "INDI.fst"), as.data.table = TRUE)
outc <- read_fst(file.path(app_data_dir, "OUTC.fst"), as.data.table = TRUE)
reac <- read_fst(file.path(app_data_dir, "REAC.fst"), as.data.table = TRUE)
rpsr <- read_fst(file.path(app_data_dir, "RPSR.fst"), as.data.table = TRUE)
ther <- read_fst(file.path(app_data_dir, "THER.fst"), as.data.table = TRUE)

message("Daten geladen in ",
        round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1),
        " Sekunden")

message("Daten geladen: ",
        format(nrow(demo), big.mark = "'"), " DEMO, ",
        format(nrow(drug), big.mark = "'"), " DRUG, ",
        format(nrow(reac), big.mark = "'"), " REAC")

# =============================================================
# 2) VORBERECHNUNGEN
# =============================================================

# Top-200 Medikamente für schnelles Auswahlmenü
top_drugs <- drug[!is.na(drugname) & drugname != "",
                  .N, by = drugname][order(-N)][1:200]

# Top-10 für Startanzeige
top10_drugs <- top_drugs[1:10]

# =============================================================
# 3) HELFER-FUNKTIONEN
# =============================================================

# -------------------------------------------------------------
# Datum-Parse-Funktion
# Unterstützt das FAERS-Format YYYYMMDD (z.B. "20230415")
# Gibt NA zurück bei ungültigem oder unvollständigem Datum
# -------------------------------------------------------------
parse_faers_date <- function(x) {
  x <- trimws(as.character(x))
  d <- rep(as.Date(NA), length(x))
  # Nur 8-stellige rein numerische Strings verarbeiten (= YYYYMMDD)
  valid <- !is.na(x) & nchar(x) == 8 & grepl("^[0-9]{8}$", x)
  if (any(valid)) {
    d[valid] <- suppressWarnings(as.Date(x[valid], format = "%Y%m%d"))
  }
  d
}

# -------------------------------------------------------------
# Hauptfunktion: für ein Medikament alle Daten zusammenstellen
# (entspricht Aufgabe 2d aus dem PDF)
# -------------------------------------------------------------
get_drug_data <- function(drug_input,
                          alter_min = 0, alter_max = 120,
                          geschlecht = c("M", "F", "UNK"),
                          role_cods  = c("PS", "SS", "C", "I", "")) {

  # Schritt 1: Alle Drug-Einträge mit dem gewählten Medikament
  drug_target <- drug[grepl(drug_input, drugname, ignore.case = TRUE)]

  # Falls Filter ROLE_COD: nur Fälle wo das Medikament diese Rolle hat
  if (length(role_cods) > 0 && length(role_cods) < 5) {
    drug_target <- drug_target[role_cod %in% role_cods]
  }

  # primaryids dieser Fälle
  ids <- unique(drug_target$primaryid)

  if (length(ids) == 0) {
    return(NULL)
  }

  # Schritt 2: Demographic der gefilterten Fälle
  demo_seq <- demo[primaryid %in% ids]

  # Filter: Geschlecht
  if (!is.null(geschlecht) && length(geschlecht) > 0) {
    # Spalte heisst entweder "sex" oder "gndr_cod" (je nach Quartal)
    if ("sex" %in% names(demo_seq)) {
      sex_col <- "sex"
    } else if ("gndr_cod" %in% names(demo_seq)) {
      sex_col <- "gndr_cod"
    } else {
      sex_col <- NULL
    }
    if (!is.null(sex_col)) {
      demo_seq <- demo_seq[get(sex_col) %in% geschlecht | is.na(get(sex_col))]
    }
  }

  # Filter: Alter (in Jahren)
  if (!is.null(alter_min) && !is.null(alter_max)) {
    # age ist character, wir konvertieren zu numeric
    demo_seq[, age_num := suppressWarnings(as.numeric(age))]
    # Behalte: passende Altersrange ODER fehlendes Alter (NA)
    demo_seq <- demo_seq[is.na(age_num) | (age_num >= alter_min & age_num <= alter_max)]
  }

  # Update ids nach Demo-Filtern
  ids_filtered <- unique(demo_seq$primaryid)

  # Schritt 3: Erweiterung auf vollständige Sequenzen
  # (alle Drug-Einträge der gefilterten primaryids - auch andere Medikamente!)
  drug_full_seq <- drug[primaryid %in% ids_filtered]

  # Outcome: nur letzter Eintrag pro primaryid
  outc_seq  <- outc[primaryid %in% ids_filtered]
  outc_last <- outc_seq[, .SD[.N], by = primaryid]

  # Therapy
  ther_seq <- ther[primaryid %in% ids_filtered]

  # Indication
  indi_seq <- indi[primaryid %in% ids_filtered]

  # Reactions
  reac_seq <- reac[primaryid %in% ids_filtered]

  # Report Sources
  rpsr_seq <- rpsr[primaryid %in% ids_filtered]

  list(
    n_faelle    = length(ids_filtered),
    drug_target = drug[primaryid %in% ids_filtered &
                         grepl(drug_input, drugname, ignore.case = TRUE)],
    drug_full   = drug_full_seq,
    demo        = demo_seq,
    outc_last   = outc_last,
    ther        = ther_seq,
    indi        = indi_seq,
    reac        = reac_seq,
    rpsr        = rpsr_seq
  )
}

# =============================================================
# 4) USER INTERFACE
# =============================================================

ui <- fluidPage(

  titlePanel("FAERS Adverse Event Explorer"),

  fluidRow(
    column(12,
           p(strong("FDA Adverse Event Reporting System (FAERS)"),
             "- Auswertung gemeldeter Nebenwirkungen pro Medikament.",
             "Daten der letzten 2 Jahre."),
           hr())
  ),

  sidebarLayout(

    # ============== SIDEBAR ==============
    sidebarPanel(width = 3,

      h4("1. Medikament"),
      selectizeInput("drug_choice",
                     "Medikament auswählen:",
                     choices = NULL,  # wird im Server gefüllt
                     selected = NULL,
                     options = list(placeholder = "Tippen, um zu suchen...")),

      hr(),

      h4("2. Filter (optional)"),

      sliderInput("alter_range",
                  "Altersbereich (Jahre):",
                  min = 0, max = 120,
                  value = c(0, 120),
                  step = 5),

      checkboxGroupInput("geschlecht",
                         "Geschlecht:",
                         choices = c("Männlich" = "M",
                                     "Weiblich" = "F",
                                     "Unbekannt" = "UNK"),
                         selected = c("M", "F", "UNK")),

      checkboxGroupInput("role_cod",
                         "Rolle des Medikaments (ROLE_COD):",
                         choices = c("Primary Suspect (PS)"   = "PS",
                                     "Secondary Suspect (SS)" = "SS",
                                     "Concomitant (C)"        = "C",
                                     "Interacting (I)"        = "I"),
                         selected = c("PS", "SS", "C", "I")),

      actionButton("apply_filter",
                   "Filter anwenden",
                   icon = icon("play"),
                   class = "btn-primary",
                   width = "100%"),

      hr(),

      div(style = "font-size: 0.85em; color: #666;",
          textOutput("status_text"))
    ),

    # ============== MAIN PANEL ==============
    mainPanel(width = 9,

      tabsetPanel(id = "tabs",

        # ------------------ TAB 1: ÜBERSICHT ------------------
        tabPanel("Übersicht",
                 br(),
                 h3("Top 10 häufigste Medikamente"),
                 p("Klicke auf eine Zeile, um das Medikament auszuwählen."),
                 DT::dataTableOutput("top10_table"),
                 br(),
                 h4("Aktuell ausgewählt:"),
                 verbatimTextOutput("current_drug_info")
        ),

        # ------------------ TAB 2: PFLICHT-STATISTIKEN ------------------
        tabPanel("Statistiken",
                 br(),
                 conditionalPanel(
                   condition = "output.has_data == false",
                   div(class = "alert alert-info",
                       "Bitte links ein Medikament auswählen und 'Filter anwenden' klicken.")
                 ),

                 conditionalPanel(
                   condition = "output.has_data == true",

                   h3("1. Anzahl Meldungen pro Quartal nach Rolle des Medikaments"),
                   plotOutput("plot_quartal_role", height = "350px"),

                   br(),
                   h3("2. Top mit-vorkommende Substanzen (in den anderen Rollen)"),
                   p("Welche anderen Medikamente kommen am häufigsten zusammen mit dem ausgewählten Medikament vor?"),
                   DT::dataTableOutput("table_cosubstanzen"),

                   br(),
                   h3("3. Histogramm der Therapielänge"),
                   plotOutput("plot_therapie_dauer", height = "350px"),

                   br(),
                   h3("4. Top 10 Indikationen"),
                   plotOutput("plot_top_indi", height = "400px"),

                   br(),
                   h3("5. Top 10 Reaktionen"),
                   plotOutput("plot_top_reac", height = "400px"),

                   br(),
                   h3("6. Verteilung der Outcomes"),
                   p("Nur Fälle mit abgeschlossener Therapie"),
                   plotOutput("plot_outcomes", height = "350px")
                 )
        ),

        # ------------------ TAB 3: ZUSATZ-STATISTIKEN ------------------
        tabPanel("Zusatz-Statistiken",
                 br(),
                 conditionalPanel(
                   condition = "output.has_data == false",
                   div(class = "alert alert-info",
                       "Bitte links ein Medikament auswählen und 'Filter anwenden' klicken.")
                 ),

                 conditionalPanel(
                   condition = "output.has_data == true",

                   h3("7. Zeit zwischen Therapiebeginn und Ereignisdatum"),
                   p(
                     "Differenz in Tagen zwischen dem Start der Therapie und dem gemeldeten ",
                     "Ereignisdatum. Verknüpfung über drug_seq = dsg_drug_seq, damit nur ",
                     "die Therapie des ausgewählten Medikaments berücksichtigt wird. ",
                     "Negative Differenzen (Ereignis vor Therapiebeginn) werden als Eingabefehler ",
                     "ausgeschlossen."
                   ),
                   plotOutput("plot_zeit_diff", height = "400px"),

                   br(),

                   h3("8. Altersverteilung nach Geschlecht"),
                   p(
                     "Verteilung des Patientenalters in den gemeldeten Fällen, ",
                     "aufgeteilt nach Geschlecht. Zeigt das demografische Profil der ",
                     "Personen, bei denen das ausgewählte Medikament zu einer Meldung geführt hat. ",
                     "Berücksichtigt nur Fälle mit gültiger Altersangabe (0-120 Jahre) ",
                     "und bekanntem Geschlecht (M oder F)."
                   ),
                   plotOutput("plot_alter_geschlecht", height = "400px")
                 )
        ),

        # ------------------ TAB 4: ANLEITUNG ------------------
        tabPanel("Anleitung",
                 br(),
                 h3("So benutzt du diese App"),

                 h4("Schritt 1: Medikament wählen"),
                 p("In der linken Seitenleiste gibst du den Namen eines Medikaments ein.",
                   "Du kannst tippen und bekommst Vorschläge - oder du klickst",
                   "im Tab 'Übersicht' auf eine Zeile in der Top-10-Liste."),

                 h4("Schritt 2: Filter setzen (optional)"),
                 p("Falls du die Auswertung einschränken willst:",
                   tags$ul(
                     tags$li(strong("Altersbereich:"), "Schieberegler für Patientenalter."),
                     tags$li(strong("Geschlecht:"), "Männlich, weiblich oder unbekannt."),
                     tags$li(strong("Rolle des Medikaments:"),
                             "PS = Hauptverdächtiges Medikament,",
                             "SS = Nebenverdächtig,",
                             "C = Begleitmedikation (concomitant),",
                             "I = Interagierend.")
                   )),

                 h4("Schritt 3: Auf 'Filter anwenden' klicken"),
                 p("Erst nach diesem Klick wird die Analyse neu berechnet.",
                   "Wechsle dann in den Tab 'Statistiken' oder 'Zusatz-Statistiken'."),

                 hr(),

                 h4("Was zeigen die Statistiken?"),
                 tags$ul(
                   tags$li(strong("Meldungen pro Quartal:"),
                           "Wie viele Reports wurden eingereicht, aufgeteilt nach Rolle des Medikaments."),
                   tags$li(strong("Mit-vorkommende Substanzen:"),
                           "Andere Medikamente, die häufig zusammen mit dem ausgewählten gemeldet werden."),
                   tags$li(strong("Therapielänge:"),
                           "Verteilung, wie lange die Therapie dauerte (in Tagen)."),
                   tags$li(strong("Top Indikationen:"),
                           "Wofür wurde das Medikament am häufigsten verschrieben."),
                   tags$li(strong("Top Reaktionen:"),
                           "Welche Nebenwirkungen am häufigsten gemeldet wurden."),
                   tags$li(strong("Outcomes:"),
                           "Schwere der Folgen (Tod, Hospitalisierung, etc.)."),
                   tags$li(strong("Zeit Therapiebeginn bis Ereignis:"),
                           "Wie viele Tage lagen zwischen Therapiestart und dem gemeldeten Ereignis."),
                   tags$li(strong("Altersverteilung nach Geschlecht:"),
                           "Demografisches Profil der Patienten - Histogramm des Alters, aufgeteilt nach männlich/weiblich.")
                 ),

                 hr(),

                 h5("Datenquelle"),
                 p("FDA Adverse Event Reporting System (FAERS),",
                   tags$a(href = "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html",
                          "Quarterly Data Files"),
                   ".")
        )
      )
    )
  )
)

# =============================================================
# 5) SERVER
# =============================================================

server <- function(input, output, session) {

  # Top-200 Medikamente in Auswahl-Box laden (mit "tippen" suchbar)
  updateSelectizeInput(session, "drug_choice",
                       choices = top_drugs$drugname,
                       server = TRUE)

  # Deutsche Sprache für DataTables (Show entries / Search etc.)
  dt_lang_de <- list(
    info         = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen",
    lengthMenu   = "Zeige _MENU_ Einträge",
    search       = "Suche:",
    paginate     = list(previous = "Zurück", `next` = "Weiter",
                        first = "Erste", last = "Letzte"),
    zeroRecords  = "Keine passenden Einträge gefunden",
    emptyTable   = "Keine Daten verfügbar",
    infoEmpty    = "Zeige 0 Einträge",
    infoFiltered = "(gefiltert aus _MAX_ Einträgen)"
  )

  # ----- Top-10 Tabelle -----
  output$top10_table <- DT::renderDataTable({
    DT::datatable(
      top10_drugs,
      colnames = c("Medikament", "Anzahl Meldungen"),
      selection = "single",
      options = list(pageLength = 10, dom = "t",
                     language = dt_lang_de),
      rownames = FALSE
    )
  })

  # Klick in Top-10 -> Drug auswählen
  observeEvent(input$top10_table_rows_selected, {
    row <- input$top10_table_rows_selected
    if (length(row) > 0) {
      updateSelectizeInput(session, "drug_choice",
                           selected = top10_drugs$drugname[row])
    }
  })

  # ----- Aktuelle Drug-Info -----
  output$current_drug_info <- renderText({
    req(input$drug_choice)
    paste0(input$drug_choice,
           "\n\nKlicke links auf 'Filter anwenden', um die Statistiken zu berechnen.")
  })

  # ----- HAUPT-REACTIVE: nur bei Klick auf "Filter anwenden" -----
  filtered_data <- eventReactive(input$apply_filter, {

    req(input$drug_choice)

    showNotification("Berechne Daten...", type = "message", duration = 3)

    get_drug_data(
      drug_input = input$drug_choice,
      alter_min  = input$alter_range[1],
      alter_max  = input$alter_range[2],
      geschlecht = input$geschlecht,
      role_cods  = input$role_cod
    )
  })

  # ----- Status-Indikator -----
  output$status_text <- renderText({
    fd <- filtered_data()
    if (is.null(fd)) {
      "Keine Fälle gefunden mit diesen Filtern."
    } else {
      paste0(format(fd$n_faelle, big.mark = "'"), " Fälle (primaryid)")
    }
  })

  # has_data für conditionalPanel
  output$has_data <- reactive({
    fd <- filtered_data()
    !is.null(fd) && fd$n_faelle > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # =============================================
  # PFLICHT-STATISTIKEN
  # =============================================

  # 1) Meldungen pro Quartal nach ROLE_COD
  output$plot_quartal_role <- renderPlot({
    fd <- filtered_data()
    req(fd)

    dt <- fd$drug_target[, .N, by = .(quartal, role_cod)]
    dt <- dt[!is.na(role_cod) & role_cod != ""]

    ggplot(dt, aes(x = quartal, y = N, fill = role_cod)) +
      geom_col(position = "dodge") +
      labs(x = "Quartal", y = "Anzahl Meldungen",
           fill = "Rolle des Medikaments") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # 2) Top mit-vorkommende Substanzen (andere ROLE_COD)
  output$table_cosubstanzen <- DT::renderDataTable({
    fd <- filtered_data()
    req(fd)

    # Andere Drugs in den Sequenzen (nicht das ausgewählte selbst)
    co_drugs <- fd$drug_full[
      !grepl(input$drug_choice, drugname, ignore.case = TRUE)
    ]
    co_drugs <- co_drugs[!is.na(drugname) & drugname != ""]
    co_drugs <- co_drugs[!is.na(role_cod) & role_cod != ""]

    # Pro ROLE_COD die häufigsten zählen
    co_summary <- co_drugs[, .N, by = .(role_cod, drugname)][order(role_cod, -N)]
    co_summary <- co_summary[, head(.SD, 10), by = role_cod]

    DT::datatable(
      co_summary,
      colnames = c("Rolle", "Substanz", "Anzahl"),
      options = list(pageLength = 10, language = dt_lang_de),
      rownames = FALSE
    )
  })

  # 3) Histogramm Therapielänge
  output$plot_therapie_dauer <- renderPlot({
    fd <- filtered_data()
    req(fd)

    if (nrow(fd$ther) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Therapie-Daten vorhanden") +
               theme_void())
    }

    ther_dt <- copy(fd$ther)
    ther_dt[, dauer := suppressWarnings(as.numeric(dur))]
    ther_dt <- ther_dt[!is.na(dauer) & dauer > 0 & dauer < 5000]

    if (nrow(ther_dt) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine validen Therapie-Dauern") +
               theme_void())
    }

    ggplot(ther_dt, aes(x = dauer)) +
      geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
      labs(x = "Therapie-Dauer (Tage)", y = "Anzahl") +
      theme_minimal(base_size = 13) +
      scale_x_log10()
  })

  # 4) Top 10 Indikationen
  output$plot_top_indi <- renderPlot({
    fd <- filtered_data()
    req(fd)

    if (nrow(fd$indi) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Indikationen vorhanden") +
               theme_void())
    }

    top_indi <- fd$indi[!is.na(indi_pt) & indi_pt != "",
                       .N, by = indi_pt][order(-N)][1:10]

    ggplot(top_indi, aes(x = reorder(indi_pt, N), y = N)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(x = NULL, y = "Anzahl") +
      theme_minimal(base_size = 13)
  })

  # 5) Top 10 Reaktionen
  output$plot_top_reac <- renderPlot({
    fd <- filtered_data()
    req(fd)

    if (nrow(fd$reac) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Reaktionen vorhanden") +
               theme_void())
    }

    top_reac <- fd$reac[!is.na(pt) & pt != "",
                       .N, by = pt][order(-N)][1:10]

    ggplot(top_reac, aes(x = reorder(pt, N), y = N)) +
      geom_col(fill = "firebrick") +
      coord_flip() +
      labs(x = NULL, y = "Anzahl") +
      theme_minimal(base_size = 13)
  })

  # 6) Outcomes (nur abgeschlossene Therapien)
  output$plot_outcomes <- renderPlot({
    fd <- filtered_data()
    req(fd)

    if (nrow(fd$outc_last) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Outcome-Daten vorhanden") +
               theme_void())
    }

    # Nur Fälle mit abgeschlossener Therapie
    # (vereinfacht: end_dt vorhanden)
    abgeschlossen_ids <- fd$ther[!is.na(end_dt) & end_dt != "",
                                 unique(primaryid)]

    outc_dt <- fd$outc_last[primaryid %in% abgeschlossen_ids]
    outc_dt <- outc_dt[!is.na(outc_cod) & outc_cod != ""]

    if (nrow(outc_dt) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine abgeschlossenen Therapien") +
               theme_void())
    }

    # Lesbare Labels für Outcome-Codes
    outc_labels <- c(
      "DE" = "Tod",
      "LT" = "Lebensbedrohlich",
      "HO" = "Hospitalisierung",
      "DS" = "Behinderung",
      "CA" = "Geburtsfehler",
      "RI" = "Erforderte Intervention",
      "OT" = "Andere"
    )

    outc_summary <- outc_dt[, .N, by = outc_cod][order(-N)]
    outc_summary[, label := ifelse(outc_cod %in% names(outc_labels),
                                    outc_labels[outc_cod], outc_cod)]

    ggplot(outc_summary, aes(x = reorder(label, N), y = N)) +
      geom_col(fill = "purple4") +
      coord_flip() +
      labs(x = NULL, y = "Anzahl") +
      theme_minimal(base_size = 13)
  })

  # =============================================
  # ZUSATZ-STATISTIKEN
  # =============================================

  # 7) Zeit zwischen Therapiebeginn (start_dt) und Ereignisdatum (event_dt)
  #    -> nur für die Therapie, die zum gewählten Medikament gehört
  #    Join: DRUG.drug_seq  <->  THER.dsg_drug_seq  (innerhalb derselben primaryid)
  output$plot_zeit_diff <- renderPlot({
    fd <- filtered_data()
    req(fd)

    if (nrow(fd$ther) == 0 || nrow(fd$demo) == 0 || nrow(fd$drug_target) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Therapie- oder Ereignis-Daten vorhanden") +
               theme_void())
    }

    # Schritt 1: drug_seq des ausgewählten Medikaments pro Fall holen
    drug_keys <- fd$drug_target[!is.na(drug_seq) & drug_seq != "",
                                .(primaryid, drug_seq)]

    if (nrow(drug_keys) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine drug_seq-Verknüpfung möglich") +
               theme_void())
    }

    # Schritt 2: nur THER-Einträge nehmen, die zum gewählten Medikament gehören
    # (Verknüpfung über primaryid + dsg_drug_seq = drug_seq)
    ther_matched <- merge(
      fd$ther[!is.na(start_dt) & start_dt != "",
              .(primaryid, dsg_drug_seq, start_dt)],
      drug_keys,
      by.x = c("primaryid", "dsg_drug_seq"),
      by.y = c("primaryid", "drug_seq")
    )

    if (nrow(ther_matched) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine passenden Therapie-Einträge zum gewählten Medikament") +
               theme_void())
    }

    # Falls mehrere Therapien zum selben Medikament im gleichen Fall:
    # frühesten Start nehmen
    ther_first <- ther_matched[, .(start_dt = min(start_dt, na.rm = TRUE)),
                               by = primaryid]

    # Schritt 3: Ereignisdatum aus DEMO holen
    demo_event <- fd$demo[!is.na(event_dt) & event_dt != "",
                           .(primaryid, event_dt)]

    # Schritt 4: zusammenführen
    merged <- merge(demo_event, ther_first, by = "primaryid")

    if (nrow(merged) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Fälle mit Therapie- UND Ereignis-Datum") +
               theme_void())
    }

    # Schritt 5: Datum parsen (Format YYYYMMDD)
    merged[, event_date := parse_faers_date(event_dt)]
    merged[, start_date := parse_faers_date(start_dt)]
    merged[, diff_tage  := as.numeric(event_date - start_date)]

    n_alle    <- nrow(merged[!is.na(diff_tage)])
    n_negativ <- nrow(merged[!is.na(diff_tage) & diff_tage < 0])

    # Nur positive Werte: Ereignis NACH oder AM Therapiebeginn
    # bis maximal 10 Jahre (3650 Tage) - längere Werte sind meist Datenfehler
    plot_dt <- merged[!is.na(diff_tage) & diff_tage >= 0 & diff_tage <= 3650]

    if (nrow(plot_dt) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine gültigen Datumspaare im Bereich 0 bis 3650 Tage") +
               theme_void())
    }

    n_valid  <- nrow(plot_dt)
    median_t <- round(median(plot_dt$diff_tage), 0)

    ggplot(plot_dt, aes(x = diff_tage)) +
      geom_histogram(bins = 60, fill = "steelblue", colour = "white", alpha = 0.85) +
      geom_vline(xintercept = median_t,
                 colour = "darkgreen", linetype = "dotted", linewidth = 0.9) +
      annotate("text", x = median_t, y = Inf,
               label = paste0(" Median: ", median_t, " Tage"),
               hjust = 0, vjust = 1.8, colour = "darkgreen", size = 4) +
      labs(
        x        = "Tage zwischen Therapiebeginn und Ereignis",
        y        = "Anzahl Meldungen",
        subtitle = paste0(
          "n = ", format(n_valid, big.mark = "'"),
          " Fälle (verknüpft über drug_seq = dsg_drug_seq). ",
          "Ausgeschlossen: ", n_negativ,
          " Fälle mit negativer Differenz (vermutlich Eingabefehler)."
        )
      ) +
      theme_minimal(base_size = 13)
  })

  # 8) Altersverteilung nach Geschlecht
  output$plot_alter_geschlecht <- renderPlot({
    fd <- filtered_data()
    req(fd)

    if (nrow(fd$demo) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Demographic-Daten vorhanden") +
               theme_void())
    }

    demo_dt <- copy(fd$demo)

    # Geschlechtsspalte: heisst je nach Quartal "sex" oder "gndr_cod"
    if ("sex" %in% names(demo_dt)) {
      demo_dt[, geschlecht := sex]
    } else if ("gndr_cod" %in% names(demo_dt)) {
      demo_dt[, geschlecht := gndr_cod]
    } else {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine Geschlechtsspalte (sex/gndr_cod) gefunden") +
               theme_void())
    }

    # Alter parsen (ist als character gespeichert)
    demo_dt[, alter := suppressWarnings(as.numeric(age))]

    # Filter: nur M/F, gültiges Alter 0-120
    plot_dt <- demo_dt[geschlecht %in% c("M", "F") &
                         !is.na(alter) & alter > 0 & alter <= 120]

    if (nrow(plot_dt) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Keine gültigen Alters- und Geschlechtsdaten") +
               theme_void())
    }

    # Lesbare Labels
    plot_dt[, geschlecht_label := ifelse(geschlecht == "M", "Männlich", "Weiblich")]

    # Kennzahlen für den Untertitel
    n_m      <- sum(plot_dt$geschlecht == "M")
    n_f      <- sum(plot_dt$geschlecht == "F")
    median_m <- round(median(plot_dt$alter[plot_dt$geschlecht == "M"]), 1)
    median_f <- round(median(plot_dt$alter[plot_dt$geschlecht == "F"]), 1)

    ggplot(plot_dt, aes(x = alter, fill = geschlecht_label)) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.55,
                     colour = "white", linewidth = 0.2) +
      scale_fill_manual(values = c("Männlich" = "steelblue",
                                   "Weiblich" = "coral2")) +
      labs(
        x        = "Alter (Jahre)",
        y        = "Anzahl Meldungen",
        fill     = "Geschlecht",
        subtitle = paste0(
          "Männlich: n = ", format(n_m, big.mark = "'"),
          ", Median ", median_m, " Jahre  |  ",
          "Weiblich: n = ",  format(n_f, big.mark = "'"),
          ", Median ", median_f, " Jahre"
        )
      ) +
      theme_minimal(base_size = 13)
  })

}

# =============================================================
# 6) APP STARTEN
# =============================================================

shinyApp(ui = ui, server = server)
