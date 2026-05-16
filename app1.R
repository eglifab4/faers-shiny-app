# =============================================================
# app.R - FAERS Adverse Event Explorer (Shiny App)
# -------------------------------------------------------------
# Praktikum 3 - PM2 ZHAW 2026
#
# Voraussetzung:
#   In app_data/ liegen DEMO.rds, DRUG.rds, INDI.rds,
#   OUTC.rds, REAC.rds, RPSR.rds, THER.rds (durch
#   Daten_fuer_Shiny.R erzeugt).
#
# Starten:
#   In RStudio: oben rechts "Run App" klicken
#   Oder Console:  shiny::runApp("app.R")
# =============================================================

library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(here)

# =============================================================
# 1) DATEN LADEN (einmal beim App-Start)
# =============================================================
message("App startet... Daten werden geladen...")

app_data_dir <- here("app_data")

demo <- readRDS(file.path(app_data_dir, "DEMO.rds"))
drug <- readRDS(file.path(app_data_dir, "DRUG.rds"))
indi <- readRDS(file.path(app_data_dir, "INDI.rds"))
outc <- readRDS(file.path(app_data_dir, "OUTC.rds"))
reac <- readRDS(file.path(app_data_dir, "REAC.rds"))
ther <- readRDS(file.path(app_data_dir, "THER.rds"))

# Sicherstellen, dass alle data.tables sind
setDT(demo); setDT(drug); setDT(indi); setDT(outc); setDT(reac); setDT(ther)

message("Daten geladen: ",
        format(nrow(demo), big.mark = "'"), " DEMO, ",
        format(nrow(drug), big.mark = "'"), " DRUG, ",
        format(nrow(reac), big.mark = "'"), " REAC")

# =============================================================
# 2) VORBERECHNUNGEN
# =============================================================

# Top-100 Medikamente für schnelles Auswahlmenü
top_drugs <- drug[!is.na(drugname) & drugname != "",
                  .N, by = drugname][order(-N)][1:200]

# Top-10 für Startanzeige
top10_drugs <- top_drugs[1:10]

# =============================================================
# 3) HELFER-FUNKTIONEN
# =============================================================

# Hauptfunktion: für ein Medikament alle Daten zusammenstellen
# (entspricht Aufgabe 2d aus dem PDF)
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
    # age_cod gibt die Einheit an (YR, MON, etc.) - wir nehmen vereinfacht alles als Jahre
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
  outc_seq <- outc[primaryid %in% ids_filtered]
  outc_last <- outc_seq[, .SD[.N], by = primaryid]

  # Therapy
  ther_seq <- ther[primaryid %in% ids_filtered]

  # Indication
  indi_seq <- indi[primaryid %in% ids_filtered]

  # Reactions
  reac_seq <- reac[primaryid %in% ids_filtered]

  list(
    n_faelle    = length(ids_filtered),
    drug_target = drug[primaryid %in% ids_filtered &
                         grepl(drug_input, drugname, ignore.case = TRUE)],
    drug_full   = drug_full_seq,
    demo        = demo_seq,
    outc_last   = outc_last,
    ther        = ther_seq,
    indi        = indi_seq,
    reac        = reac_seq
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
                                     "Weiblich"   = "F",
                                     "Unbekannt"  = "UNK"),
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

                   h3("1. Anzahl Meldungen pro Quartal nach ROLE_COD"),
                   plotOutput("plot_quartal_role", height = "350px"),

                   br(),
                   h3("2. Top mit-vorkommende Substanzen (in den anderen ROLE_COD)"),
                   p(em("Welche anderen Medikamente kommen am häufigsten zusammen mit dem ausgewählten Medikament vor?")),
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
                   p(em("Nur Fälle mit abgeschlossener Therapie")),
                   plotOutput("plot_outcomes", height = "350px")
                 )
        ),

        # ------------------ TAB 3: ZUSATZ-STATISTIKEN (PLATZHALTER) ------------------
        tabPanel("Zusatz-Statistiken",
                 br(),
                 div(class = "alert alert-warning",
                     "Diese 2 zusätzlichen Statistiken werden vom Team noch festgelegt.",
                     br(),
                     "Vorschläge: Top Reporter Countries, Trend über Zeit, Altersverteilung nach Geschlecht, Schwere-Outcome-Quote.")
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
                   "Wechsle dann in den Tab 'Statistiken'."),

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
                           "Schwere der Folgen (Tod, Hospitalisierung, etc.).")
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

  # ----- Top-10 Tabelle -----
  output$top10_table <- DT::renderDataTable({
    DT::datatable(
      top10_drugs,
      colnames = c("Medikament", "Anzahl Meldungen"),
      selection = "single",
      options = list(pageLength = 10, dom = "t"),
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
           fill = "ROLE_COD",
           title = paste0("Meldungen pro Quartal: ", input$drug_choice)) +
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
      colnames = c("ROLE_COD", "Substanz", "Anzahl"),
      options = list(pageLength = 10),
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
      labs(x = "Therapie-Dauer (Tage)", y = "Anzahl",
           title = "Verteilung der Therapielänge") +
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
      labs(x = NULL, y = "Anzahl",
           title = "Top 10 Indikationen") +
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
      labs(x = NULL, y = "Anzahl",
           title = "Top 10 Reaktionen") +
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
      labs(x = NULL, y = "Anzahl",
           title = "Verteilung der Outcomes (nur abgeschlossene Therapien)") +
      theme_minimal(base_size = 13)
  })
}

# =============================================================
# 6) APP STARTEN
# =============================================================

shinyApp(ui = ui, server = server)
