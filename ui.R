library(shiny)
library(shinyBS)
library(RMySQL)
# library(bigrf)

shinyUI(
  tabsetPanel(
    tabPanel("Import",
      fluidPage(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
        fluidRow(
          column(12, h1("Willkommen bei Gaikan"))),
        fluidRow(
            column(12, h6("Gaikan hilft Ihnen, die Reservierungen des BERNINA-Express auszuwerten und Buchungsentscheide zu treffen")),
            column(12, h6("Dazu laden Sie bitte einen aktuellen Datenstand hoch oder wählen Sie einen alten Datenstand aus, der für die weiteren Analysen verwendet werden soll"))),
        fluidRow(
          column(12,
            #selectInput("ddata", "Bisher hochgeladene Daten", choices = getIntervals()),
            uiOutput("SaveData"),
            fileInput('file1', 'Import', accept=c('application/vnd.ms-excel',
                                                  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                  '.xls',
                                                  '.xlsx'))
          ))
      )
    ),
    tabPanel("Data Visualization",
     fluidPage(
       tags$head(
         tags$link(rel="stylesheet", type="text/css", href="styles.css")),
       fluidRow(
         column(12, h1("Willkommen bei GAIKAN"))),
       fluidRow(
         column(12, "Bitte überprüfen Sie die Daten auf Korrektheit")),
       fluidRow(
         column(1, actionButton("saveData", "Speichern")),
         column(3, bsAlert(anchorId="savingDataAlert"))
         ),
       
       fluidRow(
         column(12, bsAlert("savingDataAlert"))),
       fluidRow(
        column(12, DT::dataTableOutput('contents')))
     )
    ),
    tabPanel("Wahrscheinlichkeiten",
     fluidPage(
       tags$head(
         tags$link(rel="stylesheet", type="text/css", href="styles.css")),
       fluidRow(
         column(12, h1("Willkommen bei GAIKAN")))),
       #column(12, selectInput("dstand", "Gewählter Datenstand", choices = c("")))), #default: vorher ausgewÃ¤hlt
       fluidRow( 
          tabsetPanel(
            tabPanel("Reservierungsplanung",
              sidebarLayout(
                sidebarPanel(
                  tags$b("Zug auswählen..."),
                  uiOutput("ZugNr"),
                  #selectInput("zugnr", "Zug Nr.", choices=c("")),
                  dateInput("datres", "Datum", format = "dd.mm.yyyy", language="de"),
                  tags$b("Reservierungsanfrage prüfen..."),
                  selectInput("anbieter", "Anbieter", choices=c("")),
                  #selectInput("land", "Land", choices=c("")),
                  textInput("plaetze",  "Plätze", "1"),
                  selectInput("von", "Von", choices=c("")),
                  selectInput("bis", "Bis", choices=c("")),
                  radioButtons('klasse', 'Klasse', c('1'='1', '2'='2'), selected='2'),
                  actionButton("pruefen", "Prüfen")               
                ),
                mainPanel(
                  htmlOutput("resAnfrage"),
                  DT::dataTableOutput("reservations")
                  )
            )),
            tabPanel("Reporting & Statistiken",
              sidebarLayout(
            sidebarPanel(
              selectInput("auswertung", "Auswertung", choices=c("Statistik", "Ausfallrate", "Auslastung")),
              tags$b("Optionen"),
              selectInput("forvar", "Für Variable", choices=c("Business.Partner", "Dossier.Status", "Einzel", "Key.Account", "Land", "Last.second.Anbieter", "Reisestart", "Reisewochentag", "Reiseziel", "Verkaufskanal", "Verkaufsort", "Wagentyp", "Zug.Nr.", "Zugkategorie", selected="Business.Partner"), selected="Business.Partner"),
              selectInput("forfactor", "Faktor", choices=c("")),
              selectInput("sorted", "Sortiert nach", choices=c("")),
              dateInput("datvon", "Datum von", format = "dd.mm.yyyy", language="de"),
              dateInput("datbis", "Datum bis", format = "dd.mm.yyyy", language="de"),
              uiOutput("ZugNrStat"),
              selectInput("streckevon", "Strecke von", choices=c("")),
              selectInput("streckebis", "Strecke bis", choices=c("")),
              selectInput("horizont", "Zeithorizont Vorhersage", choices=as.character(1:6)),
              radioButtons('ausklasse', 'Klasse', c('1'='1', '2'='2', 'all'='alle'), selected='alle'),
              radioButtons('gcontingent', 'Kontingent', c('ja'='ja', 'nein'='nein'), selected='ja'),
              radioButtons('reisende', 'Reisende', c('Gruppe'='Gruppe', 'Einzel'='Einzel', 'all'='Alle'), selected='Alle'),
              actionButton("ok", "OK")
              ),
            mainPanel(
              plotOutput("reporting")
              ))),
            tabPanel("Optionen",
             sidebarLayout(
               sidebarPanel(
                 textInput("greenregion",  "Grüner Bereich ab", "80"),
                 textInput("redregion",  "Roter Bereich bis", "70"),
                 tags$br(),
                 tags$html("% Verfügungs-Sicherheit")
                 ),
               mainPanel(
                 tags$b("Optionen"),
                 tags$b("Zug-Gestaltung"),
                 uiOutput("ZugNrOption"),
                 selectInput("jahropt", "Jahr", choices=c("")),
                 selectInput("saison", "Saison", c("Sommer", "Winter"), selected="Sommer"),
                 tags$br(),
                 tags$b("Erste Klasse"),
                 textInput("klasse1einzel", "Anzahl Plätze für Einzelreisende", c("")),
                 textInput("klasse1gruppe", "Anzahl Plätze für Gruppenreisende", c("")),
                 textInput("klasse1kontingent", "Anzahl Plätze im Kontingent", c("")),
                 tags$br(),
                 tags$b("Zweite Klasse"),
                 textInput("klasse2einzel", "Anzahl Plätze für Einzelreisende", c("")),
                 textInput("klasse2gruppe", "Anzahl Plätze für Gruppenreisende", c("")),
                 textInput("klasse2kontingent", "Anzahl Plätze im Kontingent", c("")),
                 actionButton("savezug", "Speichern"),
                 tags$br(),
                 textOutput("Save")
                 )
             ))
          ))
      )
    )
)