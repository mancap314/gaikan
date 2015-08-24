library(shiny)
library(shinyBS)
library(plyr)
library(xlsx)
library(gdata)
#library(XLConnect)
library(tseries)
library(DBI)
library(RMySQL)

library(stringr)
library(forecast)
library(xts)

#source('vorskript.R')

options(shiny.maxRequestSize=180*1024^2)
options(encoding = 'UTF-8')
options(java.parameters = "-Xms1g -Xmx1g")
options(warn=1)
#options(shiny.error=traceback)
#options(warn=2, error=browser, shiny.error=browser)
# con <- gzfile(description = "/home/rstudio/forest.rds")
# forest0 <- readLines(con, n = 10)
# close(con)
#data <- getData()
#forest0 <- mForest(data)


shinyServer(function(input, output, session){
  
  con <- file("test.log")
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  
  output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
#     inFile <- input$file1
#     
#     if (is.null(inFile)) return(NULL)
#     
#     dataset <- read.xlsx2(inFile$datapath, sheetName = "Sheet", encoding="UTF-8")
#     dataset <- read.csv(dataset, header=T, sep=",", quote="\"", check.names=F, fileEncoding = "UTF-8")
#     dataset <- processData(dataset)
#     dataset
      dataset()
  },options = list(bSortClasses = TRUE))
  
  
  dataset <- reactive({
    infile = input$file1  
    if (is.null(infile))
      return(NULL)
    
    dataset <- read.xlsx2(infile$datapath, sheetName = "Sheet", encoding="UTF-8", mode="wb")
    
    dataset <- processData(dataset)
    dataset
  })
  
  
  output$dataSaved <- renderText({
    input$saveData
    
    isolate({
      if(input$saveData != 0){
        dataset <- dataset()
        saveData(dataset)
        }
    })
    return("gespeichert")
  })
  
  output$resAnfrage <- renderUI({
    input$pruefen
    
    options(shiny.error=traceback)
    
    isolate({
      if(input$pruefen != 0){
        res <- resProba(input$datres, input$zugnr, input$anbieter, input$land, input$plaetze, input$von, input$bis, input$klasse)
        #res <- mReservation(input$datres, input$zugnr, input$anbieter, input$land, input$plaetze, input$von, input$bis, input$klasse)
        totalPlaces <- res$totalPlaces
        totalPlaces <- paste0("Anzahl Plätze total: ", totalPlaces)
        reservedPlaces <- res$reservedPlaces
        reservedPlaces <- paste0("Anzahl Plätze belegt: ", reservedPlaces)
        pAusfall <- res$pAusfall
        pAusfall <- paste0("storniert wird, liegt bei: ", pAusfall, "%")
        pSuccessN <- as.numeric(res$pSuccess)
        res <- NULL
        #pSuccessN <- res$pSuccess
        pSuccess <- paste0("genügend Plätze sind, liegt bei: ", pSuccessN, "%")
        
        redlimit <- as.numeric(input$redregion)
        greenlimit <- as.numeric(input$greenregion)
        ampelf <- "gelb"
        if(pSuccessN<=redlimit) ampelf <- "rot"
        if(pSuccessN>=greenlimit) ampelf <- "gruen"
        ampelsrc <- paste0(ampelf, ".png")
        
        
        l <- tags$ul(
          tags$li(paste0(input$plaetze, " Plätze in der ", input$klasse, ". Klasse von ", input$von, " nach ", input$bis, " am ", input$datres)),
          tags$li(paste("Zug-Nummer: ", input$zugnr)),
          tags$li(paste("Anbieter: ", input$anbieter))
          )        
        h <- list()      
        h <- list(h, tags$html(totalPlaces))
        h <- list(h, tags$hr())
        h <- list(h, tags$html(reservedPlaces))
        h <- list(h, tags$hr())
        h <- list(h, tags$html("Das Risiko, dass diese Reservation"))
        h <- list(h, l)
        h <- list(h, tags$html(pAusfall))
        h <- list(h, tags$hr())
        h <- list(h, tags$img(src=ampelsrc, class="ampel"))
        h <- list(h, tags$html("Die Sicherheit, dass für diese Reservation"))
        h <- list(h, l)
        h <- list(h, tags$html(pSuccess))
        h <- list(h, tags$hr())
        
        h <- tagList(h)
        h
      }
    })
  })
  
  
  output$reservations <- DT::renderDataTable({
    input$pruefen
    
    isolate({
      if(input$pruefen != 0){
        #inFile <- input$file1
        #dataset <- read.csv(inFile$datapath, header=T, sep=",", quote="\"", check.names=F, fileEncoding = "latin1")
        zugnr <- input$zugnr
        datres <- input$datres
        anbieter <- input$anbieter
        plaetze <- input$plaetze
        von <- input$von
        bis <- input$bis
        klasse <- input$klasse
        
        #forest <- unserialize(connection=file("forest.rds", "r"))
        #predictions <- bigrf::predict(forest0, data, as.factor(data$Dossier.Status))
        #predictions <- mPredictions(dataset)
        #predictions <- cprob(predictions)
        tr <- tableReservations(predictions, datres, zugnr, plaetze, klasse)
        tr
      }
    })
  }, options = list(bSortClasses = TRUE))
  
  output$reporting <- renderPlot({
    input$ok
    
    isolate({
      if(input$ok != 0){
        auswertung <- input$auswertung
        forvar <- input$forvar
        forfactor <- input$forfactor
        sorted <- input$sorted
        datvon <- input$datvon
        datbis <- input$datbis
        trainnr <- input$zugnrstat
        streckevon <- input$streckevon
        streckebis <- input$streckebis
        klasse <- input$ausklasse
        horizont <- as.numeric(input$horizont)
        einzel <- input$reisende
        if(input$reisende=="Einzel") einzel <- "ja"
        #data <- subset(data, Reisedatum>=datvon & Reisedatum<= datbis)
      
        if(auswertung=="Statistik"){
          stat <- ausfallrate(forvar, forfactor, datvon, datbis, sorted, streckevon, streckebis, einzel=einzel, byPassenger=T)
          plot <- graphStats(stat, "", forvar, forfactor, datvon, datbis)
        }
        if(auswertung=="Ausfallrate"){
          plot <- prevision(trainnr, datvon, datbis, fromPlace="", toPlace="", einzel=einzel, to=horizont)
        }
        
        if(auswertung=="Auslastung"){
          kontingent <- F
          if(input$gcontingent=="ja") kontingent <- T
          plot <- auslastung(datvon, datbis, trainnr, kontingent, einzel, klasse, to=horizont)
        }
        plot
      }
  })
  })
    
  output$Save <- renderText({
    input$savezug
    
    isolate({
      if(input$savezug != 0){
        setPlaces(input$zugnropt, input$jahropt, input$saison, input$klasse1einzel, input$klasse1gruppe, input$klasse1kontingent, input$klasse2einzel, input$klasse2gruppe, input$klasse2kontingent)
        }
      })
    return("gespeichert")
    })
  
  
  output$ZugNr <- renderUI({
    selectInput("zugnr", "Zug Nr.", choices=c(getTrainNumbers(input$datres)))
  })
  
  output$ZugNrStat <- renderUI({
    selectInput("zugnrstat", "Zug Nr.", choices=c("Alle", getTrainNumbers()))
  })
  
  output$ZugNrOption <- renderUI({
    selectInput("zugnroption", "Zug Nr.", choices=c(getTrainNumbers()))
  })
  
  observe({
    namen <- c("Reisedatum", "Reisewochentag", "Zug.Nr.", "Zugkategorie", "Einzel", "Reisestrecke", "Wagentyp", "Passagiere.2..Kl.", "Passagiere.1..Kl.", "Dossier.Status", "Verkaufsort", "Verkaufskanal", "Key.Account", "Last.second.Anbieter", "Land", "Business.Partner")
    #updateSelectInput(session, "ddata", "Bisher hochgeladene Daten", choices = getIntervals())
    updateSelectInput(session, "anbieter", "Anbieter", choices=c("Anderer...", getFactors("Business.Partner")))
    updateSelectInput(session, "land", "Land", choices=getFactors("Land"))
    if(!(is.null(input$zugnr))) updateSelectInput(session, "von", "Von", choices=getStations(input$zugnr, "von"))
    if(!(is.null(input$zugnr))) updateSelectInput(session, "bis", "Bis", choices=getStations(input$zugnr, "bis"))
    if(!(is.null(input$forvar))) updateSelectInput(session, "forfactor", "Faktor", choices=getFactors(input$forvar))
    if(!(is.null(input$forvar))) updateSelectInput(session, "sorted", "Sortiert nach", choices=c(namen[!namen %in% c(input$forvar, "Reisedatum", "Dossier.Status")]))
    if(!is.null(input$zugnrstat)) updateSelectInput(session, "streckevon", "Strecke von", choices=c("", getStations(input$zugnrstat, "von")))
    if(!(is.null(input$zugnrstat))) updateSelectInput(session, "streckebis", "Bis", choices=c("", getStations(input$zugnrstat, "bis")))
  
    updateSelectInput(session, "jahropt", "Jahr", choices=getYears())
    
    if(!(is.null(input$zugnroption) | is.null(input$jahropt) | is.null(input$saison))){
      capacity <- getCapacity(input$zugnroption, input$jahropt, input$saison)
      print("capacity")
      print(capacity)
      
      updateTextInput(session, "klasse1einzel", "Anzahl Plätze für Einzelreisende", c(capacity$Klasse1Einzel))
      updateTextInput(session, "klasse1gruppe", "Anzahl Plätze für Gruppenreisende", c(capacity$Klasse1Gruppe))
      updateTextInput(session, "klasse1kontingent", "Anzahl Plätze im Kontingent", c(capacity$Klasse1Kontingent))
      
      updateTextInput(session, "klasse2einzel", "Anzahl Plätze für Einzelreisende", c(capacity$Klasse2Einzel))
      updateTextInput(session, "klasse2gruppe", "Anzahl Plätze für Gruppenreisende", c(capacity$Klasse2Gruppe))
      updateTextInput(session, "klasse2kontingent", "Anzahl Plätze im Kontingent", c(capacity$Klasse2Kontingent))
    }
    updateSelectInput(session, "ddata", "Bisher hochgeladene Daten", choices = getIntervals())
    #updateSelectInput(session, "zugnropt", "Zug-Nr.", choices=c("", getTrainNumbers()))
    #updateSelectInput(session, "zugnraus", "Zug Nr. (Auslastung)", choices=c("all", getZugNr()))
    })
})