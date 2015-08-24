library(DBI)
library(RMySQL)

getPlaces <- function(trainnr, class, contingent, einzel, jahr, saison){
  #trainnr: train number/all
  #class: wagon-class, 1/2/all
  #contingent: True/False, only places in contingent or not
  #einzel: Einzel/Gruppe/all 
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  #Gestaltung der Variable
  var <- c()
  if(class=="alle") class <- c("1", "2")
  var <- paste0("Klasse", class)
  if(contingent) var <- paste0(var, "Kontingent")
  else{
    print(paste("einzel:", einzel))
    if(einzel=="Alle") einzel <- c("Einzel", "Gruppe")
    einzel <- rep(einzel, each=length(var))
    var <- paste0(var, einzel)
  }
  var <- paste0(var, collapse="+")
  query <- paste0("SELECT SUM(", var, ") FROM Trains WHERE Jahr=", jahr, " AND Saison='", saison, "'")
  if(trainnr!="all") query <- paste0(query, " AND ZugNr=", trainnr)
  query <- paste0(query, ";")
  #print(query)
  res <- dbSendQuery(connexion, query)
  anzahlPlaetze <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(connexion)
  return(as.numeric(anzahlPlaetze))
}

#TODO: test
setPlaces <- function(zugnr, jahr, saison, klasse1Einzel, klasse1gruppe, klasse1kontingent, klasse2Einzel, klasse2gruppe, klasse2kontingent){  
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")  
  query <- paste0("INSERT INTO Trains VALUES (", zugnr, ", ", jahr, ", '", saison, "', ",  klasse1Einzel, ", ", klasse1gruppe, ", ", klasse1kontingent, ", ", klasse2Einzel, ", ", klasse2gruppe, ", ", klasse2kontingent,");")
  print(query)
  dbSendQuery(connexion, query)
  dbDisconnect(connexion)
}

getTrainNumbers <- function(datres=""){ #OK
  #datres: date reservation
  datres <- as.character(datres)
  print(paste("datres:", datres))
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  query <- "SELECT ZugNr FROM Trains"
  if(!is.null(datres) & datres != ""){
    
    period <- getSaison(datres)
    query <- paste0(query, " WHERE Jahr=", period$jahr, " AND Saison='", period$saison, "'")
    print(paste("query:", query))
  }
  query <- paste0(query, ";")
  res <- dbGetQuery(connexion, query)
  dbDisconnect(connexion)
  return(unique(res$ZugNr))
}

saveData <- function(dataset){#TODO: test
#   library(bigrf)
#   library(doMC)
#   registerDoMC(cores=4)
  
  print("arrivée dans saveData")

  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  minDate <- min(dataset$Reisedatum)
  maxDate <- max(dataset$Reisedatum)
  
  print(paste("minDate:", minDate, ", maxDate:", maxDate))
  #TODO: check query
  query <- paste0("SELECT COUNT(*) FROM Reservations WHERE Reisedatum >='", minDate, "' AND Reisedatum <= '", maxDate, "';")
  res <- dbSendQuery(connexion, query)
  anzahl <- dbFetch(res)
  anzahl <- as.numeric(anzahl)
  print(paste("debug, anzahl =", anzahl))
  if(anzahl>=0){
    query <- paste0("DELETE FROM Reservations WHERE Reisedatum >='", minDate, "' AND Reisedatum <= '", maxDate, "';")
    dbSendQuery(connexion, query)
    names(dataset) <- gsub("[:.:]", "", names(dataset))
    dbWriteTable(connexion, "Reservations", dataset, append=T, row.names=F)
  }
  
  # data <- getData()
  
  print(paste0("nrow(data) before = ", nrow(data)))
#   #local assignments
#   data <- rbind(data, dataset)
#   print(paste0("nrow(data) just after = ", nrow(data)))
#   forest0 <- bigrfc(data, as.factor(data$Dossier.Status), ntree=30L, varselect=as.integer(c(1, 2, 3, 7, 8, 16, 17)), cachepath=NULL)
  #global assignments

  # data <- as.data.frame(rbind(data, dataset))
  print(paste0("nrow(data) just after = ", nrow(data)))
#   forest <- bigrfc(data, as.factor(data$Dossier.Status), ntree=30L, varselect=as.integer(c(1, 2, 3, 7, 8, 16, 17)), cachepath=NULL)
#   print("fores0 calculated")
  print(paste("nrow(data) even after =", nrow(data)))
  print(head(as.factor(data$Dossier.Status)))
  print(paste("length(data$Dossier.Status) even after =", length(data$Dossier.Status)))
  print(head(data))

#   forest0 <- bigrfc(data, as.factor(data$Dossier.Status), ntree=30L, varselect=as.integer(c(1, 2, 3, 7, 8, 16, 17)), cachepath=NULL)
#   print("forest0")
#   print(forest0)
#   predictions <- predict(forest0, data, as.factor(data$Dossier.Status))
#   #predictions <- predict(forest0, data, data$Dossier.Status)
#   print("predictions calculated")
#   predictions <- cprob(predictions)
#   print("cprob calculated")
  
  # data <<- data
#   forest0 <<- forest0
#   predictions <<- predictions
  
  print(paste0("nrow(data) after = ", nrow(data)))
  anz <- nrow(subset(dataset, Dossier.Status="Belegt"))
  print(paste("Anzahl belegte Reservationen: ", anz))
  query <- paste0("INSERT IGNORE INTO Intervals(minDate, maxDate) VALUES('",minDate,"', '", maxDate, "');")
  dbSendQuery(connexion, query)
  dbDisconnect(connexion)
}

getIntervals <- function(){
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  intervals <- dbGetQuery(connexion, "SELECT * FROM Intervals;")
  res <- paste("Von", intervals$minDate,"bis", intervals$maxDate)
  res <- c(res)
  dbDisconnect(connexion)
  return(res)
}

getData <- function(datvon=NULL, datbis=NULL, trainnr=NULL, fromPlace=NULL, toPlace=NULL, einzel=NULL){
  print(paste("getData, einzel=", einzel))
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  query <- "SELECT * FROM Reservations"
  if(!(is.null(datvon) | is.null(datbis))) query <- paste0(query, " WHERE Reisedatum >= '", datvon, "' AND Reisedatum <= '", datbis, "'")
  if(!is.null(trainnr) & trainnr!="" & trainnr!="Alle") query <- paste0(query, " AND ZugNr=", trainnr)
  if(!is.null(fromPlace) & fromPlace!="") query <- paste0(query, " AND Reisestart='", fromPlace, "'")
  if(!is.null(toPlace) & toPlace!="") query <- paste0(query, " AND Reiseziel='", toPlace, "'")
  if(!is.null(einzel) & einzel!="" & einzel!="Alle") query <- paste0(query, " AND Einzel='", einzel, "'")
  query <- paste0(query, ";")
  print(query)
  data <- dbGetQuery(connexion, query)
  names(data) <- c("Reisedatum", "Reisewochentag", "Zug.Nr.", "Zugkategorie", "Einzel", "Wagentyp", "Passagiere.2..Kl.", "Passagiere.1..Kl.", "Dossier.Status", "Verkaufsort", "Verkaufskanal", "Key.Account", "Last.second.Anbieter", "Land", "Business.Partner", "Reisestart", "Reiseziel")
  dbDisconnect(connexion)
  data
}

getZugNr <- function(){
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  data <- dbGetQuery(connexion, "SELECT ZugNr FROM Trains;")
  data <- unique(data$ZugNr)
  dbDisconnect(connexion)
  data
}

getStations <- function(zugnr, richtung=""){
  if(!(is.null(zugnr) | zugnr=="")){
    var <- "Reisestart, Reiseziel"
    if(var=="von") var <- "Reisestart"
    if(var=="bis") var <- "Reiseziel"
    connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
    query <- paste("SELECT", var, "FROM Reservations")
    if(zugnr!="Alle") query <- paste(query, "WHERE ZugNr=", zugnr)
    query <- paste0(query, ";")
    data <- dbGetQuery(connexion, query)
    data <- sort(unique(c(data$Reisestart, data$Reiseziel)))
    dbDisconnect(connexion)
  }
  else data <- c("")
  return(data)
}

getCapacity <- function(trainno, year, saison){
  print(paste0("trainno=", trainno, ", year=", year, ", saison=", saison))
  if(is.null(trainno) | is.null(year) | is.null(saison) | trainno=="" | year=="" | saison==""){
    df <- data.frame(matrix(c("", "", "", "", "", ""), 1, 6, dimnames=list(c(), c("Klasse1Einzel, Klasse1Gruppe, Klasse1Kontingent, Klasse2Einzel, Klasse2Gruppe, Klasse2Kontingent"))))
    return(df)
  }
  
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  
  query <- paste0("SELECT  Klasse1Einzel, Klasse1Gruppe, Klasse1Kontingent, Klasse2Einzel, Klasse2Gruppe, Klasse2Kontingent FROM Trains WHERE ZugNr=", trainno, " AND Jahr=", year, " AND Saison='", saison, "';")
  #print(query)
  suppressWarnings(
    data <- dbGetQuery(connexion, query))
  dbDisconnect(connexion)
  return(data)
}

getYears <- function(){
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  query <- "SELECT DISTINCT Jahr FROM Trains ORDER BY Jahr DESC;"
  data <- dbGetQuery(connexion, query)
  dbDisconnect(connexion)
  return(data$Jahr)
}

getFactors <- function(var){
  if(is.na(var) | is.null(var) | var=="") return("")
  var <- gsub("[:.:]", "", var)
  connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
  query <- paste("SELECT DISTINCT", var, "FROM Reservations ORDER BY", var, "ASC;")
  print("getFactors, query:")
  print(query)
  values <- dbGetQuery(connexion, query)
  values <- c(values)
  dbDisconnect(connexion)
  return(values)
}

#falls alle 16 connections besetzt:
# cons <- dbListConnections(MySQL())
# for(con in cons) dbDisconnect(con)