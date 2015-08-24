#data processing
processData <- function(data){
  print("data gets processed")
  #data <- data[,c("Reisedatum", "Reisewochentag", "Zug Nr.", "Zugkategorie", "Einzel", "Reisestrecke", "Wagentyp", "Passagiere 2. Kl.", "Passagiere 1. Kl.", "Dossier Status", "Verkaufsort", "Verkaufskanal", "Key Account", "Last second Anbieter", "Land", "Business Partner")]
  data <- data[,c("Reisedatum", "Reisewochentag", "Zug.Nr.", "Zugkategorie", "Einzel", "Reisestrecke", "Wagentyp", "Passagiere.2..Kl.", "Passagiere.1..Kl.", "Dossier.Status", "Verkaufsort", "Verkaufskanal", "Key.Account", "Last.second.Anbieter", "Land", "Business.Partner")]
  names(data) <- c("Reisedatum", "Reisewochentag", "Zug.Nr.", "Zugkategorie", "Einzel", "Reisestrecke", "Wagentyp", "Passagiere.2..Kl.", "Passagiere.1..Kl.", "Dossier.Status", "Verkaufsort", "Verkaufskanal", "Key.Account", "Last.second.Anbieter", "Land", "Business.Partner")
  data <- subset(data, !is.na(Zug.Nr.))
  #delete string "Gesamt" in values
  data$Passagiere.2..Kl. <- gsub("Gesamt", "", data$Passagiere.2..Kl.)
  data$Passagiere.1..Kl. <- gsub("Gesamt", "", data$Passagiere.1..Kl.)
  #make it numeric
  data$Passagiere.2..Kl. <- as.numeric(as.character(data$Passagiere.2..Kl.))
  data$Passagiere.1..Kl. <- as.numeric(as.character(data$Passagiere.1..Kl.))
  #replace NA by 0
  data$Passagiere.2..Kl.[is.na(data$Passagiere.2..Kl.)] <- 0
  data$Passagiere.1..Kl.[is.na(data$Passagiere.1..Kl.)] <- 0
  #suppress status code
  data$Dossier.Status <- as.character(data$Dossier.Status)
  data$Dossier.Status <- gsub("^\\d{2}\\s", "", data$Dossier.Status)
  data$Dossier.Status <- as.factor(data$Dossier.Status)
  data$Reisewochentag <- ordered(data$Reisewochentag, levels=c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"))
  
  #split Reisestrecke in start and ziel
  
  splitting <- str_split(data$Reisestrecke, " - ")
  Reise.start <- rapply(splitting, function(x){return(x[1])})
  Reise.ziel <- rapply(splitting, function(x){return(x[2])})
  #drop variable Reisestrecke
  data <- data[,!(names(data) %in% c("Reisestrecke"))]
  #add start and ziel
  data$Reisestart <- as.factor(Reise.start)
  data$Reiseziel <- as.factor(Reise.ziel)
  
  #suppress entries with no Reiseziel
  data <- subset(data, !is.na(Reiseziel))
  data <- subset(data, !is.na(Reisedatum))
  
  print("unique(data$Dossier.Status)")
  print(unique(data$Dossier.Status))
  tochange <- data$Dossier.Status %in% c("Fahrplanänderung", "Bestellt", "Erfolgreich abgeschlossen", "Offen")
  print("tochange")
  print(tochange[1:30])
  data$Dossier.Status <- as.character(data$Dossier.Status)
  data$Dossier.Status[tochange] <- "Belegt"
  print("data$Dossier.Status")
  print(data$Dossier.Status[1:30])
  
  data <- subset(data, Dossier.Status %in% c("Belegt", "Storniert"))
  data$Dossier.Status <- factor(data$Dossier.Status, levels=c("Belegt", "Storniert"))
  data$Reisedatum <- as.Date(as.numeric(as.character(data$Reisedatum)), origin = "1899-12-30")
  data
}

formatData <- function(dataset){
  dataset$Zug.Nr. <- as.factor(dataset$Zug.Nr.)
  dataset$Zugkategorie <- as.factor(dataset$Zugkategorie)
  dataset$Einzel <- as.factor(dataset$Einzel)
  dataset$Wagentyp <- as.factor(dataset$Wagentyp)
  dataset$Land <- as.factor(dataset$Land)
  dataset$Verkaufsort<- as.factor(dataset$Verkaufsort)
  dataset$Verkaufskanal <- as.factor(dataset$Verkaufskanal)
  dataset$Key.Account <- as.factor(dataset$Key.Account)
  dataset$Last.second.Anbieter <- as.factor(dataset$Last.second.Anbieter)
  dataset$Business.Partner <- as.factor(dataset$Business.Partner)
  
  dataset$Passagiere.2..Kl. <- as.numeric(as.character(dataset$Passagiere.2..Kl.))
  dataset$Passagiere.1..Kl. <- as.numeric(as.character(dataset$Passagiere.1..Kl.))
  dataset$Reisestart <- as.factor(dataset$Reisestart)
  dataset$Reiseziel <- as.factor(dataset$Reiseziel)
  dataset$Reisedatum <- as.POSIXct(dataset$Reisedatum, format="%Y-%m-%d")
  dataset$Dossier.Status <- as.factor(as.character(dataset$Dossier.Status))
  dataset$Reisewochentag <- ordered(dataset$Reisewochentag, levels=c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"))
  dataset
}

#how many NAs in a column?
hmna <- function(x){
  length(x[is.na(x)])
}
#how many unique values?
hmunique <- function(x){
  length(unique(x))
}

isfactor <- function(x){
  is.factor(x)
}

ascharacter <- function(x){
  x <- as.character(x)
}

asfactor <- function(x){
  x <- factor(x, levels=unique(x))
}

#analyse with bigrf
#runs only with Unix
# mForest <- function(data){
#   library(bigrf)
#   dat <- subset(data, Einzel=="nein")
#   forest <- bigrfc(dat, as.factor(dat$Dossier.Status), ntree=30, varselect=c(1:8, 10:17))
# }
#make statistics

#to test
#dat=data ; param="Land" ; factor="Deutschland" ; fromDate="2014-01-01" ; toDate="2014-01-30" ; byvar="Reisewochentag"
#args: dat=data, param="Land", factor="Deutschland", fromDate="2014-01-01", toDate="2014-01-30", byvar="Reisewochentag"
#t?gliche fallrate: ausfallts <- ausfallrate(data, "Reisedatum", fromDate="2014-01-01", toDate="2014-01-30")
ausfallrate <- function(param, factor="", fromDate, toDate, byvar="", from="", to="",class="alle", einzel="Alle", byPassenger=T){ #OK
  #data: from which the statistics are made
  #param: parameter for the statistics
  #factor: wished factor of param
  #exp: variable to explain
  #from: begin date
  #to: end date
  #byvar: variable to sort by
  #byPassenger: rate by passenger (vs. by reservation)
  
  fromDate <- as.POSIXct(fromDate, format="%Y-%m-%d")
  fromDate <- strptime(fromDate,"%Y-%m-%d")
  toDate <- as.POSIXct(toDate, format="%Y-%m-%d")
  toDate <- strptime(toDate,"%Y-%m-%d")
  data <- getData(datvon=fromDate, datbis=toDate, trainnr="", fromPlace=from, toPlace=to, einzel=einzel)
  
  if(factor!="") {
    sel <- which(data[[param]]==factor)
    data <- data[sel,] #ok
  }
  
  statuscode <- rep(0, nrow(data))
  sel <- which(data$Dossier.Status=="Storniert")
  statuscode[sel] <- 1
  data$statuscode <- statuscode
  data$zahl <- rep(1, nrow(data))
  
  if(byPassenger){
    data$statuscode <- data$statuscode*(data$Passagiere.2..Kl.+data$Passagiere.1..Kl.)
    data$zahl <- data$zahl*(data$Passagiere.2..Kl.+data$Passagiere.1..Kl.)
  }
  
  data$Reisedatum <- as.POSIXct(data$Reisedatum)
  data$Reisedatum <-  strptime(data$Reisedatum,"%Y-%m-%d")
#   if(!is.na(fromDate)) data <- subset(data, Reisedatum>=fromDate)
#   if(!is.na(toDate))data <- subset(data, Reisedatum<=toDate)
#   if(from!="") data <- subset(data, Reisestart==from)
#   if(to!="") data <- subset(data, Reiseziel==to)
  
  vars <- c()
  if(factor=="") vars <- c(vars, param)
  if(byvar!="") vars <- c(vars, byvar)
  data <- data[,names(data) %in% c(vars, "statuscode", "zahl")]
  if(length(vars)>1){
    byvars <- as.list(data[,names(data) %in% c(vars)])
    dat0 <- aggregate(data$statuscode, by=byvars, FUN=sum)
    dat1 <- aggregate(data$zahl, by=byvars, FUN=sum)
  }
  if(length(vars)==1){
    data[[vars]] <- as.character(data[[vars]])
    dat0 <- aggregate(data[["statuscode"]]~data[[vars]], FUN=sum)
    dat1 <- aggregate(data[["zahl"]]~data[[vars]], FUN=sum)
    names(dat0) <- names(dat1) <- c(vars, "x")
  }    
  dat0$Ausfallrate <- round(dat0$x/dat1$x*100, 2)  
  data <- dat0[,names(dat0) %in% c(vars, "Ausfallrate")]
  data
}

#stat=st; title=""; param="Land"; factor="Deutschland"; from="2014-01-01"; to="2014-01-01"
#args: stat=st, title="", param="Land", factor="Deutschland", from="2014-01-01", to="2014-01-01"
graphStats <- function(stat, title="", param="", factor="", from="", to=""){ #OK
  if(nrow(stat)==0) return(NULL)
  library(ggplot2)
  nms <- names(stat)
  l <- length(nms)
  m <- ggplot(stat, aes_string(x = nms[1], y = nms[l]))
  if(l>2) m <- m + geom_histogram(stat="identity", aes_string(fill=nms[2]))
  else m <- m + geom_histogram(stat="identity", fill="blue")
  
  #title
  if(title==""){
    title <- "Ausfallrate"
    if(l==2)  title <- paste(title, "für", param, factor)
    else title <- paste(title, "bei", nms[1], "und", nms[2])
    title <- paste(title, "\n")
    #if(from!="" & to !="") title <- paste(title, "Von", from, "bis", to)
  }
  print(paste("title:", title))
  m <- m + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  m <- m + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
  m
}

#reservations for a given day and train
#data=data ; trainnb="955" ; day="2014-01-01" ; klasse="2"
#vars: data=data, trainnb="955", day="2014-01-01", klasse="2"
reservations <- function(trainnb, day, klasse="2"){
  print(paste0("reservations, trainnb=", trainnb, ", day=", day, ", klasse=", klasse))
  #trainnb: train number
  day <- strptime(as.POSIXct(day, zone="UTC"),"%Y-%m-%d")
  data$Reisedatum <- strptime(as.POSIXct(data$Reisedatum, format="%Y-%m-%d"),"%Y-%m-%d")
  # print((data$Reisedatum==strptime(as.POSIXct("2014-01-01", format="%Y-%m-%d"), "%Y-%m-%d")))
  # data$Zug.Nr. <- as.character(data$Zug.Nr.)
  data$Dossier.Status <- as.character(data$Dossier.Status)
  print(paste("reservations: day=", day))
  print(paste("reservations, nrow(data)=", nrow(data)))
  dat <- subset(data, Reisedatum==day & Zug.Nr.==trainnb & Dossier.Status=="Belegt")
  print(paste("debug: nrow=", nrow(dat)))
  if(nrow(dat)==0) return("")
  #TODO: Ausfallwahrscheinlichkeit hinf?gen
  classvar <- paste0("Passagiere.",klasse,"..Kl.")
  dat <- dat[,c("Business.Partner", classvar)] #TODO: Ausfall-WK auch selektieren
  dat[[classvar]] <- as.numeric(dat[[classvar]])
  dat <- aggregate(dat[[classvar]]~dat$Business.Partner,FUN=sum)
  names(dat) <- c("Reiseanbieter", "Plätze belegt") #TODO: Name für Ausfall-WK hinfügen

  return(dat)
}

#time prevision
#dataset=dat ; fromDate="2014-01-01" ; toDate="2014-05-30" ; to=3 ; freq="month" ; byPassenger=T
#vars: dataset=data, fromDate="2014-01-01", toDate="2014-05-30", to=3, freq="month", byPassenger=T
prevision <- function(trainnr, fromDate, toDate, fromPlace="", toPlace="", to, var="", factor="", class="alle", einzel="alle", freq="month", byPassenger=T){

  fromDate <- as.POSIXct(fromDate, format="%Y-%m-%d")
  fromDate <- strptime(fromDate,"%Y-%m-%d")
  toDate <- as.POSIXct(toDate, format="%Y-%m-%d")
  toDate <- strptime(toDate,"%Y-%m-%d")
  print(paste("prevision, trainnr=", trainnr))
  dataset <- getData(datvon=fromDate, datbis=toDate, trainnr=trainnr, fromPlace=fromPlace, toPlace=toPlace, einzel=einzel)
  dataset$Reisedatum <- strptime(dataset$Reisedatum,"%Y-%m-%d")
#   dataset <- subset(dataset, Reisedatum>=fromDate & Reisedatum<=toDate)
#   if(trainnr!="Alle") dataset <- subset(dataset, Zug.Nr.==trainnr)
#   if(fromPlace!="") dataset <- subset(dataset, Reisestart==fromPlace)
#   if(toPlace!="") dataset <- subset(dataset, Reiseziel==toPlace)
  if(!byPassenger) weights <- count(dataset, c("Reisedatum"))
  else{
    dataset$passagiere <- dataset$Passagiere.1..Kl.+dataset$Passagiere.2..Kl.
    dataset$Reisedatum <- as.character(dataset$Reisedatum)
    #weights <- aggregate(dataset$passagiere, by=list(Reisedatum=dataset$Reisedatum), FUN=sum)
    weights <- aggregate(passagiere~Reisedatum, FUN=sum, data=dataset)
  }
  ar <- ausfallrate("Reisedatum", fromDate=fromDate, toDate=toDate)
  ar <- join(ar, weights, by=c("Reisedatum"))
  names(ar) <- c("datum", "ausfallrate", "gewichtung")
  ar$datum <- as.POSIXct(ar$datum, format="%Y-%m-%d")
  
  ar.xts <- xts(x = ar$ausfallrate*ar$gewichtung, order.by = ar$datum) #ausfallrate
  wt.xts <- xts(x = ar$gewichtung, order.by = ar$datum) #ausfallrate
  
  if(freq=="day"){
    data.xts <- apply.daily(datas.xts, FUN=agg)
    form <- "%d-%m-%Y"
  }
  if(freq=="month"){
    ar.xts <- apply.monthly(ar.xts, FUN=sum)
    wt.xts <- apply.monthly(wt.xts, FUN=sum)
    form <- "%b %Y"
  } 
  ar.xts <- xts(ar.xts)
  wt.xts <- xts(wt.xts)
  coredata(ar.xts) <- coredata(ar.xts)/coredata(wt.xts)
  
  ar.xts <- xts(ar.xts)
  firstDate <- index(ar.xts)[1]
  ar.xts <- ts(ar.xts)
  
  fitArima <- auto.arima(ar.xts)
  forecasting <- forecast(fitArima, to)
  plot(forecasting, main=paste("Vorhersage der Ausfallrate"), axes=F)
  a <- c(index(ar.xts), index(forecasting$mean))
  ap <- seq(as.Date(firstDate), by=freq, length=length(a))
  axis(1, at = a, labels = format(ap, form), cex.axis=0.6, las=2)
  axis(2, at=seq(0, max(coredata(ar.xts)+5), by=5), las=1)
}

#dataset <- data; fromDate <- "2014-01-01"; toDate <- "2014-06-01"; trainnr <-"961"; einzel <- "all"; class <- "alle"; kontingent=F; to <- 3
#auslastung(dat, "2014-01-01", "2014-06-01", "961")
auslastung <- function(fromDate, toDate, trainnr, kontingent=F, einzel="Alle", class="alle", freq="month", byPassenger=T, to=3){
  library(forecast)
  library(xts)
  fromDate <- as.POSIXct(fromDate, format="%Y-%m-%d")
  fromDate <- strptime(fromDate,"%Y-%m-%d")
  toDate <- as.POSIXct(toDate, format="%Y-%m-%d")
  toDate <- strptime(toDate,"%Y-%m-%d")
  data <- getData(datvon=fromDate, datbis=toDate, trainnr=trainnr, fromPlace=NA, toPlace=NA, einzel=einzel)
  
  data$Reisedatum <- as.POSIXct(data$Reisedatum, format="%Y-%m-%d")
  #dataset$jahr <- format(dataset$Reisedatum, "%Y")
  data$Reisedatum <- strptime(data$Reisedatum,"%Y-%m-%d")
  
#   print(paste0("einzel=",einzel))
#   print(paste0("trainnr=",trainnr))
  
  data <- subset(data, Dossier.Status=="Belegt")
  # data <- subset(data, Reisedatum>=fromDate & Reisedatum<=toDate)
  # if(einzel!="Alle") data <- subset(data, Einzel==einzel)
  # if(trainnr!="Alle") data <- subset(data, Zug.Nr.==trainnr)
  
  #if(!byPassenger) weights <- count(dataset, c("Reisedatum"))

  data$passagiere <- data$Passagiere.1..Kl.+data$Passagiere.2..Kl.
  if(class=="1") data$passagiere <- data$Passagiere.1..Kl.
  if(class=="2") data$passagiere <- data$Passagiere.2..Kl.   
  data$Reisedatum <- as.character(data$Reisedatum)
  #weights <- aggregate(dataset$passagiere, by=list(Reisedatum=dataset$Reisedatum), FUN=sum)
  
  saisons <- getSaison(data$Reisedatum)
  data$jahr <- saisons$jahr
  data$saison <- saisons$saison
  agg <- aggregate(passagiere~Reisedatum+Zug.Nr.+jahr+saison, data=data, FUN=sum)
  agg$Reisedatum <- as.POSIXct(agg$Reisedatum, format="%Y-%m-%d")
  agg <- getPlacesAgg(agg, einzel, class)
  agg$auslastung <- agg$passagiere/agg$places*100
  
  print("Aggregation:")
  print(agg$auslastung)
  
  ar.xts <- xts(x = agg$auslastung, order.by = agg$Reisedatum) #auslastung
  
  if(freq=="month"){
    ar.xts <- apply.monthly(ar.xts, FUN=mean)
    form <- "%b %Y"
  } 
  ar.xts <- xts(ar.xts)
  
  ar.xts <- xts(ar.xts)
  firstDate <- index(ar.xts)[1]
  ar.xts <- ts(ar.xts)
  
  fitArima <- auto.arima(ar.xts)
  forecasting <- forecast(fitArima, to)
  plot(forecasting, main=paste("Vorhersage der Auslastung"), axes=F)
  a <- c(index(ar.xts), index(forecasting$mean))
  ap <- seq(as.Date(firstDate), by=freq, length=length(a))
  axis(1, at = a, labels = format(ap, form), cex.axis=0.6, las=2)
  axis(2, at=seq(0, max(coredata(ar.xts)+5), by=5), las=1)
#   print(paste("max...", max(coredata(ar.xts)+5)))
#   axis(2, at=seq(0, 100, by=5), las=1)
}

#data=dat ; forest=forest ; date="2014-01-01" ; trainno="952" ; anbieter="DB- Fernverkehr - Deutschland, P. TBF 12, Frankfur" ; places="2" ; from="Chur" ; to="Tirano" ; class="2" #26%
#data=dat ; forest=forest ; date="2014-01-01" ; trainno="952" ; anbieter="Ameropa-Reisen GmbH, Bad Homburg" ; places="2" ; from="Chur" ; to="Tirano" ; class="2"

#args: data=data, forest=forest, date="2014-01-01", trainno="952", anbieter="DB- Fernverkehr - Deutschland, P. TBF 12, Frankfur", places=2, from="Chur", to="Tirano", class="2"
# ausfallWK <- function(date, trainno, anbieter, land="", places, from, to, class){
#   #data: current data frame
#   #date: date of the trip
#   #trainno: number of the train
#   #places: number of places
#   #from: starting place of the trip
#   #to: end place of the trip
#   #class: class of the places to reserve (first/second)
#   
#   library(bigrf)
#   library(doMC)
#   registerDoMC(cores=4)
#   
#   places <- as.numeric(places)
#   date <- as.POSIXct(date, format="%Y-%m-%d")
#   #take the more frequent value for each variable
#   land <- modeland(data, anbieter)
#   #land <- "China"
#   data <- subset(data, Einzel=="nein")
#   #modes <-apply(data, 2, mode)
#   modes <- lapply(data[1:17], mode)
#   #make dataframe
#   df <- data.frame(matrix(modes, nrow=1))
#   colnames(df) <- names(modes)
#   df$Dossier.Status <- factor(df$Dossier.Status, levels=levels(data$Dossier.Status))
#   #replace values
#   df$Reisedatum <- date
#   df$Reisewochentag <- weekday(as.character(date))
#   df$Einzel <- "nein"
#   if(class=="1"){
#     df$Passagiere.1..Kl. <- places
#     df$Passagiere.2..Kl. <- 0
#   }
#   if(class=="2"){
#     df$Passagiere.1..Kl. <- 0
#     df$Passagiere.2..Kl. <- places
#   }
#   df$Reisestart <- from
#   df$Reiseziel <- to
#   df$Business.Partner <- anbieter
#   df$Zug.Nr. <- trainno
#   df$Land <- land
# #   data <- data[,c("Reisedatum", "Reisewochentag", "Zug.Nr.", "Zugkategorie", "Einzel", "Wagentyp", "Passagiere.2..Kl.", "Passagiere.1..Kl.", "Dossier.Status", "Verkaufsort", "Verkaufskanal", "Key.Account", "Last.second.Anbieter", "Land", "Business.Partner")]
# #   df <- df[,c("Reisedatum", "Reisewochentag", "Zug.Nr.", "Zugkategorie", "Einzel", "Wagentyp", "Passagiere.2..Kl.", "Passagiere.1..Kl.", "Dossier.Status", "Verkaufsort", "Verkaufskanal", "Key.Account", "Last.second.Anbieter", "Land", "Business.Partner")]
#   df <- formatData(df)
#   #data <- rbind(data, df)
#   #probability
#   print(dim(df))
#   print(dim(data))
#   df <- rbind(df, data)
#   print(paste("debug, nrow(df) =", nrow(df)))
#   forest0 <- bigrfc(df, as.factor(df$Dossier.Status), ntree=30L, varselect=as.integer(c(1, 2, 3, 7, 8, 16, 17)), cachepath=NULL)
#   sessionInfo()
#   prediction <- predict(forest0, df, as.factor(df$Dossier.Status))
#   
#   print(paste("AusfallWK, nrow(prediction)=", nrow(prediction)))
# #   prediction <- cprob(prediction)
# #   print("prediction")
# #   print(head(prediction))
# #   print(mean(prediction))
# #   prediction <- prediction[length(prediction)]
#   
#   prediction <- cprob(prediction)[1, 2]
#   
# #   prediction <- predict(forest0, df, as.factor(df$Dossier.Status))
# #   prediction <- cprob(prediction)
# #   prediction <- prediction[1]
#   #p <- prediction[nrow(data)]
#   #p <- prediction[nrow(prediction)]
#   
#   return(prediction)
# }

#probability of success of a reservation request
# prediction <- ausfallWK(data=data, forest=forest, date="2014-01-01", trainno="955", anbieter="DB- Fernverkehr - Deutschland, P. TBF 12, Frankfur", land="Deutschland", places="2", from="Chur", to="Tirano", class="2")
# predictions <- predict(forest, data, as.factor(data$Dossier.Status), cachepath=NULL)
# preds <- cprob(predictions)
# data=data ; predictions=preds ; prediction=prediction ; date="2015-04-15" ; trainno="955" ; places="3" ; class="2" ; poss=270
#args: data=data, predictions=preds, prediction=prediction, date="2014-01-01", trainno="955", places="3", class="2", poss=50
successWK <- function(prediction, date, trainno, places, class, poss=270, rdat=F){ #OK
  #data: current data set
  #predictions: predictions for data
  #prediction: prediction for the reservation request
  #poss: number of possible train places
  
  places <- as.numeric(places)
  p <- 0
  #number of reservation
  resnb <- reservations(trainno, date, class)
  print("head(resnb):")
  print(head(resnb))
  if(resnb=="") resnb <- 0
  else resnb <- sum(resnb[,2])
  print(paste0("resnb=", resnb))
  dispo <- poss-resnb
  print(paste0("poss=", poss, ", dispo=", dispo))
  if(places<=dispo) p <- 100
  else{
    cl <- 2-as.numeric(class) #0 for 2nd class, 1 for 1st class
    date <- as.POSIXct(date, format="%Y-%m-%d")
    date <- strptime(date,"%Y-%m-%d")
    data <- mPredictions(data)
    
    data$pl <- cl*data$Passagiere.1..Kl.+(1-cl)*data$Passagiere.2..Kl.
    data$Reisedatum <- as.POSIXct(data$Reisedatum)
    data$Reisedatum <- strptime(data$Reisedatum,"%Y-%m-%d")
    
    dat <- subset(data, Zug.Nr.==trainno & Reisedatum==date & pl>0)
    print(paste("debug, nrow(dat) =", nrow(dat)))
    #dat <- dat[order(dat$predictions, decreasing=T),]
    dat <- dat[order(as.vector(dat$predictions), decreasing=T),]
    dat$cumpl <- cumsum(dat$pl)
    mis <- resnb+places-poss #missing places
    ind <- min(which(dat$cumpl>=mis))
    if(ind==Inf) p <- 0
    else{
      dat <- dat[1:ind,]
      p0 <- dat$predictions/100
      p0 <- prod(p0)
      print(paste("prediction=", prediction, ", p0=", p0))
      p0 <- prediction/100+(100-prediction)/100*p0
      p <- p0*100
    }
  }
  p <- round(p, 2)
  res <- list(totalPlaces=as.character(poss), reservedPlaces=as.character(resnb), pSuccess=as.character(p))
  return(res)
}

# mPredictions <- function(df){
#   library(bigrf)
#   #library(plyr)
#   #forest <- unserialize(connection=gzfile("forest.rds", "r"))
#   #forest <- forest0
#   data$idrow <- 1:nrow(data)
#   data$predictions <- predictions
#   sdata <- subset(data, Einzel=="nein")
#   #predictions <- predict(forest, sdata, as.factor(sdata$Dossier.Status))
#   #predictions <- cprob(predictions)
#   #sdata$predictions <- predictions
#   sdata <- sdata[,c("idrow", "predictions")]
#   p <- nrow(subset(data, Einzel=="ja", Dossier.Status=="Belegt"))/nrow(subset(data, Einzel=="ja"))
#   p <- paste0(p, "%")
#   #print(paste0("nrow(data)=", nrow(data), ", nrow(sdata)=", nrow(sdata)))
#   if(nrow(sdata)!=nrow(data)){
#     sdata1 <- data.frame(idrow=setdiff(1:nrow(data), sdata$idrow), predictions=p)
#     sdata <- rbind(sdata, sdata1)
#   }
#   data <- merge(data, sdata, by="idrow")
#   data
# }

#anbieter="SBB Swiss Federal Railways, 3000 Bern 65 SBB"
#which land for an anbieter?
modeland <- function(data, anbieter){ #OK
  print(dim(data))
  dat <- subset(data, data$Business.Partner==anbieter)
  if(nrow(dat)==0) dat <- data
  land <- mode(dat$Land)
  land <- as.character(land)
  land
}

#date="2015-06-04"
#which week day for a date?
weekday <- function(date){ #OK
  date <- as.POSIXct(date, format="%Y-%m-%d")
  wd <- weekdays(date, abbreviate = FALSE)
  days <- data.frame(Monday="Mo", Tuesday="Di", Wednesday="Mi", Thursday="Do", Friday="Fr", Saturday="Sa", Sunday="So")
  day <- as.character(days[[wd]])
  day
}

#probability of cancelling from prediction vote
cprob <- function(pred){
  pred <- pred@testvotes/rowSums(pred@testvotes)
  #pred <- round((1-pred)*100, 2)
  pred <- round(pred*100, 2)
  print(head(pred))
  pred
}

mode <- function(x){names(table(x))[which.max(table(x))]}

# getFactors <- function(data, var){
#   return(sort(as.character(unique(data[[var]]))))
# }

mReservation <- function(datres, zugnr, anbieter, land, plaetze, von, bis, klasse){
  #forest <- unserialize(connection=gzfile("forest.rds", "r"))
  tplaces <- saison(datres, 3, 10, zugnr, klasse, F, "Gruppe") #TODO: NA error
  pAusfall <- ausfallWK(datres, zugnr, anbieter, land, plaetze, von, bis, klasse)
  print(paste0("tplaces=", tplaces))
  res <- successWK(pAusfall, datres, zugnr, plaetze, klasse, poss=tplaces)
  return(list(totalPlaces=res$totalPlaces, reservedPlaces=res$reservedPlaces, pAusfall=as.character(pAusfall), pSuccess=res$pSuccess))
}

tableReservations <- function(predictions, date, trainno, places, class){
  #data: current data set
  #predictions: predictions for data
  print(paste0("tableReservations, nrow(data)=", nrow(data), ", nrow(predictions)=", nrow(predictions)))
  places <- as.numeric(places)
  #number of reservation
  resnb <- reservations(trainno, date, class)
  if(resnb==""){
    df <- data.frame(matrix(c("", "", ""), 1, 3, dimnames=list(c(), c("Anbieter", "Plätze", "Ausfallwahrscheinlichkeit"))))
    return(df)
  }
  cl <- 2-as.numeric(class) #0 for 2nd class, 1 for 1st class
  date <- as.POSIXct(date, format="%Y-%m-%d")
  date <- strptime(date,"%Y-%m-%d")
  # data$predictions <- predictions[,2]
  
  data$pl <- cl*data$Passagiere.1..Kl.+(1-cl)*data$Passagiere.2..Kl.
  data$Reisedatum <- as.POSIXct(data$Reisedatum)
  data$Reisedatum <- strptime(data$Reisedatum,"%Y-%m-%d")
  data <- subset(data, Zug.Nr.==trainno & Reisedatum==date & pl>0 & Dossier.Status=="Belegt")
  print(paste("tableReservations, nrow(data)=", nrow(data)))
  data$predictions <- predictions
  data <- data[order(data$predictions, decreasing=T),]
  data <- data[,c("Business.Partner", "pl", "predictions")]
  names(data) <- c("Anbieter", "Plätze", "Ausfallrisiko")
  return(data)
}

#get saison(sommer/winter) and year for vector of dates
getSaison <- function(dates, msommer=3, mwinter=10){
  dates <- as.POSIXct(dates, format="%Y-%m-%d")
  jahr <- as.numeric(format(dates, "%Y"))
  month <- as.numeric(format(dates, "%m"))
  
  saison <- rep("Sommer", length.out=length(dates))
  saison[which(month>=mwinter | month<msommer)] <- "Winter"
  jahr[which(month<msommer)] <- jahr[which(month<msommer)]-1
  df <- data.frame(jahr, saison)
  df
}

saison <- function(dates, msommer, mwinter, trainno, class, contingent, einzel){
  #dates: vector of dates
  #msommer: month beginning sommer season
  #mwinter: month beginning winter season
  dates <- as.POSIXct(dates, format="%Y-%m-%d")
  year <- as.numeric(format(dates, "%Y"))
  month <- as.numeric(format(dates, "%m"))
  
  saison <- rep("Sommer", length.out=length(dates))
  saison[which(month>=mwinter | month<msommer)] <- "Winter"
  year[which(month<msommer)] <- year[which(month<msommer)]-1
  df <- data.frame(year, saison)
  df <- getPlacesTrain(df, trainno, class, contingent, einzel)
  
  return(df$places)
}

#getPlacesTrain(saisons, "961", "2", T, "nein")
getPlacesTrain <- function(df, trainno, class, contingent=T, einzel="nein"){
  dfu <- unique(df)
  dfu$places <- 0
  for(i in seq(1, nrow(dfu))){
    dfu$places[i] <- getPlaces(trainno, class, contingent, einzel, df$year[i], df$saison[i])
  }
  df <- merge(df, dfu, by=c("year", "saison"))
  return(df)
}

getPlacesAgg <- function(agg, einzel, klasse){
  agg1 <- data.frame(Zug.Nr.=agg$Zug.Nr., saison=agg$saison, jahr=agg$jahr)
  agg1 <- unique(agg1)
  if(einzel=="ja") einzel <- "Einzel"
  if(einzel=="nein") einzel <- "Gruppe"
  for(i in seq(1, nrow(agg1))){
    agg1$places <- getPlaces(agg1$Zug.Nr., klasse, F, einzel, agg1$jahr, agg1$saison)
  }
  agg <- merge(agg, agg1, by=c("Zug.Nr.", "saison", "jahr"))
  print("agg")
  print(agg)
  agg
}

#date="2014-01-01", trainno="952", anbieter="DB- Fernverkehr - Deutschland, P. TBF 12, Frankfur", places=2, from="Chur", to="Tirano", class="2"
#compute probabilities for a reservation
resProba <- function(date, trainno, anbieter, land="", places, from, to, class){
  #first create an entry for the demanded reservation: df
#   library(bigrf)
#   library(doMC)
#   registerDoMC(cores=4)
  library(randomForest)
  
  tPlaces <- reservations(trainno, date, class)
  
  places <- as.numeric(places)
  date <- as.POSIXct(date, format="%Y-%m-%d")
  data$Reisedatum <- as.POSIXct(data$Reisedatum, format="%Y-%m-%d")
  #take the more frequent value for each variable
  land <- modeland(data, anbieter)
  data <- subset(data, Einzel=="nein")
  df <-apply(data[,1:17], 2, mode)
  #make dataframe
  df <- data.frame(matrix(df, nrow=1))
  colnames(df) <- names(data)
  #df$Dossier.Status <- factor(df$Dossier.Status, levels=levels(data$Dossier.Status))
  #replace values
  df$Reisedatum <- date
  df$Reisewochentag <- weekday(as.character(date))
  df$Einzel <- "nein"
  if(class=="1"){
    df$Passagiere.1..Kl. <- places
    df$Passagiere.2..Kl. <- 0
  }
  if(class=="2"){
    df$Passagiere.1..Kl. <- 0
    df$Passagiere.2..Kl. <- places
  }
  df$Dossier.Status <- NA #added
  df$Reisestart <- from
  df$Reiseziel <- to
  df$Business.Partner <- anbieter
  df$Zug.Nr. <- trainno
  df$Land <- land
  #df <- formatData(df)
  
  #second order the reservations by date and take #reservations for res. date
  
  data <- subset(data, Reisedatum <= date)
  data$pl <- data[[paste0("Passagiere.", class, "..Kl.")]]
  numRes <- nrow(subset(data, Reisedatum==date & Dossier.Status=="Belegt" & Zug.Nr.==trainno & pl>0))
  data <- data[,-which(names(data) %in% c("pl"))]
  
  #number of passengers for given day and class
  print(subset(data, Reisedatum==date & Dossier.Status=="Belegt" & Zug.Nr.==trainno))
  tPassagiere = sum(as.numeric(subset(data, Reisedatum==date & Dossier.Status=="Belegt" & Zug.Nr.==trainno)[[paste0("Passagiere.", class, "..Kl.")]]))
  
  print(paste("numRes =", numRes))
  
  data <- subset(data[1+numRes:nrow(data), ], Passagiere.2..Kl.<53-numRes & Passagiere.1..Kl.<53-numRes)

#   print("before forest0, dim(data)")
#   print(dim(data))
  
  #third compute probabilities
  
#   forest0 <- bigrfc(data, as.factor(data$Dossier.Status), ntree=30L, varselect=as.integer(c(1, 2, 3, 7, 8, 16, 17)), cachepath=NULL)
#   prediction <- predict(forest0, data, as.factor(data$Dossier.Status))
#   prediction <- cprob(prediction)[,2]
#   predRes <- 100-prediction[1] #for the demanded prediction
#   predictions <<- prediction[1+numRes] #for reservations on res. day
  
  data <- data[1:1000,]
  data <- formatData(data)
  numuni <- apply(data, 2, f <- function(x){length(unique(x))})
  data <- data[, numuni<53]
  
  rf <- randomForest(Dossier.Status~., data=data)
  print("head of rf$votes")
  print(head(rf$votes))
  pred <- c(round(100*rf$votes[,2]/(rf$votes[,1]+rf$votes[,2]), 2))
  print("head of pred")
  print(head(pred))
  predRes <- pred[1] #for the demanded prediction
  predictions <<- pred[2:2+numRes] #for reservations on res. day
  
  print(paste("length(predictions) =", length(predictions)))
  print("predictions:")
  print(predictions)
  
  # predPrior <- prediction[2+numRes:length(prediction)] #for reservations before res. day
  tPlaces <- saison(date, 3, 10, trainno, class, F, "Gruppe") #total available places
  print(paste("tPlaces=", tPlaces))
  #res <- successWK(predRes, date, trainno, places, class, poss=tplaces) #success prediction
  
  #success probability
  print(paste0("tPassagiere=", tPassagiere, ", places=", places, ", tPlaces", tPlaces))
  mis <- tPassagiere+places-tPlaces #missing places
  if(mis<=0) p <- 100
  else{
    data <- data[1:numRes+1,]
    data$cumpl <- cumsum(data[[paste0("Passagiere.",class,"..Kl.")]])
    ind <- min(which(data$cumpl>=mis))
    if(ind==Inf) p <- 0
    else{
      p0 <- predictions/100
      p0 <- prod(p0)
      print(paste("predictions=", predictions, ", p0=", p0))
      p0 <- 1-predictions/100+(100-predictions)/100*p0
      p <- p0*100
    }
    p <- round(p, 2)
  }
  
  return(list(pAusfall=predRes, pSuccess=p, totalPlaces=tPlaces, reservedPlaces=tPassagiere))
}