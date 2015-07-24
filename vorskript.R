library(DBI)
library(RMySQL)
library(bigrf)
library(doMC)
registerDoMC(cores=4)

data <- getData()
data <- formatData(data)

forest0 <<- bigrfc(data, as.factor(data$Dossier.Status), ntree=30L, varselect=as.integer(c(1, 2, 3, 7, 8, 16, 17)), cachepath=NULL)

predictions <- predict(forest0, data, as.factor(data$Dossier.Status))
predictions <- cprob(predictions)



#write Reservations
# connexion = dbConnect(MySQL(), host="localhost", port=3306, dbname="gdb", user="gaikan", password="gaikan123")
# dat <- dataset
# names(dat) <- gsub("[:.:]", "", names(dat))
# dbWriteTable(connexion, value = dat, name = "Reservations", append = TRUE, row.names=FALSE)
# rm(list="dat")
# dbSendQuery(connexion, "INSERT INTO Intervals VALUES('2014-01-01', '2014-06-30');")
# dbDisconnect(connexion)


