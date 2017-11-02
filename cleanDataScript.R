library(stringr)
library(XML)
library(xml2)
library(plyr)
parseStats <- function(inputFile) {
  fileName <- paste("RawData/Stats/", inputFile, ".csv", sep = "")
  df <- read.csv(fileName, stringsAsFactors = FALSE)
  df[1,2] <- "Name"
  names(df) <- df[1,]
  df <- df[-c(1),]
  df <- df[,-c(1)]
  df$`Y/A` <- NULL
  df$`Y/R` <- NULL
  df$`FantPt?` <- NULL
  df$DKPt <- NULL
  df$FDPt <- NULL
  columns <- c("Name", "Team", "Position", "Age", "Games", "Starts",
             "Completion", "Pass_Attempts", "Pass_Yards", "Pass_TDs",
             "Interceptions", "Rush_Attempts", "Rush_Yards", "Rush_TDs",
             "Targets", "Receptions", "Rec_Yards", "Rec_TDs", "VBD",
             "Position_Rank", "Overall_Rank")
  names(df) <- columns
  vec <- tail(columns, -3)
  df[vec] <- sapply(df[vec], as.numeric)
  df$Team <- as.factor(df$Team)
  df$Position <- as.factor(df$Position)
  playerName <- str_split(df$Name, "\\\\", simplify = TRUE)[,1]
  playerName <- str_split(playerName, "\\*", simplify = TRUE)[,1]
  df$Name <- playerName
  return(df)
}

parseID <- function(inputFile) {
  fileName <- paste("RawData/ID_to_Name/", inputFile, ".xml", sep="")
  xmlData <- xmlRoot(xmlParse(fileName))
  size <- xmlSize(xmlData)
  Id <- vector(mode="character", length=size)
  Name <- vector(mode="character", length=size)
  Position <- vector(mode="character", length=size)
  Team <- vector(mode="character", length=size)
  for(i in 1:size) {
    attrs <- xmlAttrs(xmlData[[i]])
    Id[i] <- attrs["id"]
    Name[i] <- attrs["name"]
    Position[i] <- attrs["position"]
    Team[i] <- attrs["team"]
  }
  dataFrame <- cbind(Id,Name,Position,Team)
  dataFrame <- dataFrame[dataFrame[,"Position"] == "QB" | dataFrame[,"Position"] == "RB" |
                         dataFrame[,"Position"] == "WR" | dataFrame[,"Position"] == "TE",]
  return(dataFrame)
}

parseADP <- function(inputFile) {
  fileName <- paste("RawData/ADP/", inputFile, ".xml", sep="")
  xmlData <- xmlRoot(xmlParse(fileName))
  size <- xmlSize(xmlData)
  Id <- vector(mode="character", length=size)
  Adp <- vector(mode="numeric", length=size)
  for(i in 1:size) {
    attrs <- xmlAttrs(xmlData[[i]])
    Id[i] <- attrs["id"]
    Adp[i] <- as.numeric(attrs["averagePick"])
  }
  dataFrame <- cbind(Id,Adp)
  return(dataFrame)
}

writeStats <- function() {
  for(i in 2011:2016) {
    dataFile <- parseStats(i)
    fileName <- paste("CleanData/Stats/", i, "_clean_stats.csv", sep="")
    write.csv(dataFile, fileName)
  }
}

writeID <- function() {
  for(i in 2011:2016) {
    dataFile <- parseID(i)
    fileName <- paste("CleanData/ID_to_Name/", i, "_clean_id.csv", sep="")
    write.csv(dataFile, fileName)
  }
}

writeADP <- function() {
  for(i in 2011:2016) {
    dataFile <- parseADP(i)
    fileName <- paste("CleanData/ADP/", i, "_clean_adp.csv", sep="")
    write.csv(dataFile, fileName)
  }
}

writeAll <- function() {
  writeADP()
  writeStats()
  writeID()
}

writeJoin <- function() {
  for(i in 2011:2016) {
    statFileName <- paste("CleanData/Stats/", i, "_clean_stats.csv", sep="")
    idFileName <- paste("CleanData/ID_to_Name/", i, "_clean_id.csv", sep="")
    adpFileName <- paste("CleanData/ADP/", i, "_clean_adp.csv", sep="")
    stats <- read.csv(statFileName)
    id_to_name <- read.csv(idFileName)
    adp <- read.csv(adpFileName)
    idadp <- merge(id_to_name, adp, by = "Id")
    idadp <- idadp[order(idadp[,7]),]
    idadp$X.x <- NULL
    idadp$X.y <- NULL
    idadp$Position <- NULL
    idadp$Team <- NULL
    playerName <- str_split(idadp$Name, "\\, ", simplify = TRUE)
    dp <- paste(playerName[,2], playerName[,1])
    idadp$Name <- dp
    total <- merge(idadp, stats, by = "Name")
    total <- total[order(total[,3]),]
    paste(total)
    totalFileName <- paste("CleanData/Total/", i, "_clean_total.csv", sep="")
    write.csv(total, totalFileName)
  }
}