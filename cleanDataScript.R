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
  id <- vector(mode="character", length=size)
  name <- vector(mode="character", length=size)
  position <- vector(mode="character", length=size)
  team <- vector(mode="character", length=size)
  for(i in 1:size) {
    attrs <- xmlAttrs(xmlData[[i]])
    id[i] <- attrs["id"]
    name[i] <- attrs["name"]
    position[i] <- attrs["position"]
    team[i] <- attrs["team"]
  }
  dataFrame <- cbind(id,name,position,team)
  dataFrame <- dataFrame[dataFrame[,"position"] == "QB" | dataFrame[,"position"] == "RB" |
                         dataFrame[,"position"] == "WR" | dataFrame[,"position"] == "TE",]
  return(dataFrame)
}

parseADP <- function(inputFile) {
  fileName <- paste("RawData/ADP/", inputFile, ".xml", sep="")
  xmlData <- xmlRoot(xmlParse(fileName))
  size <- xmlSize(xmlData)
  id <- vector(mode="character", length=size)
  adp <- vector(mode="numeric", length=size)
  for(i in 1:size) {
    attrs <- xmlAttrs(xmlData[[i]])
    id[i] <- attrs["id"]
    adp[i] <- as.numeric(attrs["averagePick"])
  }
  dataFrame <- cbind(id,adp)
  return(dataFrame)
}

writeStats <- function() {
  for(i in 2011:2016) {
    dataFile <- parseStats(i)
    fileName <- paste("CleanData/Stats/", i, "_clean.csv", sep="")
    write.csv(dataFile, fileName)
  }
}

writeID <- function() {
  for(i in 2011:2016) {
    dataFile <- parseID(i)
    fileName <- paste("CleanData/ID_to_Name/", i, "_clean.csv", sep="")
    write.csv(dataFile, fileName)
  }
}

writeADP <- function() {
  for(i in 2011:2016) {
    dataFile <- parseADP(i)
    fileName <- paste("CleanData/ADP/", i, "_clean.csv", sep="")
    write.csv(dataFile, fileName)
  }
}