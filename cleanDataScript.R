library(stringr)
parseStats <- function(inputFile) {
  fileName <- paste("Stats/", inputFile, ".csv", sep = "")
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