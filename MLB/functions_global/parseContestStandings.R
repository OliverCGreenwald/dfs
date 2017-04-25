if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Preprocesses contest-standings.csv files.


####### Begin Function #######
parseContestStandings <- function(contest.date, contest.name) {
  # load libraries
  require(stringr)
  
  # read in contest standings
  temp.results <- read.csv(file = paste0("MLB/data_warehouse/", contest.date, "/", contest.name, "/contest-standings.csv"), stringsAsFactors = F, header = T)
  
  # remove useless columns
  temp.results$TimeRemaining <- NULL
  temp.results$X <- NULL
  temp.results$Player <- NULL
  temp.results$X.Drafted <- NULL
  temp.results$FPTS <- NULL
  
  # initialize columns for each position
  temp.ind <- ncol(temp.results)
  temp.results[,(temp.ind+1):(temp.ind+10)] <- NA
  colnames(temp.results)[(temp.ind+1):(temp.ind+10)] <- c('P1','P2','C','1B','2B','3B','SS','OF1','OF2','OF3')
  
  # function for parsing the Lineup column
  splitPlayers <- function(x) {
    return(str_split_fixed(x, "P | P | C | 1B | 2B | 3B | SS | OF ", 11)[2:11])
  }
  
  # fill position columns by parsing Lineup column
  temp.players <- data.frame(matrix(unlist(lapply(X = temp.results$Lineup[1:nrow(temp.results)], FUN = splitPlayers)), nrow=nrow(temp.results), byrow=T))
  temp.results[,(temp.ind+1):(temp.ind+10)] <- temp.players
  
  # split EntryName column into username and entry number
  temp.entry <- str_split_fixed(temp.results$EntryName, " ", 2)
  temp.results$User_Name <- temp.entry[,1]
  temp.results$Entry_Num <- temp.entry[,2]
  
  return(temp.results)
}


# debugging
# contest.date <- "2017-04-23"
# contest.name <- "$4.00entry_MLB$35KFOUR-SEAMER(AFTERNOON)"





