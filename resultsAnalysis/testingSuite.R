#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/resultsAnalysis")

#--------- Set week number for testing ---------#
week.num <- 1 # change this!

####### IMPORT AND CLEAN DK HISTORICAL FPTS DATA FOR THE WEEK #########
file.name <- paste0("data_warehouse/player_weekly_performance/draftkings_player_production_week", week.num, ".csv")
player.performance <- read.csv(file = file.name, stringsAsFactors = F)

player.performance$Player <- sub(' Sr.','', player.performance$Player)
player.performance$Player <- sub(' Jr.','', player.performance$Player)

######## IMPORT SUBMITTED LINEUPS ########
file.name <- paste0("../optimizationCode/submitted_lineups/week", week.num, "_lineups.csv")
lineups <- read.csv(file = file.name, stringsAsFactors = F)

######## CALCULATE FPTS FOR EACH LINEUP ########
for (i in 1:ncol(lineups)) {
  lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
  lineups[,i] <- sub(' Sr.','', lineups[,i])
  lineups[,i] <- sub(' Jr.','', lineups[,i]) 
}
lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)

total_results <- player.performance[,c('Player', 'Actual.Score')]
lineups$total <- 0

for (index in 1:nrow(lineups)){
  row <- t(lineups[index,])
  colnames(row) <- 'Player'
  row <- merge(row, total_results, by = 'Player')
  lineups$total[index] <- sum(row$Actual.Score)
}

plot(lineups$total, main = paste0("Week ", week.num), ylab = "Lineup FPts")