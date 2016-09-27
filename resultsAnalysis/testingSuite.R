#setwd("~/Projects/DFS/resultsAnalysis")

#--------- Set week number automation purposes ---------#
week.num <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1)

#--------- Input DF's ---------#
contest.results.week3 <- read.csv("data_warehouse/contest_lineups_week3.csv", stringsAsFactors = F)
player.performance.week1 <- read.csv("data_warehouse/player_weekly_performance/draftkings_player_production_week1.csv", stringsAsFactors = F)
player.performance.week2 <- read.csv("data_warehouse/player_weekly_performance/draftkings_player_production_week2.csv", stringsAsFactors = F)
player.performance.week3 <- read.csv("data_warehouse/player_weekly_performance/draftkings_player_production_week3.csv", stringsAsFactors = F)


####### CLEAN PLAYER PERFORMANCE CSV'S #########
player.performance.week1$Player <- sub(' Sr.','', player.performance.week1$Player)
player.performance.week1$Player <- sub(' Jr.','', player.performance.week1$Player)

player.performance.week2$Player <- sub(' Sr.','', player.performance.week2$Player)
player.performance.week2$Player <- sub(' Jr.','', player.performance.week2$Player)

player.performance.week3$Player <- sub(' Sr.','', player.performance.week3$Player)
player.performance.week3$Player <- sub(' Jr.','', player.performance.week3$Player)

######## CHANGE THIS TO YOUR FILE THAT YOU WANT TO COMPARE TO: ########

lineups <- read.csv("../optimizationCode/submitted_lineups/week1_lineups.csv", stringsAsFactors = F)

######## Clean Output ########
for (i in 1:ncol(lineups)) {
  lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
  lineups[,i] <- sub(' Sr.','', lineups[,i])
  lineups[,i] <- sub(' Jr.','', lineups[,i]) 
}
lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)

total_results <- player.performance.week1[,c('Player', 'Actual.Score')]
lineups$total <- 0

for (index in 1:nrow(lineups)){
  row <- t(lineups[index,])
  colnames(row) <- 'Player'
  row <- merge(row, total_results, by = 'Player')
  lineups$total[index] <- sum(row$Actual.Score)
}

plot(lineups$total, main = 'week 1')