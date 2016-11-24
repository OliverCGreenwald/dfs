#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we clean various csv files in the optimizationCode folder for ease of use in other scripts.
# TODO: clean up this hard coded shit

####### REMOVE FIRST AND LAST NAME FROM PLAYER PRODUCTION CSV #########
fpts.realized.week1 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week1.csv', stringsAsFactors = F)
fpts.realized.week2 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week2.csv', stringsAsFactors = F)
fpts.realized.week3 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week3.csv', stringsAsFactors = F)
fpts.realized.week4 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week4.csv', stringsAsFactors = F)
fpts.realized.week5 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week5.csv', stringsAsFactors = F)
fpts.realized.week6 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week6.csv', stringsAsFactors = F)
fpts.realized.week7 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week7.csv', stringsAsFactors = F)
fpts.realized.week8 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week8.csv', stringsAsFactors = F)
fpts.realized.week9 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week9.csv', stringsAsFactors = F)
fpts.realized.week10 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week10.csv', stringsAsFactors = F)
fpts.realized.week11 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week11.csv', stringsAsFactors = F)

fpts.realized.allwks <- list(fpts.realized.week1,fpts.realized.week2,fpts.realized.week3,fpts.realized.week4,fpts.realized.week5,fpts.realized.week6,fpts.realized.week7,
                             fpts.realized.week8,fpts.realized.week9,fpts.realized.week10,fpts.realized.week11)

for (i in 1:length(fpts.realized.allwks)) {
  temp <- fpts.realized.allwks[[i]]
  temp$Player <- sub(' Sr.', '', temp$Player)
  temp$Player <- sub(' Jr.', '', temp$Player)
  fpts.realized.allwks[[i]] <- temp
  write.csv(fpts.realized.allwks[[i]], file = paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", i, ".csv"), row.names = F)
}