#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we clean various csv files in the optimizationCode folder for ease of use in other scripts.

####### REMOVE FIRST AND LAST NAME FROM DFN OFFENSE CSV #########
dfn_offense_week1 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week1.csv", stringsAsFactors = F)
dfn_offense_week2 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week2.csv", stringsAsFactors = F)
dfn_offense_week3 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week3.csv", stringsAsFactors = F)
dfn_offense_week4 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week4.csv", stringsAsFactors = F)
dfn_offense_week5 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week5.csv", stringsAsFactors = F)
dfn_offense_week6 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week6.csv", stringsAsFactors = F)
dfn_offense_week7 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week7.csv", stringsAsFactors = F)

dfn_offense_allwks <- list(dfn_offense_week1,dfn_offense_week2,dfn_offense_week3,dfn_offense_week4,dfn_offense_week5,dfn_offense_week6,dfn_offense_week7)

for (i in 1:length(dfn_offense_allwks)) {
  temp <- dfn_offense_allwks[[i]]
  temp$Player.Name <- sub(' Sr.', '', temp$Player.Name)
  temp$Player.Name <- sub(' Jr.', '', temp$Player.Name)
  dfn_offense_allwks[[i]] <- temp
  write.csv(dfn_offense_allwks[[i]], file = paste0("optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week", i, ".csv"), row.names = F)
}

####### REMOVE FIRST AND LAST NAME FROM DFN UPDATED OFFENSE CSV #########
dfn_offense_week1 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week1.csv", stringsAsFactors = F)
dfn_offense_week2 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week2.csv", stringsAsFactors = F)
dfn_offense_week3 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week3.csv", stringsAsFactors = F)
dfn_offense_week4 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week4.csv", stringsAsFactors = F)
dfn_offense_week5 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week5.csv", stringsAsFactors = F)
dfn_offense_week6 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week6.csv", stringsAsFactors = F)
dfn_offense_week7 <- read.csv(file = "optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week7.csv", stringsAsFactors = F)

dfn_offense_allwks <- list(dfn_offense_week1,dfn_offense_week2,dfn_offense_week3,dfn_offense_week4,dfn_offense_week5,dfn_offense_week6,dfn_offense_week7)

for (i in 1:length(dfn_offense_allwks)) {
  temp <- dfn_offense_allwks[[i]]
  temp$Player.Name <- sub(' Sr.', '', temp$Player.Name)
  temp$Player.Name <- sub(' Jr.', '', temp$Player.Name)
  dfn_offense_allwks[[i]] <- temp
  write.csv(dfn_offense_allwks[[i]], file = paste0("optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week", i, ".csv"), row.names = F)
}

####### REMOVE FIRST AND LAST NAME FROM CLEANED INPUT OFFENSE CSV #########
fpts.offense.week1 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk1/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week2 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk2/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week3 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk3/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week4 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk4/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week5 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk5/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week6 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk6/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week7 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk7/offensive_players.csv', stringsAsFactors = F)

fpts.offense.allwks <- list(fpts.offense.week1,fpts.offense.week2,fpts.offense.week3,fpts.offense.week4,fpts.offense.week5,fpts.offense.week6,fpts.offense.week7)

for (i in 1:length(fpts.offense.allwks)) {
  temp <- fpts.offense.allwks[[i]]
  temp$Name <- sub(' Sr.', '', temp$Name)
  temp$Name <- sub(' Jr.', '', temp$Name)
  fpts.offense.allwks[[i]] <- temp
  write.csv(fpts.offense.allwks[[i]], file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", i, "/offensive_players.csv"), row.names = F)
}
