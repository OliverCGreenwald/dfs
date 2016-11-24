#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we clean various csv files in the optimizationCode folder for ease of use in other scripts.
# TODO: clean up this hard coded shit


####### REMOVE FIRST AND LAST NAME FROM (ORIGINAL) DFN OFFENSE CSV #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  # load files
  name <- paste("dfn_offense_week", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
  
  # remove Sr. and Jr.
  temp <- eval(parse(text=name))
  temp$Player.Name <- sub(' Sr.', '', temp$Player.Name)
  temp$Player.Name <- sub(' Jr.', '', temp$Player.Name)
  
  # write to file
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week", i, ".csv"), row.names = F)
}


####### REMOVE FIRST AND LAST NAME FROM (UPDATED) DFN OFFENSE CSV #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  # load files
  name <- paste("dfn_offense_updated_week", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
  
  # remove Sr. and Jr.
  temp <- eval(parse(text=name))
  temp$Player.Name <- sub(' Sr.', '', temp$Player.Name)
  temp$Player.Name <- sub(' Jr.', '', temp$Player.Name)
  
  # write to file
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week", i, ".csv"), row.names = F)
}


####### REMOVE FIRST AND LAST NAME FROM CLEANED INPUT OFFENSE CSV #########
fpts.offense.week1 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk1/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week2 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk2/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week3 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk3/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week4 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk4/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week5 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk5/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week6 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk6/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week7 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk7/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week8 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk8/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week9 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk9/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week10 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk10/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week11 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk11/offensive_players.csv', stringsAsFactors = F)

fpts.offense.allwks <- list(fpts.offense.week1,fpts.offense.week2,fpts.offense.week3,fpts.offense.week4,fpts.offense.week5,fpts.offense.week6,fpts.offense.week7,
                            fpts.offense.week8,fpts.offense.week9,fpts.offense.week10,fpts.offense.week11)

for (i in 1:length(fpts.offense.allwks)) {
  temp <- fpts.offense.allwks[[i]]
  temp$Name <- sub(' Sr.', '', temp$Name)
  temp$Name <- sub(' Jr.', '', temp$Name)
  fpts.offense.allwks[[i]] <- temp
  write.csv(fpts.offense.allwks[[i]], file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", i, "/offensive_players.csv"), row.names = F)
}
