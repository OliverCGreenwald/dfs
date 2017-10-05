#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we clean various csv files in the optimizationCode folder for ease of use in other scripts.


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
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  # load files
  name <- paste("cleaned_input_offense_wk", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), stringsAsFactors = F))
  
  # remove Sr. and Jr.
  temp <- eval(parse(text=name))
  temp$Name <- sub(' Sr.', '', temp$Name)
  temp$Name <- sub(' Jr.', '', temp$Name)
  
  # write to file
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", i, "/offensive_players.csv"), row.names = F)
}


####### REMOVE FIRST AND LAST NAME FROM CLEANED INPUT OFFENSE CSV #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  # load files
  name <- paste("cleaned_input_offense_wk", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/includes_thu-mon/offensive_players.csv'), stringsAsFactors = F))
  
  # remove Sr. and Jr.
  temp <- eval(parse(text=name))
  temp$Name <- sub(' Sr.', '', temp$Name)
  temp$Name <- sub(' Jr.', '', temp$Name)
  
  # remove players with I, II, III, IV, V, etc suffixes
  
  # write to file
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", i, "/includes_thu-mon/offensive_players.csv"), row.names = F)
}
