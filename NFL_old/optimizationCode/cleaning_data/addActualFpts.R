#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# Add floor, ceiling, and actual fpts to the 2016_cleaned_input (offense and defense) and 2016_cleaned_input/all_data (just offense) folders.


####### SET WEEK #########
# week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
week.latest <- 15
i <- week.latest


####### LOAD DFN FILES #########
#--- offense ---#
# assign(paste0("dfn_offense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
assign(paste0("dfn_offense_updates_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, '.csv'), stringsAsFactors = F))

#--- defense ---#
# assign(paste0("dfn_defense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_defense_week', i, '.csv'), stringsAsFactors = F))
assign(paste0("dfn_defense_updates_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week', i, '.csv'), stringsAsFactors = F))


####### ADD FLOOR, CEIL, ACTUAL (only run after current week's data is prepared) #########
#--- 2016_cleaned_input (offense) ---#
temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), stringsAsFactors = F)
temp.dfn <- eval(parse(text=paste0("dfn_offense_updates_week", i)))

# add Actual
temp$Actual <- temp.dfn$Actual.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]
temp$Actual[is.na(temp$Actual)==T] <- 0 # replace NA's with 0's b/c can't run julia with NA's

# write to file
write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), row.names = F)

#--- 2016_cleaned_input (defense) ---#
temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/defenses.csv'), stringsAsFactors = F)
temp.dfn <- eval(parse(text=paste0("dfn_defense_updates_week", i)))

# name cleaning
temp$Team <- toupper(temp$Team)

# add Actual
temp$Actual <- temp.dfn$Actual.FP[match(temp$Team, temp.dfn$Team)]
# temp$Actual[is.na(temp$Actual)==T] <- 0 # replace NA's with 0's b/c can't run julia with NA's (catch NAs)

# write to file
write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/defenses.csv'), row.names = F)

#--- 2016_cleaned_input/all_data (offense) ---#
temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), stringsAsFactors = F)
temp.dfn <- eval(parse(text=paste0("dfn_offense_updates_week", i)))

# add Projection_dfn_floor, Projection_dfn_ceiling, Actual
temp$Projection_dfn_floor <- temp.dfn$Floor.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]
temp$Projection_dfn_ceiling <- temp.dfn$Ceil.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]
temp$Actual <- temp.dfn$Actual.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]

# write to file
write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), row.names = F)
