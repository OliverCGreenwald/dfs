# This file used for adding necessary columns after creating our_edits folders in weeks 12 and 15
wk <- 15
temp.orig <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), stringsAsFactors = F)
temp.edit <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/our_edits/offensive_players.csv"), stringsAsFactors = F)

sum(temp.orig$Name...ID!=temp.edit$Name...ID) # should be 0

temp.orig$Name <- temp.edit$Name # gets rid of jr and sr

# add columns
temp.orig$RankTargets <- temp.edit$RankTargets
temp.orig$RollingTargetPctg <- temp.edit$RollingTargetPctg
temp.orig$Actual <- temp.edit$Actual
temp.orig$ValueWR <- temp.edit$ValueWR

write.csv(temp.orig, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), row.names = F)
