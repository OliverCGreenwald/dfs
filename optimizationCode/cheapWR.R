#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #######
# In this file we examine look for signs that a cheap player could go off.


####### WRITE TO FILE? #######
write.bool <- F # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### Set Parameters #######
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1


####### LOAD DFN FILES #######
player.performance <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', week.num, ".csv"), stringsAsFactors = F)




