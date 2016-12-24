#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #######
# In this file we examine look for signs that a cheap player could go off.
# We also add a column ValueWR to cleaned_input_files


####### WRITE TO FILE? #######
write.bool <- F # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### SET PARAMETERS #######
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
salary.threshold <- 4500 # defining cheap
fpts.threshold <- 18 # defining cheap + value
historical.threshold <- 15 # used for Above.x.Fpts column

slate.days <- "sun-mon" # "thu-mon" or "sun-mon" or "" (for writing to file only)
wk <- 16 # (for writing to file only)


####### PREPARE DATAFRAME OF CHEAP WR THAT GO OFF #######
# Subset using DFN updated files
wr.value <- NULL # init
wr.miss <- NULL # init (cheap that WR that don't add value i.e. suck as they should that week)
for (i in 4:week.latest) {
  assign(paste0("dfn_offense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, ".csv"), stringsAsFactors = F))
  temp <- eval(parse(text=paste0("dfn_offense_week", i)))
  
  temp.hit <- temp[temp$Salary < salary.threshold & temp$Actual.FP >= fpts.threshold & temp$Pos=='WR', c('Player.Name','Salary','Team','Opp','Vegas.Pts','Projected.Usage','Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]
  temp.hit$Week.Num <- i
  wr.value <- rbind(wr.value, temp.hit) # append
  
  temp.miss <- temp[temp$Salary < salary.threshold & temp$Actual.FP < fpts.threshold & temp$Pos=='WR', c('Player.Name','Salary','Team','Opp','Vegas.Pts','Projected.Usage','Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]
  temp.miss$Week.Num <- i
  wr.miss <- rbind(wr.miss, temp.miss) # append
  
  print(i)
}

# Load fantasydata salaries files for opp def rankings
fantasydata.salaries.all.weeks <- NULL
for (i in 4:week.latest) {
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/fantasydata/salaries/fantasydata_salaries_2016_week', i, ".csv"), stringsAsFactors = F)
  fantasydata.salaries.all.weeks <- rbind(fantasydata.salaries.all.weeks, temp)
  print(i)
}

# Add Opp.Rank and Opp.Pos.Rank columns
wr.value$Opp.Rank <- as.numeric(fantasydata.salaries.all.weeks$Opp.Rank[match(paste0(wr.value$Player.Name,wr.value$Week.Num), paste0(fantasydata.salaries.all.weeks$Player,fantasydata.salaries.all.weeks$Week))])
wr.value$Opp.Pos.Rank <- as.numeric(fantasydata.salaries.all.weeks$Opp.Pos.Rank[match(paste0(wr.value$Player.Name,wr.value$Week.Num), paste0(fantasydata.salaries.all.weeks$Player,fantasydata.salaries.all.weeks$Week))])

# Add historical fpts
historical.fpts <- read.csv(file = "optimizationCode/data_warehouse/historical_fpts/historical.fpts.csv", stringsAsFactors = F)
wr.value[,14:(14+week.latest-1)] <- NA # add extra cols
for (i in 14:(14+week.latest-1)) {
  colnames(wr.value)[i] <- paste0("Week", i-13) # name cols
  wr.value[,i] <- historical.fpts[,i-12][match(wr.value$Player.Name, historical.fpts$FullName)] # match
}
for (i in 1:nrow(wr.value)) {
  wr.value[i,(13+wr.value$Week.Num[i]):ncol(wr.value)] <- "." # get rid of weeks not played yet
}

# Add Opp.Rank and Opp.Pos.Rank columns
wr.miss$Opp.Rank <- as.numeric(fantasydata.salaries.all.weeks$Opp.Rank[match(paste0(wr.miss$Player.Name,wr.miss$Week.Num), paste0(fantasydata.salaries.all.weeks$Player,fantasydata.salaries.all.weeks$Week))])
wr.miss$Opp.Pos.Rank <- as.numeric(fantasydata.salaries.all.weeks$Opp.Pos.Rank[match(paste0(wr.miss$Player.Name,wr.miss$Week.Num), paste0(fantasydata.salaries.all.weeks$Player,fantasydata.salaries.all.weeks$Week))])

# Add historical fpts
wr.miss[,14:(14+week.latest-1)] <- NA # add extra cols
for (i in 14:(14+week.latest-1)) {
  colnames(wr.miss)[i] <- paste0("Week", i-13) # name cols
  wr.miss[,i] <- historical.fpts[,i-12][match(wr.miss$Player.Name, historical.fpts$FullName)] # match
}
for (i in 1:nrow(wr.miss)) {
  wr.miss[i,(13+wr.miss$Week.Num[i]):ncol(wr.miss)] <- "." # get rid of weeks not played yet
}

####### ANALYSIS #######
# Plot histogram of opp def rank to see if that matters (doesn't seem to matter)
hist(wr.value$Opp.Rank)
hist(wr.value$Opp.Pos.Rank)

# Add col with number of historical games > historical.threshold fpts
for (i in 1:nrow(wr.value)) {
  wr.value$Above.x.Fpts[i] <- sum(as.numeric(wr.value[i,14:(14+wr.value$Week.Num[i]-2)]) > historical.threshold, na.rm = T)
}
plot(wr.value$Above.x.Fpts, type = 'b')

for (i in 1:nrow(wr.miss)) {
  wr.miss$Above.x.Fpts[i] <- sum(as.numeric(wr.miss[i,14:(14+wr.miss$Week.Num[i]-2)]) > historical.threshold, na.rm = T)
}

# For each week, print % of value WRs that had at least one game > fpts.threshold
one.game.above.fpts.threshold.wr.value <- as.data.frame(matrix(NA, nrow = length(4:week.latest), ncol = 2, dimnames = list(NULL, c("Week","Pctg.ValueWR.one.game.above.fpts.threshold.wr.value"))))
for (i in 4:week.latest) {
  one.game.above.fpts.threshold.wr.value[i-3,1] <- i # week num
  temp <- wr.value$Above.x.Fpts[wr.value$Week.Num==i]
  one.game.above.fpts.threshold.wr.value[i-3,2] <- sum(temp > 0)/length(temp)
}
plot(one.game.above.fpts.threshold.wr.value, type = 'b')

# For each week, print % of miss WRs that had at least one game > fpts.threshold
one.game.above.fpts.threshold.wr.miss <- as.data.frame(matrix(NA, nrow = length(4:week.latest), ncol = 2, dimnames = list(NULL, c("Week","Pctg.ValueWR.one.game.above.fpts.threshold.wr.miss"))))
for (i in 4:week.latest) {
  one.game.above.fpts.threshold.wr.miss[i-3,1] <- i # week num
  temp <- wr.miss$Above.x.Fpts[wr.miss$Week.Num==i]
  one.game.above.fpts.threshold.wr.miss[i-3,2] <- sum(temp > 0)/length(temp)
}
plot(one.game.above.fpts.threshold.wr.miss, type = 'b')


#######  ValueWR column to 2016_cleaned_input #######
if (slate.days=="thu-mon") {
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk, '/includes_thu-mon/offensive_players.csv'), stringsAsFactors = F) 
} else if (slate.days=="sun-mon") {
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk, '/includes_sun-mon/offensive_players.csv'), stringsAsFactors = F) 
} else {
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk, '/offensive_players.csv'), stringsAsFactors = F) 
}
temp$ValueWR <- 0 # init
temp.wr.cheap <- temp[temp$Salary < salary.threshold & temp$Position=='WR',]

wr.value[,14:(14+week.latest-1)] <- NA # add extra cols
for (i in 14:(14+week.latest-1)) {
  colnames(wr.value)[i] <- paste0("Week", i-13) # name cols
  wr.value[,i] <- historical.fpts[,i-12][match(wr.value$Player.Name, historical.fpts$FullName)] # match
}
for (i in 1:nrow(wr.value)) {
  wr.value[i,(13+wr.value$Week.Num[i]):ncol(wr.value)] <- "." # get rid of weeks not played yet
}

# # write
# if (write.bool==T) {
#   if (slate.days=="thu-mon") {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk,'/includes_thu-mon/offensive_players.csv'), row.names = F) 
#   } else if (slate.days=="sun-mon") {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk,'/includes_sun-mon/offensive_players.csv'), row.names = F) 
#   } else {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk,'/offensive_players.csv'), row.names = F) 
#   } 
# }










# # Adding ValueWR column to 2016_cleaned_input/all_data
# if (slate.days=="thu-mon") {
#   temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/includes_thu-mon/offensive_players.csv'), stringsAsFactors = F)
# } else if (slate.days=="sun-mon") {
#   temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/includes_sun-mon/offensive_players.csv'), stringsAsFactors = F)
# } else {
#   temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/offensive_players.csv'), stringsAsFactors = F)
# }
# 
# if (write.bool==T) {
#   if (slate.days=="thu-mon") {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/includes_thu-mon/offensive_players.csv'), row.names = F)
#   } else if (slate.days=="sun-mon") {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/includes_sun-mon/offensive_players.csv'), row.names = F)
#   } else {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/offensive_players.csv'), row.names = F)
#   } 
# }
