#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #######
# In this file we examine look for signs that a cheap player could go off.
# We also add a column ValueWR to cleaned_input_files.


####### WRITE TO FILE? #######
write.bool <- T # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### SET PARAMETERS #######
# week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
week.latest <- 5 # wk we write to will be week.latest+1
slate.days <- "thu-mon" # "thu-mon" or "sun-mon" or ""

salary.threshold <- 5000 # defining cheap
fpts.threshold <- 18.5 # defining cheap + value
historical.threshold <- 18.5 # used for Above.x.Fpts column
num.gm.over.hist.thresh <- 1 # 1 or 2 suggested

# don't edit this
wk <- week.latest + 1 # load this wk's cleaned_input (must be week.latest + 1)


####### PREPARE DATAFRAME OF CHEAP WR THAT GO OFF #######
# Subset using DFN updated files
wr.value <- NULL # init
wr.miss <- NULL # init (cheap that WR that don't add value i.e. suck as they should that week)
for (i in 4:week.latest) {
  assign(paste0("dfn_offense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, ".csv"), stringsAsFactors = F))
  temp <- eval(parse(text=paste0("dfn_offense_week", i)))
  
  temp.hit <- temp[temp$Salary <= salary.threshold & temp$Actual.FP >= fpts.threshold & temp$Pos=='WR', c('Player.Name','Salary','Team','Opp','Vegas.Pts','Projected.Usage','Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]
  temp.hit$Week.Num <- i
  wr.value <- rbind(wr.value, temp.hit) # append
  
  temp.miss <- temp[temp$Salary <= salary.threshold & temp$Actual.FP < fpts.threshold & temp$Pos=='WR', c('Player.Name','Salary','Team','Opp','Vegas.Pts','Projected.Usage','Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]
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

# For each week, print % of value WRs that had at least num.gm.over.hist.thresh games > fpts.threshold
one.game.above.fpts.threshold.wr.value <- as.data.frame(matrix(NA, nrow = length(4:week.latest), ncol = 2, dimnames = list(NULL, c("Week","Pctg.ValueWR.one.game.above.fpts.threshold.wr.value"))))
for (i in 4:week.latest) {
  one.game.above.fpts.threshold.wr.value[i-3,1] <- i # week num
  temp <- wr.value$Above.x.Fpts[wr.value$Week.Num==i]
  one.game.above.fpts.threshold.wr.value[i-3,2] <- sum(temp >= num.gm.over.hist.thresh)/length(temp)
}
plot(one.game.above.fpts.threshold.wr.value, type = 'b')

# For each week, print % of miss WRs that had at least num.gm.over.hist.thresh games > fpts.threshold
one.game.above.fpts.threshold.wr.miss <- as.data.frame(matrix(NA, nrow = length(4:week.latest), ncol = 2, dimnames = list(NULL, c("Week","Pctg.ValueWR.one.game.above.fpts.threshold.wr.miss"))))
for (i in 4:week.latest) {
  one.game.above.fpts.threshold.wr.miss[i-3,1] <- i # week num
  temp <- wr.miss$Above.x.Fpts[wr.miss$Week.Num==i]
  one.game.above.fpts.threshold.wr.miss[i-3,2] <- sum(temp >= num.gm.over.hist.thresh)/length(temp)
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

temp.wr.cheap <- temp[temp$Salary <= salary.threshold & temp$Position=='WR',]
temp.ind <- ncol(temp.wr.cheap)+1
temp.wr.cheap[,temp.ind:(temp.ind+week.latest-1)] <- NA # add extra cols
for (i in temp.ind:(temp.ind+week.latest-1)) {
  colnames(temp.wr.cheap)[i] <- paste0("Week", i-temp.ind+1) # name cols
  temp.wr.cheap[,i] <- historical.fpts[,i-temp.ind+2][match(temp.wr.cheap$Name, historical.fpts$FullName)] # match
}

# for (i in 1:nrow(temp.wr.cheap)) {
#   temp.wr.cheap[i,(temp.ind+week.latest-1):ncol(temp.wr.cheap)] <- "." # get rid of weeks not played yet
# }

# this is hard coded, only will work if adding to current week (need to get above commented code working). NVM should work
for (i in 1:nrow(temp.wr.cheap)) {
  if (sum(temp.wr.cheap[i,temp.ind:(temp.ind+week.latest-1)] > historical.threshold, na.rm = T) >= num.gm.over.hist.thresh) {
    temp.wr.cheap$ValueWR[i] <- 1
  }
}

sum(temp.wr.cheap$ValueWR)
sum(temp.wr.cheap$ValueWR)/nrow(temp.wr.cheap)

# add to temp
temp[temp$Salary <= salary.threshold & temp$Position=='WR','ValueWR'] <- temp.wr.cheap$ValueWR

# write
if (write.bool==T) {
  if (slate.days=="thu-mon") {
    write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk,'/includes_thu-mon/offensive_players.csv'), row.names = F)
  } else if (slate.days=="sun-mon") {
    write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk,'/includes_sun-mon/offensive_players.csv'), row.names = F)
  } else {
    write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', wk,'/offensive_players.csv'), row.names = F)
  }
  
  # also write temp.wr.cheap 1's
  temp.wr.cheap.1s <- temp.wr.cheap[temp.wr.cheap$ValueWR==1,]
  write.csv(temp.wr.cheap.1s, file = paste0("optimizationCode/data_warehouse/valueWR/week",wk,"_valueWR.csv"), row.names = F)
}


#######  ValueWR column to 2016_cleaned_input/all_data #######
# if (slate.days=="thu-mon") {
#   temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/includes_thu-mon/offensive_players.csv'), stringsAsFactors = F) 
# } else if (slate.days=="sun-mon") {
#   temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/includes_sun-mon/offensive_players.csv'), stringsAsFactors = F) 
# } else {
#   temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk, '/offensive_players.csv'), stringsAsFactors = F) 
# }
# temp$ValueWR <- 0 # init
# 
# temp.wr.cheap <- temp[temp$Salary <= salary.threshold & temp$Position=='WR',]
# temp.ind <- ncol(temp.wr.cheap)+1
# temp.wr.cheap[,temp.ind:(temp.ind+week.latest-1)] <- NA # add extra cols
# for (i in temp.ind:(temp.ind+week.latest-1)) {
#   colnames(temp.wr.cheap)[i] <- paste0("Week", i-temp.ind+1) # name cols
#   temp.wr.cheap[,i] <- historical.fpts[,i-temp.ind+2][match(temp.wr.cheap$Name, historical.fpts$FullName)] # match
# }
# 
# # for (i in 1:nrow(temp.wr.cheap)) {
# #   temp.wr.cheap[i,(temp.ind+week.latest-1):ncol(temp.wr.cheap)] <- "." # get rid of weeks not played yet
# # }
# 
# # this is hard coded, only will work if adding to current week (need to get above commented code working). NVM should work
# for (i in 1:nrow(temp.wr.cheap)) {
#   if (sum(temp.wr.cheap[i,temp.ind:(temp.ind+week.latest-1)] > historical.threshold, na.rm = T) >= num.gm.over.hist.thresh) {
#     temp.wr.cheap$ValueWR[i] <- 1
#   }
# }
# 
# # add to temp
# temp[temp$Salary <= salary.threshold & temp$Position=='WR','ValueWR'] <- temp.wr.cheap$ValueWR
# 
# # write
# if (write.bool==T) {
#   if (slate.days=="thu-mon") {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk,'/includes_thu-mon/offensive_players.csv'), row.names = F)
#   } else if (slate.days=="sun-mon") {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk,'/includes_sun-mon/offensive_players.csv'), row.names = F)
#   } else {
#     write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', wk,'/offensive_players.csv'), row.names = F)
#   }
# }


#######  PRINT NUMBER OF VALUEWR #######
paste0("Number of Value WR (has at least game 'num.gm.over.hist.thresh' above 'historical.threshold'): ", sum(temp.wr.cheap$ValueWR))
sum(temp.wr.cheap$ValueWR)/nrow(temp.wr.cheap)





