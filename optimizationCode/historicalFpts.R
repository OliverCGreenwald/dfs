#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we create a dataframe of all weekly historical fpts for each player. We then add a column
# to 2016_CLEANED_INPUT files that is in indicator function that takes on
# value 1 if:
# (1) at least 25% games > 99.9 percentile fpts OR
# (2) at least 25% games > 90 percentile fpts, at least 50% games > 75 percentile fpts, at most 25% games < 50 percentile fpts
# (3) last 3 weeks all > 99.9 percentile fpts [not implemented yet]
# and value 0 otherwise.


####### IMPORT LIBRARIES #########
library('stringr')


####### LOAD DFN FILES (UPDATES FOLDER B/C WE NEED HISTORICAL ACTUAL FPTS) #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
# player.names.qbs <- c() # record QBs for adding to player.names
player.names <- c()
for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
  
  # add week column
  temp.df <- eval(parse(text=name))
  temp.df$Week.Num <- i
  
  # clean names
  # temp.df$Player.Name <- sub("'", "", temp.df$Player.Name)
  
  # add Unique.ID column for matching purposes
  temp.df$Unique.ID <- paste0(temp.df$Player.Name, '@', temp.df$Team, '@', temp.df$Pos)
  
  # concatenate QBs who played this week to player.names.qbs
  # player.names.qbs <- c(player.names.qbs, temp.df[temp.df$Pos=='QB','Player.Name'])
  
  #
  player.names <- c(player.names, temp.df$Unique.ID)
  
  # assign
  assign(name, temp.df)
}


####### CREATE DATAFRAME OF ALL HISTORICAL ACTUAL FPTS #######
player.names <- unique(player.names)

# intialize df and add player names
historical.fpts.data <- as.data.frame(matrix(data = NA, nrow = length(player.names), ncol = week.latest+1))
historical.fpts.data[,1] <- unique(player.names)

# add column names to df
colnames(historical.fpts.data)[1] <- "Unique.ID"
for (i in 2:(week.latest+1)) {
  colnames(historical.fpts.data)[i] <- paste0("Week", i-1)
}

# split Unique.ID column (using @ symbol) for later use
historical.fpts.data$FullName <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,1]
historical.fpts.data$Team <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,2]
historical.fpts.data$Pos <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,3]

# match
for (i in 2:(week.latest+1)) {
  dfn.df <- eval(parse(text=paste("dfn_offense_week", i-1, sep = "")))
  historical.fpts.data[,i] <- dfn.df$Actual.FP[match(historical.fpts.data$Unique.ID, dfn.df$Unique.ID)]
}

# # load list of player names (use this b/c players vary week to week. this aggregated all of them) # wait, why didn't we just do unique() on the DFN
# load(file = "projectionsAnalysis/player.names.RData")
# # also add in QB's
# player.names.qbs
# player.names <- c(player.names, unique(player.names.qbs))
# player.names
# unique(player.names)
# 
# # intialize df and add player names
# historical.fpts.data <- as.data.frame(matrix(data = NA, nrow = length(player.names), ncol = week.latest+1))
# historical.fpts.data[,1] <- player.names
# 
# # add column names to df
# colnames(historical.fpts.data)[1] <- "Unique.ID"
# for (i in 2:(week.latest+1)) {
#   colnames(historical.fpts.data)[i] <- paste0("Week", i-1)
# }
# 
# # split Unique.ID column (using @ symbol) for matching purposes
# historical.fpts.data$FullName <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,1]
# historical.fpts.data$Team <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,2]
# historical.fpts.data$Pos <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,3]
# 
# # further split FullName column (using ", ") for matching purposes
# historical.fpts.data$Last.Name <- str_split_fixed(historical.fpts.data$FullName, ", ", 2)[,1]
# historical.fpts.data$First.Name <- str_split_fixed(historical.fpts.data$FullName, ", ", 2)[,2]
# 
# # replace FB with RB in Pos column for matching
# historical.fpts.data$Pos[historical.fpts.data$Pos=="FB"] <- "RB"
# 
# # replace Unique.ID for matching
# historical.fpts.data$Unique.ID <- paste0(historical.fpts.data$First.Name," ",historical.fpts.data$Last.Name, '@', historical.fpts.data$Team, '@', historical.fpts.data$Pos)
# 
# # match
# for (i in 2:(week.latest+1)) {
#   dfn.df <- eval(parse(text=paste("dfn_offense_week", i-1, sep = "")))
#   historical.fpts.data[,i] <- dfn.df$Actual.FP[match(historical.fpts.data$Unique.ID, dfn.df$Unique.ID)]
# }

# injuries
historical.fpts.data[historical.fpts.data$Unique.ID=="LeSean McCoy@BUF@RB",'Week8'] <- NA
historical.fpts.data[historical.fpts.data$Unique.ID=="Steve Smith@BAL@WR",'Week6'] <- NA
historical.fpts.data[historical.fpts.data$Unique.ID=="Steve Smith@BAL@WR",'Week7'] <- NA
historical.fpts.data[historical.fpts.data$Unique.ID=="Eric Ebron@DET@TE",'Week5'] <- NA
historical.fpts.data[historical.fpts.data$Unique.ID=="Eric Ebron@DET@TE",'Week6'] <- NA
historical.fpts.data[historical.fpts.data$Unique.ID=="Eric Ebron@DET@TE",'Week7'] <- NA

# actually, let's just set all 0's to NA's b/c we don't have injury data
is.na(historical.fpts.data[,2:(week.latest+1)]) <- !historical.fpts.data[,2:(week.latest+1)]

# replace NA's with 0's for any game we know player actually got 0 fpts
historical.fpts.data[historical.fpts.data$Unique.ID=="Brandin Cooks@NO@WR",'Week12'] <- 0

# add mean column for analysis
historical.fpts.data$mean <- NA
for (j in 1:nrow(historical.fpts.data)) {
  historical.fpts.data$mean[j] <- mean(as.numeric(historical.fpts.data[j,2:(week.latest+1)]), na.rm = TRUE)  
}


####### WRITE DATAFRAME TO FILE #######
write.csv(historical.fpts.data, file = "optimizationCode/data_warehouse/historical_fpts/historical.fpts.csv", row.names = F)


####### CREATE DATAFRAME OF INDICATOR FUNCTION AS DEFINED IN DESCRIPTION #######
# store indicators in new df (note that these will hold indicators for use in the following week!)
freq.ind.data <- historical.fpts.data
freq.ind.data$mean <- NULL
for (i in 2:(week.latest+1)) {
  freq.ind.data[,i] <- 0
}

# look at each position to help set thresholds for frequency indicator function
historical.fpts.data.mean.wr <- historical.fpts.data$mean[historical.fpts.data$Pos=="WR"]
historical.fpts.data.mean.rb <- historical.fpts.data$mean[historical.fpts.data$Pos=="RB"]
historical.fpts.data.mean.te <- historical.fpts.data$mean[historical.fpts.data$Pos=="TE"]

mean.wr <- mean(historical.fpts.data.mean.wr, na.rm = TRUE) # note: taking mean of mean
mean.rb <- mean(historical.fpts.data.mean.rb, na.rm = TRUE)
mean.te <- mean(historical.fpts.data.mean.te, na.rm = TRUE)

sd.wr <- sd(historical.fpts.data.mean.wr, na.rm = TRUE)
sd.rb <- sd(historical.fpts.data.mean.rb, na.rm = TRUE)
sd.te <- sd(historical.fpts.data.mean.te, na.rm = TRUE)

count.wr <- length(historical.fpts.data.mean.wr) - sum(is.na(historical.fpts.data.mean.wr))
count.rb <- length(historical.fpts.data.mean.rb) - sum(is.na(historical.fpts.data.mean.rb))
count.te <- length(historical.fpts.data.mean.te) - sum(is.na(historical.fpts.data.mean.te))

par(mfrow=c(1,3))
hist(historical.fpts.data.mean.wr, main = paste0("WR, (mean = ",format(round(mean.wr, 2), nsmall = 2),", sd = ",format(round(sd.wr, 2), nsmall = 2),")"), xlab = "Avg Fpts", ylab = paste0("Frequency (total count: ",count.wr,")"))
hist(historical.fpts.data.mean.rb, main = paste0("RB, (mean = ",format(round(mean.rb, 2), nsmall = 2),", sd = ",format(round(sd.rb, 2), nsmall = 2),")"), xlab = "Avg Fpts", ylab = paste0("Frequency (total count: ",count.rb,")"))
hist(historical.fpts.data.mean.te, main = paste0("TE, (mean = ",format(round(mean.te, 2), nsmall = 2),", sd = ",format(round(sd.te, 2), nsmall = 2),")"), xlab = "Avg Fpts", ylab = paste0("Frequency (total count: ",count.te,")"))

quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95, 0.99))
quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95, 0.99))
quantile(historical.fpts.data.mean.te, na.rm = T, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95, 0.99))

# WR is 1 if
# (1) at least 25% games > 23 fpts (99.9 pct) OR
# (2) at least 25% games > 15 fpts (90 pct), at least 50% games > 12 fpts (75 pct), at most 25% games < 7.5 fpts (50 pct)
# (3) last 3 weeks all > 25 fpts (99.9 pct) [not implemented yet]

# Follow similar conditions for RB and TE (change threshold based on quantiles)

cond1.pct1 <- 1/4
cond2.pct1 <- 1/4
cond2.pct2 <- 1/2
cond2.pct3 <- 1/4

for (i in 1:week.latest) {
  for (j in 1:nrow(freq.ind.data)) {
    # WRs
    if (freq.ind.data$Pos[j]=="WR") {
      # for condition (1)
      temp.override <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.99)) # 23 # ~99.9th percentile
      num.override <- sum(temp.override, na.rm = T)
      
      # for condition (2)
      temp.great <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.85)) # 15 # ~90th percentile
      num.great <- sum(temp.great, na.rm = T)
      temp.good <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.50)) # 12 # ~75th percentile
      num.good <- sum(temp.good, na.rm = T)
      temp.bad <- historical.fpts.data[j,2:(i+1)] < quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.25)) # 7.5 # ~50th percentile
      num.bad <- sum(temp.bad, na.rm = T)
      # temp.useless <- historical.fpts.data[j,2:(week.latest+1)] < 5.0
      # num.useless <- sum(temp.useless, na.rm = T)
      
      # total games played (so exclude NAs)
      num.total <- length(temp.good) - sum(is.na(temp.good))
      
      # set to 1 if a condition is met
      if (num.override >= num.total*cond1.pct1 | (num.great >= num.total*cond2.pct1 & num.good >= num.total*cond2.pct2 & num.bad <= num.total*cond2.pct3)) { #& num.useless <= ceiling(num.total*1/4)) {
        freq.ind.data[j,i+1] <- 1
      }
    }
    
    # RBs
    if (freq.ind.data$Pos[j]=="RB") {
      # for condition (1)
      temp.override <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.99)) # 23 # ~99.9th percentile
      num.override <- sum(temp.override, na.rm = T)
      
      # for condition (2)
      temp.great <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.85)) # 15.5 # ~90th percentile
      num.great <- sum(temp.great, na.rm = T)
      temp.good <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.60)) # 11 # ~75th percentile
      num.good <- sum(temp.good, na.rm = T)
      temp.bad <- historical.fpts.data[j,2:(i+1)] < quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.35)) # 6 # ~50th percentile
      num.bad <- sum(temp.bad, na.rm = T)
      # temp.useless <- historical.fpts.data[j,2:(week.latest+1)] < 5.0
      # num.useless <- sum(temp.useless, na.rm = T)
      
      # total games played (so exclude NAs)
      num.total <- length(temp.good) - sum(is.na(temp.good))
      
      # set to 1 if a condition is met
      if (num.override >= num.total*cond1.pct1 | (num.great >= num.total*cond2.pct1 & num.good >= num.total*cond2.pct2 & num.bad <= num.total*cond2.pct3)) { #& num.useless <= ceiling(num.total*1/4)) {
        freq.ind.data[j,i+1] <- 1
      }
    }
    
    # TEs
    if (freq.ind.data$Pos[j]=="TE") {
      # for condition (1)
      temp.override <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.te, na.rm = T, c(0.99)) # 16 # ~99.9th percentile
      num.override <- sum(temp.override, na.rm = T)
      
      # for condition (2)
      temp.great <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.te, na.rm = T, c(0.85)) #12 # ~90th percentile
      num.great <- sum(temp.great, na.rm = T)
      temp.good <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.te, na.rm = T, c(0.60)) # 9 # ~75th percentile
      num.good <- sum(temp.good, na.rm = T)
      temp.bad <- historical.fpts.data[j,2:(i+1)] < quantile(historical.fpts.data.mean.te, na.rm = T, c(0.25)) # 5.5 # ~50th percentile
      num.bad <- sum(temp.bad, na.rm = T)
      # temp.useless <- historical.fpts.data[j,2:(week.latest+1)] < 5.0
      # num.useless <- sum(temp.useless, na.rm = T)
      
      # total games played (so exclude NAs)
      num.total <- length(temp.good) - sum(is.na(temp.good))
      
      # set to 1 if a condition is met
      if (num.override >= num.total*cond1.pct1 | (num.great >= num.total*cond2.pct1 & num.good >= num.total*cond2.pct2 & num.bad <= num.total*cond2.pct3)) { #& num.useless <= ceiling(num.total*1/4)) {
        freq.ind.data[j,i+1] <- 1
      }
    }
    
    # set to 0 if no games played
    if (num.total == 0) {
      freq.ind.data[j,i+1] <- 0
    }
  }  
}

# count 1's
for (i in 2:(week.latest+1)) {
  print(paste0("Count of 1's in Week ", i-1, ": ", sum(freq.ind.data[,i])))
}


####### ADD FREQUENCY INDICATOR TO 2016_CLEANED_INPUT FILES #########
for (i in 2:week.latest) { # change to week.latest+1 once current week's data has been scraped
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), stringsAsFactors = F)
  
  # # clean names
  # temp$Name.Clean <- sub("'", "", temp$Name)

  # match
  temp$FreqInd <- NA
  temp$FreqInd <- freq.ind.data[match(paste0(temp$Name,'@',temp$Position), paste0(freq.ind.data$FullName,'@',freq.ind.data$Pos)),i] # df is already offset by 1 so don't need i+1
  temp$FreqInd[is.na(temp$FreqInd)] <- 0
  
  print(paste0("Week ", i-1, ": ", sum(temp$FreqInd)))
  print(paste0("Week ", i-1, ": ", sum(freq.ind.data[,i]))) # will be less b/c bye weeks
  
  write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), row.names = F)
}


####### ADD ALL DATA TO 2016_CLEANED_INPUT/ALL_DATA FILES #########
for (i in 2:week.latest) {
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), stringsAsFactors = F)
  
  # # clean names
  # temp$Name.Clean <- sub("'", "", temp$Name)
  
  # match
  temp[,(ncol(temp)+1):(ncol(temp)+i-1)] <- historical.fpts.data[match(paste0(temp$Name,'@',temp$Position), paste0(freq.ind.data$FullName,'@',freq.ind.data$Pos)), 2:i] # df is already offset by 1 so don't need 2:(i+1)
  write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), row.names = F)
}









####### DEBUGGING #########
for (i in 2:week.latest+1) {
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), stringsAsFactors = F)
  # print(paste0("Week ",i," count FreqInd==1, RankTargets<=3, Position=='WR': ",nrow(temp[temp$FreqInd==1 & temp$RankTargets<=3 & temp$Position=="WR",])))
  # print(paste0("Week ",i," count FreqInd==1, RankTargets<=3, Position=='RB': ",nrow(temp[temp$FreqInd==1 & temp$RankTargets<=3 & temp$Position=="RB",])))
  # print(paste0("Week ",i," count FreqInd==1, RankTargets<=3, Position=='TE': ",nrow(temp[temp$FreqInd==1 & temp$RankTargets<=3 & temp$Position=="TE",])))
  
  print(paste0("Week ",i," count FreqInd==1, Position=='WR': ",nrow(temp[temp$FreqInd==1 & temp$Position=="WR",])))
  print(paste0("Week ",i," count FreqInd==1, Position=='RB': ",nrow(temp[temp$FreqInd==1 & temp$Position=="RB",])))
  print(paste0("Week ",i," count FreqInd==1, Position=='TE': ",nrow(temp[temp$FreqInd==1 & temp$Position=="TE",])))
}
