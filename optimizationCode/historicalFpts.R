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
# We also add historical fpts to the 2016_cleaned_input/all_data folder.


####### WRITE TO FILE? #######
write.bool <- T # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### RUN SECTION #########
section.run <- "1" # 1 (historical fpts df) or 2 (freq ind) # need to run 1 before 2


####### IMPORT LIBRARIES #########
library('stringr')


if (section.run==1) {
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
  historical.fpts.data[historical.fpts.data$Unique.ID=="Adam Thielen@MIN@WR",'Week15'] <- 0
  historical.fpts.data[historical.fpts.data$Unique.ID=="Jermaine Kearse@SEA@WR",'Week15'] <- 0
  
  # add mean column for analysis
  historical.fpts.data$mean <- NA
  for (j in 1:nrow(historical.fpts.data)) {
    historical.fpts.data$mean[j] <- mean(as.numeric(historical.fpts.data[j,2:(week.latest+1)]), na.rm = TRUE)  
  }
  
  
  ####### WRITE DATAFRAME TO FILE #######
  if (write.bool==T) {
    write.csv(historical.fpts.data, file = "optimizationCode/data_warehouse/historical_fpts/historical.fpts.csv", row.names = F) 
  } 
}


if (section.run==2) {
  ####### CREATE DATAFRAME OF INDICATOR FUNCTION AS DEFINED IN DESCRIPTION #######
  # store indicators in new df (note that these will hold indicators for use in the following week!)
  freq.ind.data <- historical.fpts.data
  freq.ind.data$mean <- NULL
  for (i in 2:(week.latest+1)) {
    freq.ind.data[,i] <- 1
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
  
  # WR is 0 if any of the following are true:
  # (1) more than 33% games < 15th percentile fpts (get rid of generally shitty players)
  # (2) less than 20% games > 75th percentile fpts (get rid of players that never go off)
  # (3) last three games < 25th percentile fpts (trend)
  
  # Follow similar conditions for RB and TE (change threshold based on quantiles)
  
  cond1.pct1 <- 1/3
  cond2.pct1 <- 1/5
  
  for (i in 1:week.latest) {
    for (j in 1:nrow(freq.ind.data)) {
      # WRs
      if (freq.ind.data$Pos[j]=="WR") {
        # condition (1)
        temp.cond1 <- historical.fpts.data[j,2:(i+1)] < quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.15))
        num.cond1 <- sum(temp.cond1, na.rm = T)
        
        # condition (2)
        temp.cond2 <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.75))
        num.cond2 <- sum(temp.cond2, na.rm = T)
        
        # condition (3)
        count.not.na <- 0
        ind <- 1
        for (k in (i+1):1) {
          if (!is.na(historical.fpts.data[j,k])) {
            count.not.na <- count.not.na + 1
          }
          if (count.not.na == 3) {
            ind <- k
            break
          }
        }
        temp.cond3 <- historical.fpts.data[j,(i+1):(ind)] > quantile(historical.fpts.data.mean.wr, na.rm = T, c(0.25))
        num.cond3 <- sum(temp.cond3, na.rm = T)
        
        # total games played (so exclude NAs)
        num.games <- length(temp.cond1[!is.na(temp.cond1)])
        
        # set to 0 if a condition is met
        if (num.cond1 >= floor(num.games*cond1.pct1) | num.cond2 <= ceiling(num.games*cond2.pct1) | num.cond3 == 0) {
          freq.ind.data[j,i+1] <- 0
        }
      }
      
      # RBs
      if (freq.ind.data$Pos[j]=="RB") {
        # condition (1)
        temp.cond1 <- historical.fpts.data[j,2:(i+1)] < quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.15))
        num.cond1 <- sum(temp.cond1, na.rm = T)
        
        # condition (2)
        temp.cond2 <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.75))
        num.cond2 <- sum(temp.cond2, na.rm = T)
        
        # condition (3)
        count.not.na <- 0
        ind <- 1
        for (k in (i+1):1) {
          if (!is.na(historical.fpts.data[j,k])) {
            count.not.na <- count.not.na + 1
          }
          if (count.not.na == 3) {
            ind <- k
            break
          }
        }
        temp.cond3 <- historical.fpts.data[j,(i+1):(ind)] > quantile(historical.fpts.data.mean.rb, na.rm = T, c(0.75))
        num.cond3 <- sum(temp.cond3, na.rm = T)
        
        # total games played (so exclude NAs)
        num.games <- length(temp.cond1[!is.na(temp.cond1)])
        
        # set to 0 if a condition is met
        if (num.cond1 >= floor(num.games*cond1.pct1) | num.cond2 <= ceiling(num.games*cond2.pct1) | num.cond3 == 0) {
          freq.ind.data[j,i+1] <- 0
        }
      }
      
      # TEs
      if (freq.ind.data$Pos[j]=="TE") {
        # condition (1)
        temp.cond1 <- historical.fpts.data[j,2:(i+1)] < quantile(historical.fpts.data.mean.te, na.rm = T, c(0.15))
        num.cond1 <- sum(temp.cond1, na.rm = T)
        
        # condition (2)
        temp.cond2 <- historical.fpts.data[j,2:(i+1)] > quantile(historical.fpts.data.mean.te, na.rm = T, c(0.50))
        num.cond2 <- sum(temp.cond2, na.rm = T)
        
        # condition (3)
        count.not.na <- 0
        ind <- 1
        for (k in (i+1):1) {
          if (!is.na(historical.fpts.data[j,k])) {
            count.not.na <- count.not.na + 1
          }
          if (count.not.na == 3) {
            ind <- k
            break
          }
        }
        temp.cond3 <- historical.fpts.data[j,(i+1):(ind)] > quantile(historical.fpts.data.mean.te, na.rm = T, c(0.10))
        num.cond3 <- sum(temp.cond3, na.rm = T)
        
        # total games played (so exclude NAs)
        num.games <- length(temp.cond1[!is.na(temp.cond1)])
        
        # set to 0 if a condition is met
        if (num.cond1 >= floor(num.games*cond1.pct1) | num.cond2 <= ceiling(num.games*cond2.pct1) | num.cond3 == 0) {
          freq.ind.data[j,i+1] <- 0
        }
      }
      
      # set to 0 if no games played
      if (num.games == 0) {
        freq.ind.data[j,i+1] <- 0
      }
    }
  }
  
  # count 1's
  for (i in 2:(week.latest+1)) {
    print(paste0("Count of 1's in Week ", i-1, ": ", sum(freq.ind.data[,i])))
  }
  
  
  ####### ADD FREQUENCY INDICATOR TO 2016_CLEANED_INPUT FILES #########
  # for (i in 2:week.latest+1) { # change to week.latest+1 once current week's data has been scraped
  i <- week.latest + 1  
  
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), stringsAsFactors = F)
  
  # match
  temp$FreqInd <- NA
  temp$FreqInd <- freq.ind.data[match(paste0(temp$Name,'@',temp$Position), paste0(freq.ind.data$FullName,'@',freq.ind.data$Pos)),i] # df is already offset by 1 so don't need i+1
  temp$FreqInd[is.na(temp$FreqInd)] <- 0
  
  print(paste0("Week ", i-1, ": ", sum(temp$FreqInd)))
  print(paste0("Week ", i-1, ": ", sum(freq.ind.data[,i]))) # will be less b/c bye weeks
  
  if (write.bool==T) {
    write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), row.names = F) 
  }
  # }
  
  
  ####### ADD ALL DATA TO 2016_CLEANED_INPUT/ALL_DATA FILES (only run after current week's data is prepared) #########
  # for (i in 2:week.latest) {
  w <- week.latest + 1
  
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', w, '/offensive_players.csv'), stringsAsFactors = F)
  
  # add historical fpts
  # temp[,(ncol(temp)+1):(ncol(temp)+i-1)] <- historical.fpts.data[match(paste0(temp$Name,'@',temp$Position), paste0(freq.ind.data$FullName,'@',freq.ind.data$Pos)), 2:i] # df is already offset by 1 so don't need 2:(i+1)
  temp[,paste0("Week", w-1)] <- historical.fpts.data[match(paste0(temp$Name,'@',temp$Position), paste0(freq.ind.data$FullName,'@',freq.ind.data$Pos)), w]
  
  # re-add FreqInd (this time don't replace NAs with 0s)
  temp$FreqInd <- freq.ind.data[match(paste0(temp$Name,'@',temp$Position), paste0(freq.ind.data$FullName,'@',freq.ind.data$Pos)), w] # df is already offset by 1 so don't need i+1
  
  # write to file
  if (write.bool==T) {
    write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', w, '/offensive_players.csv'), row.names = F) 
  }
  # }
}









####### DEBUGGING #########
# for (i in 2:week.latest+1) {
#   temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), stringsAsFactors = F)
#   
#   print(paste0("Week ",i," count FreqInd==1, RankTargets<=3, Position=='WR': ",nrow(temp[temp$FreqInd==1 & temp$RankTargets<=3 & temp$Position=="WR",])))
#   print(paste0("Week ",i," count FreqInd==1, RankTargets<=3, Position=='RB': ",nrow(temp[temp$FreqInd==1 & temp$RankTargets<=3 & temp$Position=="RB",])))
#   print(paste0("Week ",i," count FreqInd==1, RankTargets<=3, Position=='TE': ",nrow(temp[temp$FreqInd==1 & temp$RankTargets<=3 & temp$Position=="TE",])))
#   
#   print(paste0("Week ",i," count FreqInd==1, Position=='WR': ",nrow(temp[temp$FreqInd==1 & temp$Position=="WR",])))
#   print(paste0("Week ",i," count FreqInd==1, Position=='RB': ",nrow(temp[temp$FreqInd==1 & temp$Position=="RB",])))
#   print(paste0("Week ",i," count FreqInd==1, Position=='TE': ",nrow(temp[temp$FreqInd==1 & temp$Position=="TE",])))
# }
