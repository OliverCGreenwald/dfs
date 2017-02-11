#setwd("~/Projects/DFS")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we predict the ownership rate of each player. For sunday contests only (b/c we don't
# have a full season of thu-mon or sun-mon contest results, so Ownership.Pctg can't be computed);
# currently only supports $3 sunday play action contest. Offense only. Note that we remove players with
# 0% ownership from the dataset.
#
# Notes:
# - R^2 = 1- SSR/SST only definitively true for training set, so interpret with caution for testing set
# - if use index 99 of lambdas (i.e. very low penalty) for lasso, the two nonzero coefficients are "Likes" and "Fpts.Week.1lag"
#
# TODO:
# - get rid of negative predictions
# - GPD

####### IMPORT LIBRARIES #########
library('stringr')
library("glmnet")
library("glmnetUtils")
library("kernlab")


####### SET MODEL TO RUN #######
model.run <- "4"
week.min <- 4 # must be >= 4 (this is the week we begin appending weekly data for the overall dataset, "dataset.all")
week.max <- 6 # for loop only


####### WRITE TO FILE? #######
write.bool <- F


####### SET PARAMETERS #######
positions <- c("QB", "RB", "WR", "TE", "DST") # we subset data to only include these positions
contest.name <- "playaction_contest" # "playaction_contest" or "millymaker_contest"

historicalfpts.lag <- 3 # num wks of lagged historical fpts (use 3 for weeks 7-16, 1 for weeks 2-6, NA if not using this)

fantasydata.snapcounts.bool <- F # TRUE if want to add features from fantasydata/snapcounts (caution: lots of NAs, rows with NAs are removed)
fantasydata.stats.bool <- F # TRUE if want to add features from fantasydata/stats (caution: lots of NAs, rows with NAs are removed)
lag.num <- 3 # number of weeks lag (used for fantasydata/snapcounts)

outputweekly.bool <- T # TRUE if want to output weekly csv's (otherwise outputs all weeks combined csv). Note this can override all other boolean parameters in this section.


####### SECTION I: PREPARE DATAFRAME #######
#---- Initializing df for storing all weeks (if loop over weeks is used) ----#
dataset.all <- NULL
dataset.all.copy <- NULL # copy that keeps name and actual fpts (used for analyzing false positives)

for (wk in week.min:week.max) { # uncomment for loop if don't want to loop over weeks
  #---- Read and clean DFN features ----#
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', wk, ".csv"), stringsAsFactors = F)
  
  temp <- temp[-c(which(temp$Inj=="O")),] # remove players that are Out
  temp$Inj[temp$Inj!="Q"] <- 0 # temp$Inj <- NULL
  temp$Inj[temp$Inj=="Q"] <- 1
  temp$Inj <- as.numeric(temp$Inj)
  
  temp$Likes[is.na(temp$Likes)] <- 0
  temp$Defense.Pass.Yds.G <- as.numeric(sub("%", "", temp$Defense.Pass.Yds.G))*0.01
  temp$Defense.Rush.Yds.G <- as.numeric(sub("%", "", temp$Defense.Rush.Yds.G))*0.01
  temp$DvP <- as.numeric(sub("%", "", temp$DvP))*0.01
  
  #---- Subset by position ----#
  if (historicalfpts.lag==3 | is.na(historicalfpts.lag)==T) {
    temp.df <- temp[which(temp$Pos %in% positions), c('Player.Name','Likes','Inj','Pos','Salary','Team','Opp','Vegas.Pts','Defense.Pass.Yds.G','Defense.Rush.Yds.G','DvP','L3.Rush.Att','S.Rush.Att','Proj.Rush.Att','Red.Zone.Rush.Att','Yards.Per.Rush.Att', "L3.Targets", "S.Targets", "Proj.Targets", "Red.Zone.Targets", "Yards.Per.Target", "L3.FP", "S.FP", 'Projected.Usage','Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]
  } else {
    temp.df <- temp[which(temp$Pos %in% positions), c('Player.Name','Likes','Inj','Pos','Salary','Team','Opp','Vegas.Pts','Defense.Pass.Yds.G','Defense.Rush.Yds.G','DvP','S.Rush.Att','Red.Zone.Rush.Att','Yards.Per.Rush.Att', "L3.Targets", "S.Targets", "Red.Zone.Targets", "Yards.Per.Target", "L3.FP", "S.FP",'Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]
  }
  
  #---- Clean temp.df player names for matching fantasydata names ----#
  temp.df$Player.Name.Temp <- sub("'", "", temp.df$Player.Name)
  temp.df$Player.Name.Temp <- sub(' Sr.', '', temp.df$Player.Name.Temp)
  temp.df$Player.Name.Temp <- sub(' Jr.', '', temp.df$Player.Name.Temp)
  temp.df$Player.Name.Temp <- gsub("[.]","",temp.df$Player.Name.Temp) # remove periods
  temp.df$Player.Name.Temp <- tolower(temp.df$Player.Name.Temp)
  
  # Load and clean fantasydata salaries files for opp defense rankings
  temp.fantasydata <- read.csv(file = paste0('optimizationCode/data_warehouse/fantasydata/salaries/fantasydata_salaries_2016_week', wk, ".csv"), stringsAsFactors = F)
  temp.fantasydata$Player <- sub("'", "", temp.fantasydata$Player)
  temp.fantasydata$Player <- sub(' Sr.', '', temp.fantasydata$Player)
  temp.fantasydata$Player <- sub(' Jr.', '', temp.fantasydata$Player)
  temp.fantasydata$Player <- tolower(temp.fantasydata$Player)
  
  # Add Opp.Rank and Opp.Pos.Rank columns to temp.df
  temp.df$Opp.Rank <- as.numeric(temp.fantasydata$Opp.Rank[match(temp.df$Player.Name.Temp, temp.fantasydata$Player)])
  temp.df$Opp.Pos.Rank <- as.numeric(temp.fantasydata$Opp.Pos.Rank[match(temp.df$Player.Name.Temp, temp.fantasydata$Player)])
  
  #---- Add historical fpts ----#
  historical.fpts <- read.csv(file = "optimizationCode/data_warehouse/historical_fpts/historical.fpts.csv", stringsAsFactors = F)
  if (historicalfpts.lag > 1) {
    temp.ind <- ncol(temp.df) + 1
    temp.df[,temp.ind:(temp.ind+(historicalfpts.lag-1))] <- NA # add extra cols
    for (i in 1:historicalfpts.lag) {
      colnames(temp.df)[temp.ind-1+i] <- paste0("Wk.Lag", i, ".Fpts")
      historical.fpts[is.na(historical.fpts[,wk-i+1]),wk-i+1] <- 0 # set NA's to 0 (note that we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0)
      temp.df[,temp.ind-1+i] <- historical.fpts[,wk-i+1][match(temp.df$Player.Name, historical.fpts$FullName)] # match
    }
  } else if (historicalfpts.lag==1) {
    temp.ind <- ncol(temp.df) + 1
    temp.df[,temp.ind:(temp.ind+0)] <- NA # add extra cols
    for (i in 1:1) {
      colnames(temp.df)[temp.ind-1+i] <- paste0("Wk.Lag", i, ".Fpts")
      historical.fpts[is.na(historical.fpts[,wk-i+1]),wk-i+1] <- 0 # set NA's to 0 (note that we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0)
      temp.df[,temp.ind-1+i] <- historical.fpts[,wk-i+1][match(temp.df$Player.Name, historical.fpts$FullName)] # match
    }
  } else {
    temp.ind <- ncol(temp.df) + 1
    temp.df[,temp.ind:(temp.ind+wk-1)] <- NA # add extra cols
    for (i in temp.ind:(temp.ind+wk-1)) {
      colnames(temp.df)[i] <- paste0("Week", i-temp.ind+1) # name cols
      historical.fpts[is.na(historical.fpts[,i-temp.ind+2]),i-temp.ind+2] <- 0 # set NA's to 0 (note that we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0)
      temp.df[,i] <- historical.fpts[,i-temp.ind+2][match(temp.df$Player.Name, historical.fpts$FullName)] # match
    }
    temp.df[,temp.ind+wk-1] <- NULL # remove selected week's historical fpts column from feature set 
  }
  
  #---- Add "Completions.Rolling","Targets.Rolling","TDs.Rolling","Target.Ptcg.Rolling","Rank.Completions","Rank.Targets","Rank.TDs" from stats (nflsavant) ----#
  # read and clean name column for nfl savant data
  temp.rolling.wk <- read.csv(file = paste0("optimizationCode/data_warehouse/stats/rolling.stats.wk", wk-1, ".csv"), stringsAsFactors = F) # lag by 1 week
  temp.rolling.wk$Player.Name.Last <- str_split_fixed(temp.rolling.wk$Name, ", ", 2)[,1]
  temp.rolling.wk$Player.Name.First <- str_split_fixed(temp.rolling.wk$Name, ", ", 2)[,2]
  temp.rolling.wk$Player.Name.Temp.Savant <- paste0(temp.rolling.wk$Player.Name.First, " ", temp.rolling.wk$Player.Name.Last)
  temp.rolling.wk$Player.Name.Temp.Savant <- sub("'", "", temp.rolling.wk$Player.Name.Temp.Savant)
  temp.rolling.wk$Player.Name.Temp.Savant <- sub(' Sr.', '', temp.rolling.wk$Player.Name.Temp.Savant)
  temp.rolling.wk$Player.Name.Temp.Savant <- sub(' Jr.', '', temp.rolling.wk$Player.Name.Temp.Savant)
  temp.rolling.wk$Player.Name.Temp.Savant <- gsub("[.]","", temp.rolling.wk$Player.Name.Temp.Savant)
  temp.rolling.wk$Player.Name.Temp.Savant <- tolower(temp.rolling.wk$Player.Name.Temp.Savant)
  
  # clean position column (nfl savant data)
  temp.rolling.wk$Pos[temp.rolling.wk$Pos=="FB"] <- "RB"
  
  # add features
  temp.df$CompletionsRolling <- temp.rolling.wk$Completions.Rolling[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
  temp.df$TargetsRolling <- temp.rolling.wk$Targets.Rolling[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
  temp.df$TDsRolling <- temp.rolling.wk$TDs.Rolling[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
  temp.df$RollingTargetPctg <- temp.rolling.wk$Target.Ptcg.Rolling[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
  temp.df$RankCompletions <- temp.rolling.wk$Rank.Completions[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
  temp.df$RankTargets <- temp.rolling.wk$Rank.Targets[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
  temp.df$RankTDs <- temp.rolling.wk$Rank.TDs[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
  
  #---- Add Snaps, Snap Pct, Rush Pct, Tgt Pct, Touch Pct, Util Pct (historical) from fantasydata/snapcounts ----#
  if (fantasydata.snapcounts.bool==T) {
    temp.ind <- ncol(temp.df) + 1
    temp.df[,temp.ind:(temp.ind+lag.num*6-1)] <- NA # add extra cols
    for (i in 1:lag.num) {
      temp.snapcounts <- read.csv(file = paste0('optimizationCode/data_warehouse/fantasydata/snapcounts/fantasydata_snapcounts_2016_week', wk-i, ".csv"), stringsAsFactors = F)
      
      # clean player name
      temp.snapcounts$Player <- sub("'", "", temp.snapcounts$Player)
      temp.snapcounts$Player <- sub(' Sr.', '', temp.snapcounts$Player)
      temp.snapcounts$Player <- sub(' Jr.', '', temp.snapcounts$Player)
      temp.snapcounts$Player <- gsub("[.]","", temp.snapcounts$Player)
      temp.snapcounts$Player <- tolower(temp.snapcounts$Player)
      
      # clean position column (fantasydata/snapcounts)
      temp.snapcounts$Pos[temp.snapcounts$Pos=="FB"] <- "RB"
      
      # column name
      colnames(temp.df)[temp.ind:(temp.ind+5)+6*(i-1)] <- paste0("Wk.Lag", i, c(".Snaps",".Snap.Pct",".Rush.Pct",".Tgt.Pct",".Touch.Pct",".Util.Pct"))
      
      # add features
      temp.df[,(temp.ind+0)+6*(i-1)] <- as.numeric(temp.snapcounts$Snaps[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
      temp.df[,(temp.ind+1)+6*(i-1)] <- as.numeric(temp.snapcounts$Snap.Pct[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
      temp.df[,(temp.ind+2)+6*(i-1)] <- as.numeric(temp.snapcounts$Rush.Pct[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
      temp.df[,(temp.ind+3)+6*(i-1)] <- as.numeric(temp.snapcounts$Tgt.Pct[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
      temp.df[,(temp.ind+4)+6*(i-1)] <- as.numeric(temp.snapcounts$Touch.Pct[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
      temp.df[,(temp.ind+5)+6*(i-1)] <- as.numeric(temp.snapcounts$Util.Pct[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
    } 
  }
  
  #---- Add Rush Yds, TD [rush], Rec, Yds, TD [rec] (historical) from fantasydata/stats ----#
  if (fantasydata.stats.bool==T) {
    temp.ind <- ncol(temp.df) + 1
    temp.df[,temp.ind:(temp.ind+lag.num*5-1)] <- NA # add extra cols
    for (i in 1:lag.num) {
      temp.stats <- read.csv(file = paste0('optimizationCode/data_warehouse/fantasydata/stats/fantasydata_stats_2016_week', wk-i, ".csv"), stringsAsFactors = F)
      
      # clean player name
      temp.stats$Player <- sub("'", "", temp.stats$Player)
      temp.stats$Player <- sub(' Sr.', '', temp.stats$Player)
      temp.stats$Player <- sub(' Jr.', '', temp.stats$Player)
      temp.stats$Player <- gsub("[.]","", temp.stats$Player)
      temp.stats$Player <- tolower(temp.stats$Player)
      
      # clean position column (fantasydata/snapcounts)
      temp.stats$Pos[temp.stats$Pos=="FB"] <- "RB"
      
      # column name
      colnames(temp.df)[temp.ind:(temp.ind+4)+5*(i-1)] <- paste0("Wk.Lag", i, c(".Rush.Yds",".Rush.TDs",".Rec",".Rec.Yds",".Rec.TDs"))
      
      # add features
      temp.df[,(temp.ind+0)+5*(i-1)] <- as.numeric(temp.stats$Rush.Yds[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
      temp.df[,(temp.ind+1)+5*(i-1)] <- as.numeric(temp.stats$TD[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
      temp.df[,(temp.ind+2)+5*(i-1)] <- as.numeric(temp.stats$Rec[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
      temp.df[,(temp.ind+3)+5*(i-1)] <- as.numeric(temp.stats$Yds[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
      temp.df[,(temp.ind+4)+5*(i-1)] <- as.numeric(temp.stats$TD.2[match(paste0(temp.df$Player.Name.Temp,temp.df$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
    } 
  }
  
  #---- Add response variable: historical ownership rate ----#
  temp.ownership <- read.csv(file = paste0("projectionsCreation/predictOwnership/data_warehouse/historical_ownership/",contest.name,"/ownership_offense_week",wk,".csv"), stringsAsFactors = F)
  temp.df$Ownership.Pctg <- temp.ownership$Ownership.Pctg[match(paste0(temp.df$Player.Name,temp.df$Team), paste0(temp.ownership$Player.Name,temp.ownership$Team))] # team for players like david johnson and ryan griffin (two players same name)
  
  #---- Print number of non-zero ownership pctg ----#
  print(paste0("Wk ", wk, " Num >0% Ownership:   ", sum(temp.df$Ownership.Pctg>0), " / ", nrow(temp.df)))
  
  #---- Remove rows with NAs ----#
  temp.dataset <- na.omit(temp.df)
  
  #---- Print number of non-zero ownership pctg after removing NAs ----#
  print(paste0("Wk ", wk, " Num >0% Ownership (NAs Removed):   ", sum(temp.dataset$Ownership.Pctg>0), " / ", nrow(temp.dataset)))
  
  #---- Remove 0% ownership players ----#
  # uncomment if want to focus on predicting nonzero ownership players
  # temp.dataset <- temp.dataset[-c(which(temp.dataset$Ownership.Pctg==0)),]
  
  #---- Remove from feature set ----#
  temp.dataset$Pos <- NULL
  temp.dataset$Team <- NULL
  temp.dataset$Opp <- NULL
  temp.dataset$Player.Name.Temp <- NULL
  temp.dataset.copy <- temp.dataset # make copy that includes name and actual fpts (for looking at player names and their actual fpts when training/testing models)
  
  #---- Write dataset to file (if set parameters for weekly output) ----#
  if (write.bool==T) {
    if (outputweekly.bool==T & (fantasydata.snapcounts.bool==T | fantasydata.stats.bool==T)) {
      write.csv(temp.dataset, file = paste0("projectionsCreation/predictOwnership/data_warehouse/finalized_datasets/includes_fantasydata/ownership_data_week", wk, ".csv"), row.names = F)
    } else if (outputweekly.bool==T & is.na(historicalfpts.lag) == T) {
      write.csv(temp.dataset, file = paste0("projectionsCreation/predictOwnership/data_warehouse/finalized_datasets/includes_allhistoricalfpts/ownership_data_week", wk, ".csv"), row.names = F)
    } else if (outputweekly.bool==T & is.na(historicalfpts.lag) == F) {
      write.csv(temp.dataset, file = paste0("projectionsCreation/predictOwnership/data_warehouse/finalized_datasets/includes_historicalfpts",historicalfpts.lag,"wklag/ownership_data_week", wk, ".csv"), row.names = F)
    } else {
      # temporarily nothing
    }
  }
  
  #---- Remove from feature set for running stuff ----#
  temp.dataset$Player.Name <- NULL
  temp.dataset$Actual.FP <- NULL
  
  #---- Append each week's dataset ----#
  dataset.all <- rbind(dataset.all, temp.dataset)
  dataset.all.copy <- rbind(dataset.all.copy, temp.dataset.copy)
}

#---- Print number of non-zero ownership pctg (no NAs) ----#
print(paste0("All Weeks, Num >0% Ownership (NAs Removed):   ", sum(dataset.all$Ownership.Pctg>0), " / ", nrow(dataset.all)))


####### SPLIT INTO TRAINING AND TESTING DATA #######
testing.ind <- sample(x=1:nrow(dataset.all), size=nrow(dataset.all)/5)
data.test <- dataset.all[testing.ind, ]
data.train <- dataset.all[-testing.ind, ]

train.x <- data.train[,1:(ncol(data.train)-1)]
train.y <- data.train[,ncol(data.train)]
test.x <- data.test[,1:(ncol(data.test)-1)]
test.y <- data.test[,ncol(data.test)]


####### MODEL I: LINEAR MODEL #######
if (model.run==1) {
  #---- Training ----#
  model.lm <- lm(Ownership.Pctg ~ ., data=data.train)
  
  #---- Testing ----#
  pred.lm <- predict(object=model.lm, newdata=test.x)
  
  #---- Evaluation ----#
  # MSE
  print(paste0("MSE Training:   ", summary(model.lm)$sigma^2))
  print(paste0("MSE Testing:   ", sum((pred.lm-test.y)^2)/(length(test.y)-2)))
  
  # R-squared (1-SSR/SST)]
  print(paste0("R-Squared Training:   ", summary(model.lm)$r.squared))
  print(paste0("R-Squared Testing:   ", 1 - sum((pred.lm-test.y)^2) / ((length(test.y)-1)*var(test.y))))
  
  # Plot residuals
  plot(model.lm$residuals, main = "Residual Plot: Training Set")
  plot(test.y - pred.lm, main = "Residual Plot: Testing Set") # , ylim = c(0,10)
  
  # Check normality of residuals
  qqnorm(model.lm$residuals)
  qqline(model.lm$residuals) # clearly heavy tails
  qqnorm(test.y - pred.lm)
  qqline(test.y - pred.lm) # clearly heavy tails
}


####### MODEL II: GLM WITH LASSO PENALTY #######
if (model.run==2) {
  #---- Training ----#
  cv.model.lasso <- cv.glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=1, nfolds=10)
  plot(cv.model.lasso$lambda, cv.model.lasso$cvm, xlab = "lambda", ylab = "CV MSEs") # MSE error vs penalty term
  
  model.lasso <- glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=1, lambda=cv.model.lasso$lambda.min) # lambda.min or lambda.1st
  model.lasso$beta # coefficients
  plot(model.lasso, label=TRUE, main = "LASSO Regularization Path") # blank if < 2 nonzero coefficients # doesn't work for glmnetUtils
  pred.lasso.training <- predict(model.lasso, newdata=data.train) # used for R-squared calculation

  #---- Testing ----#
  pred.lasso <- predict(object=model.lasso, newdata=data.test)
  
  #---- Evaluation ----#
  # MSE
  print(paste0("MSE Training:   ", cv.model.lasso$cvm[which(cv.model.lasso$lambda==cv.model.lasso$lambda.min)])) # lambda.min or lambda.1se
  print(paste0("MSE Testing:   ", sum((pred.lasso-test.y)^2) / (length(test.y)-2)))
  
  # R-squared
  print(paste0("R-Squared Training:   ", 1 - sum((pred.lasso.training-train.y)^2) / ((length(train.y)-1)*var(train.y))))
  print(paste0("R-Squared Testing:   ", 1 - sum((pred.lasso-test.y)^2) / ((length(test.y)-1)*var(test.y))))
  
  # Plot residuals
  qqnorm(train.y - pred.lasso.training)
  qqline(train.y - pred.lasso.training) # clearly heavy tails
  qqnorm(test.y - pred.lasso)
  qqline(test.y - pred.lasso) # clearly heavy tails
}


####### MODEL III: GLM WITH RIDGE PENALTY #######
if (model.run==3) {
  #---- Training ----#
  cv.model.ridge <- cv.glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=0, nfolds=10)
  plot(cv.model.ridge$lambda, cv.model.ridge$cvm, xlab = "lambda", ylab = "CV MSEs") # MSE error vs penalty term
  
  model.ridge <- glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=0, lambda=cv.model.ridge$lambda.min) # lambda.min or lambda.1st 
  model.ridge$beta # coefficients
  plot(model.ridge, label=TRUE, main = "Ridge Regularization Path") # blank if < 2 nonzero coefficients # doesn't work for glmnetUtils
  pred.ridge.training <- predict(model.ridge, newdata=data.train) # used for R-squared calculation
  
  #---- Testing ----#
  pred.ridge <- predict(object=model.ridge, newdata=data.test)
  
  #---- Evaluation ----#
  # MSE
  print(paste0("MSE Training:   ", cv.model.ridge$cvm[which(cv.model.ridge$lambda==cv.model.ridge$lambda.min)])) # lambda.min or lambda.1se
  print(paste0("MSE Testing:   ", sum((pred.ridge-test.y)^2) / (length(test.y)-2)))
  
  # R-squared
  print(paste0("R-Squared Training:   ", 1 - sum((pred.ridge.training-train.y)^2) / ((length(train.y)-1)*var(train.y))))
  print(paste0("R-Squared Testing:   ", 1 - sum((pred.ridge-test.y)^2) / ((length(test.y)-1)*var(test.y))))
  
  # Plot residuals
  qqnorm(train.y - pred.ridge.training)
  qqline(train.y - pred.ridge.training) # clearly heavy tails
  qqnorm(test.y - pred.ridge)
  qqline(test.y - pred.ridge) # clearly heavy tails
}


####### MODEL IV: GLM WITH ELASTIC NET PENALTY #######
if (model.run==4) {
  #---- Training ----#
  # iterate over alpha hyperparameter
  alpha.param <- seq(from=0, to=1.0, by=0.05)
  train.mat <- as.data.frame(matrix(data = NA, nrow = length(alpha.param), ncol = 4, dimnames = list(NULL, c("Alpha.Param","MSE.CV","RSQ.CV","NUM.COEFF"))))
  for (i in 1:length(alpha.param)) {
    # cv to find optimal lambda
    cv.model.elasticnet <- cv.glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=alpha.param[i], nfolds=10)
    model.elasticnet <- glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=alpha.param[i], lambda=cv.model.elasticnet$lambda.min) # lambda.min or lambda.1se
    
    pred.elasticnet.training <- predict(model.elasticnet, newdata=data.train)
    mse.train <- cv.model.elasticnet$cvm[which(cv.model.elasticnet$lambda==cv.model.elasticnet$lambda.min)]
    rsq.train <- 1 - sum((pred.elasticnet.training-train.y)^2) / ((length(train.y)-1)*var(train.y))
    
    train.mat$Alpha.Param[i] <- alpha.param[i]
    train.mat$MSE.CV[i] <- mse.train
    train.mat$RSQ.CV[i] <- rsq.train
    train.mat$NUM.COEFF[i] <- sum(model.elasticnet$beta != 0)
  }
  # choose the alpha s.t. MSE.CV is in upper 50% of trained alphas and RSQ.CV maximized
  temp.alpha.train <- train.mat[train.mat$MSE.CV > quantile(train.mat$MSE.CV, 0.5),]
  optimal.alpha <- temp.alpha.train$Alpha.Param[which.max(temp.alpha.train$RSQ.CV)]
  print(paste0("Optimal alpha:   ", optimal.alpha))

  # Use tuned alpha parameter to train model with CV to tune lambda
  cv.model.elasticnet <- cv.glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=optimal.alpha, nfolds=10)
  plot(cv.model.elasticnet$lambda, cv.model.elasticnet$cvm, xlab = "lambda", ylab = "CV MSEs") # MSE error vs penalty term
  
  model.elasticnet <- glmnet(formula=(Ownership.Pctg ~ .), data=data.train, family='gaussian', alpha=optimal.alpha, lambda=cv.model.elasticnet$lambda.min) # lambda.min or lambda.1se
  model.elasticnet$beta
  print(paste0("Num non-zero coefficients:   ", sum(model.elasticnet$beta != 0)))
  plot(model.elasticnet, label=TRUE, main = "Elastic-Net Regularization Path") # blank if < 2 nonzero coefficients # doesn't work for glmnetUtils
  pred.elasticnet.training <- predict(model.elasticnet, newdata=data.train) # used for R-squared calculation
  
  #---- Testing ----#
  pred.elasticnet <- predict(object=model.elasticnet, newdata=data.test)
  quantile(pred.elasticnet)
  
  #---- Evaluation ----#
  # MSE
  print(paste0("MSE Training:   ", cv.model.elasticnet$cvm[which(cv.model.elasticnet$lambda==cv.model.elasticnet$lambda.min)])) # lambda.min or lambda.1se
  print(paste0("MSE Testing:   ", sum((pred.elasticnet-test.y)^2) / (length(test.y)-2)))
  mse.train <- cv.model.elasticnet$cvm[which(cv.model.elasticnet$lambda==cv.model.elasticnet$lambda.min)]
  mse.test <- sum((pred.elasticnet-test.y)^2) / (length(test.y)-2)
  
  # R-squared
  print(paste0("R-Squared Training:   ", 1 - sum((pred.elasticnet.training-train.y)^2) / ((length(train.y)-1)*var(train.y))))
  print(paste0("R-Squared Testing:   ", 1 - sum((pred.elasticnet-test.y)^2) / ((length(test.y)-1)*var(test.y))))
  rsq.train <- 1 - sum((pred.elasticnet.training-train.y)^2) / ((length(train.y)-1)*var(train.y))
  rsq.test <- 1 - sum((pred.elasticnet-test.y)^2) / ((length(test.y)-1)*var(test.y))
  
  # Plot residuals
  plot(train.y - pred.elasticnet.training, type = 'p') # does not appear to be uncorrelated or have constant variance (conditions for OLS to be unbiased estimator that minimizes MSE)
  qqnorm(train.y - pred.elasticnet.training)
  qqline(train.y - pred.elasticnet.training) # clearly heavy tails
  qqnorm(test.y - pred.elasticnet)
  qqline(test.y - pred.elasticnet) # clearly heavy tails
  
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha1.0_wks4-16.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.35_wks4-15.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.65_wks4-14.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.25_wks4-13.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.2_wks4-12.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.05_wks4-11.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.1_wks4-10.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.6_wks4-9.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.75_wks4-8.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.6_wks4-7.RData")
  # save(model.elasticnet,historicalfpts.lag,mse.train,mse.test,rsq.train,rsq.test, file = "projectionsCreation/predictOwnership/data_warehouse/models/elasticnet_alpha0.7_wks4-6.RData")
}


# Clearly, we have heavy tails on both sides of the distribution (see Q-Q plots for all regressions above).
# So, we use a semiparametric fitting method. We tune two thresholds for the beginnings of the tails.
# Then, we fit pareto distributions to the two tails, and fit a normal regression to the middle data.

####### MODEL V: GENERALIZED PARETO DISTRIBUTION #######
if (model.run==5) {
  
}


####### MODEL VI: SVM RANKING #######
if (model.run==6) {
  temp.train <- data.train[order(data.train$Ownership.Pctg),]
  temp.train <- temp.train[temp.train$Ownership.Pctg>0,]
  temp.train$Rank <- 1:nrow(temp.train)
  temp.train.x <- temp.train[,1:(ncol(temp.train)-2)]
  temp.train.y <- temp.train[,ncol(temp.train)]
  
  temp.model <- ranking(x = as.matrix(temp.train.x), y = as.matrix(temp.train.y), kernel = "rbfdot", kpar = list(sigma = 1), scale = FALSE, alpha = 0.99, iterations = 500, edgegraph = FALSE, convergence = FALSE)
  str(temp.model)
  temp.model@.Data
}





