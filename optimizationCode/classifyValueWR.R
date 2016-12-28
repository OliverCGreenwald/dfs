#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we construct a dataset to classify cheap WRs as "value" (1) or "not value" (0).
# Section I: Prepare dataset
# Section II: Logistic Regression
# Section III: Regularized Logistic Regression
# Section IV: SVM
# Section V: Random Forest
# TODO:
# - we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0 (historicalFpts.R)
# - Add Snaps, Snap Pct, Rush Pct, Tgt Pct, Touch Pct, Util Pct (rolling) from fantasydata/snapcounts
# - Add Rush Yds, TD [rush], Rec, Yds, TD [rec] (rolling) from fantasydata/fantasystats


####### WRITE TO FILE? #######
write.bool <- F # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### SET PARAMETERS #######
wk <- 15 # must be >= 4 (data availability)

salary.threshold <- 5000 # defining cheap
fpts.threshold <- 18.5 # defining value

historicalfpts3wklag.bool <- T # TRUE if want to use 3 week lag historical fpts instead of all historical

fantasydata.snapcounts.bool <- F # TRUE if want to add features from fantasydata/snapcounts (caution: lots of NAs)
fantasydata.stats.bool <- F # TRUE if want to add features from fantasydata/stats (caution: lots of NAs)
lag.num <- 3 # number of weeks lag (used for fantasydata/snapcounts)

outputweekly.bool <- F # TRUE if want to output weekly csv's (otherwise outputs all weeks combined csv). Note this can override all other boolean parameters in this section.


####### IMPORT LIBRARIES #########
library('stringr')
library("SDMTools")
library("glmnet")
library("kernlab")


####### SECTION I: PREPARE DATAFRAME OF CHEAP WR #######
#---- Initializing df for storing all weeks (if loop over weeks is used) ----#
dataset.all <- NULL

for (wk in 4:15) {
#---- Read and clean DFN features ----#
temp <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', wk, ".csv"), stringsAsFactors = F)
temp <- temp[temp$Inj != "O",]
temp$Inj[temp$Inj=='Q'] <- 1
temp$Inj[temp$Inj!=1] <- 0
temp$Likes[is.na(temp$Likes)] <- 0
temp$Defense.Pass.Yds.G <- as.numeric(sub("%", "", temp$Defense.Pass.Yds.G))*0.01
temp$Defense.Rush.Yds.G <- as.numeric(sub("%", "", temp$Defense.Rush.Yds.G))*0.01
temp$DvP <- as.numeric(sub("%", "", temp$DvP))*0.01

#---- Subset cheap WRs ----#
temp.cheap <- temp[temp$Salary <= salary.threshold & temp$Pos=='WR', c('Player.Name','Likes','Inj','Pos','Salary','Team','Opp','Vegas.Pts','Defense.Pass.Yds.G','Defense.Rush.Yds.G','DvP','L3.Rush.Att','S.Rush.Att','Proj.Rush.Att','Red.Zone.Rush.Att','Yards.Per.Rush.Att', "L3.Targets", "S.Targets", "Proj.Targets", "Red.Zone.Targets", "Yards.Per.Target", "L3.FP", "S.FP", 'Projected.Usage','Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]

#---- Clean temp.cheap player names for matching fantasydata names ----#
temp.cheap$Player.Name.Temp <- sub("'", "", temp.cheap$Player.Name)
temp.cheap$Player.Name.Temp <- sub(' Sr.', '', temp.cheap$Player.Name.Temp)
temp.cheap$Player.Name.Temp <- sub(' Jr.', '', temp.cheap$Player.Name.Temp)
temp.cheap$Player.Name.Temp <- gsub("[.]","",temp.cheap$Player.Name.Temp) # remove periods
temp.cheap$Player.Name.Temp <- tolower(temp.cheap$Player.Name.Temp)

# Load and clean fantasydata salaries files for opp defense rankings
temp.fantasydata <- read.csv(file = paste0('optimizationCode/data_warehouse/fantasydata/salaries/fantasydata_salaries_2016_week', wk, ".csv"), stringsAsFactors = F)
temp.fantasydata$Player <- sub("'", "", temp.fantasydata$Player)
temp.fantasydata$Player <- sub(' Sr.', '', temp.fantasydata$Player)
temp.fantasydata$Player <- sub(' Jr.', '', temp.fantasydata$Player)
temp.fantasydata$Player <- tolower(temp.fantasydata$Player)

# Add Opp.Rank and Opp.Pos.Rank columns to temp.cheap
temp.cheap$Opp.Rank <- as.numeric(temp.fantasydata$Opp.Rank[match(temp.cheap$Player.Name.Temp, temp.fantasydata$Player)])
temp.cheap$Opp.Pos.Rank <- as.numeric(temp.fantasydata$Opp.Pos.Rank[match(temp.cheap$Player.Name.Temp, temp.fantasydata$Player)])


#---- Add historical fpts ----#
historical.fpts <- read.csv(file = "optimizationCode/data_warehouse/historical_fpts/historical.fpts.csv", stringsAsFactors = F)
if (historicalfpts3wklag.bool==T) {
  temp.ind <- ncol(temp.cheap) + 1
  temp.cheap[,temp.ind:(temp.ind+2)] <- NA # add extra cols
  for (i in 1:3) {
    colnames(temp.cheap)[temp.ind-1+i] <- paste0("Wk-", i, ".Fpts")
    historical.fpts[is.na(historical.fpts[,wk-i+1]),wk-i+1] <- 0 # set NA's to 0 (note that we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0)
    temp.cheap[,temp.ind-1+i] <- historical.fpts[,wk-i+1][match(temp.cheap$Player.Name, historical.fpts$FullName)] # match
  }
} else {
  temp.ind <- ncol(temp.cheap) + 1
  temp.cheap[,temp.ind:(temp.ind+wk-1)] <- NA # add extra cols
  for (i in temp.ind:(temp.ind+wk-1)) {
    colnames(temp.cheap)[i] <- paste0("Week", i-temp.ind+1) # name cols
    historical.fpts[is.na(historical.fpts[,i-temp.ind+2]),i-temp.ind+2] <- 0 # set NA's to 0 (note that we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0)
    temp.cheap[,i] <- historical.fpts[,i-temp.ind+2][match(temp.cheap$Player.Name, historical.fpts$FullName)] # match
  }
  temp.cheap[,temp.ind+wk-1] <- NULL # remove selected week's historical fpts column from feature set 
}

#---- Add "Completions.Rolling","Targets.Rolling","TDs.Rolling","Target.Ptcg.Rolling","Rank.Completions","Rank.Targets","Rank.TDs" from stats (nflsavant) ----#
# read and clean name column for nfl savant data
temp.rolling.wk <- read.csv(file = paste0("optimizationCode/data_warehouse/stats/rolling.stats.wk", wk-1, ".csv"), stringsAsFactors = F)
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
temp.cheap$CompletionsRolling <- temp.rolling.wk$Completions.Rolling[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
temp.cheap$TargetsRolling <- temp.rolling.wk$Targets.Rolling[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
temp.cheap$TDsRolling <- temp.rolling.wk$TDs.Rolling[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
temp.cheap$RollingTargetPctg <- temp.rolling.wk$Target.Ptcg.Rolling[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
temp.cheap$RankCompletions <- temp.rolling.wk$Rank.Completions[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
temp.cheap$RankTargets <- temp.rolling.wk$Rank.Targets[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]
temp.cheap$RankTDs <- temp.rolling.wk$Rank.TDs[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.rolling.wk$Player.Name.Temp.Savant,temp.rolling.wk$Pos))]

#---- Add Snaps, Snap Pct, Rush Pct, Tgt Pct, Touch Pct, Util Pct (historical) from fantasydata/snapcounts ----#
if (fantasydata.snapcounts.bool==T) {
  temp.ind <- ncol(temp.cheap) + 1
  temp.cheap[,temp.ind:(temp.ind+lag.num*6-1)] <- NA # add extra cols
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
    colnames(temp.cheap)[temp.ind:(temp.ind+5)+6*(i-1)] <- paste0("Wk-", i, c(".Snaps",".Snap.Pct",".Rush.Pct",".Tgt.Pct",".Touch.Pct",".Util.Pct"))
    
    # add features
    temp.cheap[,(temp.ind+0)+6*(i-1)] <- as.numeric(temp.snapcounts$Snaps[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
    temp.cheap[,(temp.ind+1)+6*(i-1)] <- as.numeric(temp.snapcounts$Snap.Pct[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
    temp.cheap[,(temp.ind+2)+6*(i-1)] <- as.numeric(temp.snapcounts$Rush.Pct[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
    temp.cheap[,(temp.ind+3)+6*(i-1)] <- as.numeric(temp.snapcounts$Tgt.Pct[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
    temp.cheap[,(temp.ind+4)+6*(i-1)] <- as.numeric(temp.snapcounts$Touch.Pct[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
    temp.cheap[,(temp.ind+5)+6*(i-1)] <- as.numeric(temp.snapcounts$Util.Pct[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.snapcounts$Player,temp.snapcounts$Pos))]) # bad feature, missing lots of data
  } 
}

#---- Add Rush Yds, TD [rush], Rec, Yds, TD [rec] (historical) from fantasydata/stats ----#
if (fantasydata.stats.bool==T) {
  temp.ind <- ncol(temp.cheap) + 1
  temp.cheap[,temp.ind:(temp.ind+lag.num*5-1)] <- NA # add extra cols
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
    colnames(temp.cheap)[temp.ind:(temp.ind+4)+5*(i-1)] <- paste0("Wk-", i, c(".Rush.Yds",".Rush.TDs",".Rec",".Rec.Yds",".Rec.TDs"))
    
    # add features
    temp.cheap[,(temp.ind+0)+5*(i-1)] <- as.numeric(temp.stats$Rush.Yds[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
    temp.cheap[,(temp.ind+1)+5*(i-1)] <- as.numeric(temp.stats$TD[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
    temp.cheap[,(temp.ind+2)+5*(i-1)] <- as.numeric(temp.stats$Rec[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
    temp.cheap[,(temp.ind+3)+5*(i-1)] <- as.numeric(temp.stats$Yds[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
    temp.cheap[,(temp.ind+4)+5*(i-1)] <- as.numeric(temp.stats$TD.2[match(paste0(temp.cheap$Player.Name.Temp,temp.cheap$Pos), paste0(temp.stats$Player,temp.stats$Pos))])
  } 
}

#---- Add response variable: 1 if >= fpts.threshold and 0 otherwise ----#
temp.cheap$Value[temp.cheap$Actual.FP >= fpts.threshold] <- 1
temp.cheap$Value[temp.cheap$Actual.FP < fpts.threshold] <- 0

#---- Remove from feature set ----#
temp.cheap$Player.Name <- NULL
temp.cheap$Pos <- NULL
temp.cheap$Team <- NULL
temp.cheap$Opp <- NULL
temp.cheap$Actual.FP <- NULL
temp.cheap$Player.Name.Temp <- NULL

#---- Print number of Value WR ----#
print(paste0("Wk ", wk, " Num of Value WR / Num Cheap WR:   ", sum(temp.cheap$Value>0), " / ", nrow(temp.cheap)))

#---- Remove rows with NAs ----#
temp.dataset <- na.omit(temp.cheap)

#---- Print number of Value WR after removing NAs ----#
print(paste0("Wk ", wk, " Num of Value WR / Num Cheap WR (NAs Removed):   ", sum(temp.dataset$Value>0), " / ", nrow(temp.dataset)))

#---- Write dataset to file (if set parameters for weekly output) ----#
if (write.bool==T) {
  if (outputweekly.bool==T & (fantasydata.snapcounts.bool==T | fantasydata.stats.bool==T)) {
    write.csv(temp.dataset, file = paste0("optimizationCode/data_warehouse/datasets/cheapWR/weekly_data/includes_fantasydata/cheapwr_data_week", wk, ".csv"), row.names = F) 
  } else if (outputweekly.bool==T & historicalfpts3wklag.bool == F) {
    write.csv(temp.dataset, file = paste0("optimizationCode/data_warehouse/datasets/cheapWR/weekly_data/includes_allhistoricalfpts/cheapwr_data_week", wk, ".csv"), row.names = F)
  } else if (outputweekly.bool==T & historicalfpts3wklag.bool == T) {
    write.csv(temp.dataset, file = paste0("optimizationCode/data_warehouse/datasets/cheapWR/weekly_data/includes_historicalfpts3wklag/cheapwr_data_week", wk, ".csv"), row.names = F)
  } else {
    # nothing
  }
}

#---- Append each week's dataset ----#
dataset.all <- rbind(dataset.all, temp.dataset)

}


#---- Print number of Value WR over all weeks (no NAs) ----#
print(paste0("All Weeks, Num of Value WR / Num Cheap WR (NAs Removed):   ", sum(dataset.all$Value>0), " / ", nrow(dataset.all)))

#---- Print number of Value WR after removing NAs ----#
if (write.bool==T & outputweekly.bool==F) {
  write.csv(dataset.all, file = paste0("optimizationCode/data_warehouse/datasets/cheapWR/cheapwr_data_allwks.csv"), row.names = F)
}



####### SPLIT INTO TRAINING AND TESTING DATA #######
testing.ind <- sample(x=1:nrow(dataset.all), size=nrow(dataset.all)/5)
data.test <- dataset.all[testing.ind, ]
data.train <- dataset.all[-testing.ind, ]

train.x <- data.train[,1:(ncol(data.train)-1)]
train.y <- data.train[,ncol(data.train)]
test.x <- data.test[,1:(ncol(data.test)-1)]
test.y <- data.test[,ncol(data.test)]


####### SECTION II: LOGISTIC REGRESSION #######
model <- glm(Value ~ ., family='binomial', data=data.train)

pred <- predict(object=model, newdata=test.x, type='response')
pred[pred > 0.5] <- 1
pred[pred <= 0.5] <- 0

test.error <- (mean(abs(as.numeric(pred) - test.y)))
test.error

confusion.matrix(obs = as.numeric(pred), pred = test.y, threshold = 0.5) # 0% true positives success rate


####### SECTION III: REGULARIZED LOGISTIC REGRESSION #######
# LASSO PENALTY
model.cv <- cv.glmnet(x=data.matrix(train.x), y=train.y, alpha=1, family='binomial', type.measure='class', nfolds=5) # alpha = 1 is lasso, alpha = 0 is ridge
plot(model.cv, main = "Regularization Path") # Not the usual U-shape. We expect a U-shape if the addition of the tuning parameter (regularization) is necessary. The lack of U-shape suggests that the model does not overfit, so regularization is probably not going to help.

model <- glmnet(x=data.matrix(train.x), y=train.y, family='binomial', lambda=model.cv$lambda.min) # lambda.1se
coef(model)

pred <- predict(model, data.matrix(test.x), type='class')
test.error <- (mean(abs(as.numeric(pred) - test.y)))
test.error

confusion.matrix(obs = as.numeric(pred), pred = test.y, threshold = 0.5) # 0% true positives success rate

# RIDGE PENALTY
model.cv <- cv.glmnet(x=data.matrix(train.x), y=train.y, alpha=0, family='binomial', type.measure='class', nfolds=5) # alpha = 1 is lasso, alpha = 0 is ridge
plot(model.cv, main = "Regularization Path") # Not the usual U-shape. We expect a U-shape if the addition of the tuning parameter (regularization) is necessary. The lack of U-shape suggests that the model does not overfit, so regularization is probably not going to help.

model <- glmnet(x=data.matrix(train.x), y=train.y, family='binomial', lambda=model.cv$lambda.min) # lambda.1se
coef(model) # why are they all 0's ???

pred <- predict(model, data.matrix(test.x), type='class')
test.error <- (mean(abs(as.numeric(pred) - test.y)))
test.error

confusion.matrix(obs = as.numeric(pred), pred = test.y, threshold = 0.5) # 0% true positives success rate

# ELASTIC NET PENALTY
model.cv <- cv.glmnet(x=data.matrix(train.x), y=train.y, alpha=0.4, family='binomial', type.measure='class', nfolds=5) # alpha = 1 is lasso, alpha = 0 is ridge
plot(model.cv, main = "Regularization Path") # Not the usual U-shape. We expect a U-shape if the addition of the tuning parameter (regularization) is necessary. The lack of U-shape suggests that the model does not overfit, so regularization is probably not going to help.

model <- glmnet(x=data.matrix(train.x), y=train.y, family='binomial', lambda=model.cv$lambda.min) # lambda.1se
coef(model)

pred <- predict(model, data.matrix(test.x), type='class')
test.error <- (mean(abs(as.numeric(pred) - test.y)))
test.error

confusion.matrix(obs = as.numeric(pred), pred = test.y, threshold = 0.5) # 0% true positives success rate


####### SECTION IV: SVM #######
ln.tuning.param <- seq(log(1e-4), log(1e2), length.out=100)
tuning.param <- exp(ln.tuning.param)
cv.error <- vector(mode='numeric', length=100)
for (i in 1:100) {
  model <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel='vanilladot', C=tuning.param[i], cross=5)
  cv.error[i] <- model@cross #cross(model) #svm_i@cross
  print(i)
}

plot(log(tuning.param), cv.error, type='l', main='Misclassification Error')
c.optimal <- tuning.param[which.min(cv.error)] # optimal tuning parameter C

model <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel='vanilladot', C=c.optimal, cross=5)

pred <- predict(model, data.matrix(test.x), type='response')
test.error <- (mean(abs(as.numeric(pred) - test.y)))
test.error

confusion.matrix(obs = as.numeric(pred), pred = test.y, threshold = 0.5) # 0% true positives success rate


####### SECTION V: RANDOM FOREST #######





