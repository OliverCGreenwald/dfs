#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we construct a dataset to classify cheap WRs as "value" (1) or "not value" (0).
# Section I: Prepare dataset
# Section II: Machine Learning Algorithms for Classifying Value WRs
#   Model I: Logistic Regression
#     - never predicts 1's on testing set
#   Model II: Regularized Logistic Regression
#     - never predicts 1's on testing set
#   Model III: CART
#     - never predicts 1's on testing set
#   Model IV: Random Forest
#   Model V: PCA
#     - PCA1-PCA3 seem to capture ~75% of the variance
#   Model VI: SVM (Linear Kernel)
#     (1) unaltered functional margin: never predicts 1's on testing set
#     (2) looser functional margin: predicts 1's on testing set, but error tradeoff (note that we set an arbitrary "cutoff" probability for classifying 1's)
#   Model VII: SVM (Set to any of the following kernels: rbfdot, anovadot, tanhdot, laplacedot, besseldot, polydot, splinedot, stringdot) (NOTE: CODE FOR ALTERING FUNCTION MARGIN CURRENTLY DOESN'T WORK)
#     - rbfdot (Radial Basis/Gaussian kernel): 
#     - anovadot (ANOVA RBF kernel): 
#     - tanhdot (Hyperbolic tangent / sigmoid kernel): 
#     - laplacedot (Laplacian kernel): 
#     - besseldot (Bessel kernel): 
#     - polydot (Polynomial kernel): 
#     - splinedot (Spline [piece-wise cubic polynomial] kernel): solid (only one to have 1's even with unaltered functional margin)
#   Test: testing code
#
# TODO:
# - we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0 (historicalFpts.R)
# - Add Snaps, Snap Pct, Rush Pct, Tgt Pct, Touch Pct, Util Pct (rolling) from fantasydata/snapcounts
# - Add Rush Yds, TD [rush], Rec, Yds, TD [rec] (rolling) from fantasydata/fantasystats
# - might not be optimal for our problem to use tuning parameters that minimize error
# - use PCA1-PCA3 for CART (good visualization) or some other model. use this to classify.
# - Model VII doesn't work for the altered functional margin (prob.model=T => "line search fails" => predict function returns error: "Error in prob.model(object)[[p]]$A : $ operator is invalid for atomic vectors")
#   - http://stackoverflow.com/questions/15895897/line-search-fails-in-training-ksvm-prob-model


####### SET MODEL TO RUN #######
model.run <- "7" # 1-7, "test"
modelVII.kernel <- "splinedot" # set this to some kernel if model.run = 7
week.min <- 11 # must be >= 4 (this is the week we begin appending weekly data for the overall dataset, "dataset.all")


####### WRITE TO FILE? #######
write.bool <- F # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)
save.model.bool <- T # TRUE if save workspace variables to RData file
save.model.name <- "splinedot_kernel_wks10-15.RData" # only used if save.model.bool is TRUE


####### SET PARAMETERS #######
# wk <- 15 # must be >= 4 (data availability) # uncomment this if for loop (wk in 4:15) is commented out

salary.threshold <- 5000 # define cheap
fpts.threshold <- 15.0 # 18.5 # define value

historicalfpts3wklag.bool <- T # TRUE if want to use 3 week lag historical fpts instead of all historical

fantasydata.snapcounts.bool <- F # TRUE if want to add features from fantasydata/snapcounts (caution: lots of NAs, rows with NAs are removed)
fantasydata.stats.bool <- F # TRUE if want to add features from fantasydata/stats (caution: lots of NAs, rows with NAs are removed)
lag.num <- 3 # number of weeks lag (used for fantasydata/snapcounts)

outputweekly.bool <- F # TRUE if want to output weekly csv's (otherwise outputs all weeks combined csv). Note this can override all other boolean parameters in this section.


####### IMPORT LIBRARIES #########
library('stringr')
library("SDMTools")
library("glmnet")
library("kernlab")
library("rpart")
library("rpart.plot")
library("randomForest")


####### SECTION I: PREPARE DATAFRAME OF CHEAP WR #######
#---- Initializing df for storing all weeks (if loop over weeks is used) ----#
dataset.all <- NULL

for (wk in 4:15) { # uncomment for loop if don't want to loop over weeks
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
      colnames(temp.cheap)[temp.ind-1+i] <- paste0("Wk.Lag", i, ".Fpts")
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
      colnames(temp.cheap)[temp.ind:(temp.ind+5)+6*(i-1)] <- paste0("Wk.Lag", i, c(".Snaps",".Snap.Pct",".Rush.Pct",".Tgt.Pct",".Touch.Pct",".Util.Pct"))
      
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
      colnames(temp.cheap)[temp.ind:(temp.ind+4)+5*(i-1)] <- paste0("Wk.Lag", i, c(".Rush.Yds",".Rush.TDs",".Rec",".Rec.Yds",".Rec.TDs"))
      
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
  
  #---- Append each week's dataset (if meets min week parameter) ----#
  if (wk >= week.min) {
    dataset.all <- rbind(dataset.all, temp.dataset) 
  }
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


####### MODEL I: LOGISTIC REGRESSION #######
if (model.run==1) {
  model.logistic <- glm(Value ~ ., family='binomial', data=data.train)
  
  pred.logistic <- predict(object=model.logistic, newdata=test.x, type='response')
  pred.logistic[pred.logistic > 0.5] <- 1
  pred.logistic[pred.logistic <= 0.5] <- 0
  
  test.error.logistic <- mean(abs(as.numeric(pred.logistic) - test.y))
  print(paste0("Logistic Testing Error:   ", test.error.logistic))
  
  print("Logistic Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.logistic), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
}


####### MODEL II: REGULARIZED LOGISTIC REGRESSION #######
if (model.run==2) {
  # LASSO PENALTY
  model.lasso.cv <- cv.glmnet(x=data.matrix(train.x), y=train.y, alpha=1, family='binomial', type.measure='class', nfolds=5) # alpha = 1 is lasso, alpha = 0 is ridge
  plot(model.lasso.cv, main = "Regularization Path") # Not the usual U-shape. We expect a U-shape if the addition of the tuning parameter (regularization) is necessary. The lack of U-shape suggests that the model does not overfit, so regularization is probably not going to help.
  
  model.lasso <- glmnet(x=data.matrix(train.x), y=train.y, family='binomial', lambda=model.lasso.cv$lambda.min) # lambda.1se
  coef(model.lasso)
  
  pred.lasso <- predict(model.lasso, data.matrix(test.x), type='class')
  test.error.lasso <- mean(abs(as.numeric(pred.lasso) - test.y))
  print(paste0("LASSO Testing Error:   ", test.error.lasso))
  
  print("LASSO Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.lasso), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
  
  # RIDGE PENALTY
  model.ridge.cv <- cv.glmnet(x=data.matrix(train.x), y=train.y, alpha=0, family='binomial', type.measure='class', nfolds=5) # alpha = 1 is lasso, alpha = 0 is ridge
  plot(model.ridge.cv, main = "Regularization Path") # Not the usual U-shape. We expect a U-shape if the addition of the tuning parameter (regularization) is necessary. The lack of U-shape suggests that the model does not overfit, so regularization is probably not going to help.
  
  model.ridge <- glmnet(x=data.matrix(train.x), y=train.y, family='binomial', lambda=model.ridge.cv$lambda.min) # lambda.1se
  coef(model.ridge) # why are they all 0's ???
  
  pred.ridge <- predict(model.ridge, data.matrix(test.x), type='class')
  test.error.ridge <- mean(abs(as.numeric(pred.ridge) - test.y))
  print(paste0("Ridge Testing Error:   ", test.error.ridge))
  
  print("Ridge Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.ridge), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
  
  # ELASTIC NET PENALTY
  model.elasticnet.cv <- cv.glmnet(x=data.matrix(train.x), y=train.y, alpha=0.4, family='binomial', type.measure='class', nfolds=5) # alpha = 1 is lasso, alpha = 0 is ridge
  plot(model.elasticnet.cv, main = "Regularization Path") # Not the usual U-shape. We expect a U-shape if the addition of the tuning parameter (regularization) is necessary. The lack of U-shape suggests that the model does not overfit, so regularization is probably not going to help.
  
  model.elasticnet <- glmnet(x=data.matrix(train.x), y=train.y, family='binomial', lambda=model.elasticnet.cv$lambda.min) # lambda.1se
  coef(model.elasticnet)
  
  pred.elasticnet <- predict(model.elasticnet, data.matrix(test.x), type='class')
  test.error.elasticnet <- mean(abs(as.numeric(pred.elasticnet) - test.y))
  print(paste0("Elastic-Net Testing Error:   ", test.error.elasticnet))
  
  print("Elastic-Net Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.elasticnet), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
}


####### MODEL III: CART #######
if (model.run==3) {
  # dataset.all$Value <- as.factor(dataset.all$Value) # factor
  # dataset.all$Inj <- as.factor(dataset.all$Inj) # factor
  
  model.cart <- rpart(Value ~ ., data = data.train, method="class", minbucket=25) # need to tune minbucket
  prp(model.cart, main="Decision Tree")
  pred.cart <- predict(model.cart, newdata = data.test, type = "class")
  test.error.cart <- mean(abs(as.numeric(as.character(pred.cart)) - test.y))
  print(paste0("CART Testing Error:   ", test.error.cart))
  
  print("CART Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.cart), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
}


####### MODEL IV: RANDOM FOREST #######
if (model.run==4) {
  dataset.all.temp <- dataset.all
  dataset.all.temp$Value <- as.factor(dataset.all.temp$Value) # factor
  dataset.all.temp$Inj <- as.factor(dataset.all.temp$Inj) # factor
  data.test.temp <- dataset.all.temp[testing.ind, ]
  data.train.temp <- dataset.all.temp[-testing.ind, ]
  train.x.temp <- data.train[,1:(ncol(data.train.temp)-1)]
  train.y.temp <- data.train[,ncol(data.train.temp)]
  test.x.temp <- data.test[,1:(ncol(data.test.temp)-1)]
  test.y.temp <- data.test[,ncol(data.test.temp)]
  
  k <- 10 # num of folds in cv
  N <- nrow(data.train.temp) # total sample size (training set)
  n <- floor(N/k) # split sample size
  error.all <- rep(0, 40) # records the cv error with different nodesizes
  for (j in (1:40)){
    ind <- sample(N) # random permutation of all the samples
    error <- rep(0, k) # records the misclassification error for each split as the testing data within cv
    for (i in 1:k){
      test <- ind[(((i-1)*n+1):(min(i*n, N)))] # choose the ith split as the testing data
      rf.model <- randomForest(Value ~ ., data = data.train.temp[-test,], ntree=10, nodesize=j+10) # ideally, also tune ntree
      rf.prediction <- predict(rf.model, newdata = data.train.temp[test,], type = "class")
      error[i] <- sum(abs(as.numeric(as.character(data.train.temp[test,'Value'])) - as.numeric(as.character(rf.prediction)))) / length(test)
      cat("=======", j, ",", i, "=======", "\n")
    }
    error.all[j] <- mean(error)
  }
  min(error.all) # minimum cv error # 0.0779
  nodesize.opt <- 10+which(error.all==min(error.all))[1] # take first match if multiple # 27
  
  # calculate misclassification error on the real testing data using the best tuned nodesize
  model.rf <- randomForest(Value ~ ., data = data.train.temp, ntree=10, nodesize=nodesize.opt)
  pred.rf <- predict(model.rf, newdata=data.test.temp, type="class")
  test.error.rf <- mean(abs(as.numeric(as.character(pred.rf)) - as.numeric(as.character(data.test.temp$Value))))
  print(paste0("Random Forest Testing Error:   ", test.error.rf))
  
  print("Random Forest Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.rf), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
}


####### SECTION V: PCA #######
if (model.run==5) {
  data.pca <- dataset.all[,1:(ncol(dataset.all)-1)] # remove response var
  data.pca$Inj <- NULL # remove non-numeric data
  
  model.pca <- prcomp(data.pca, center = T, scale. = T)
  model.pca
  
  # determine how many PCs to keep
  plot(model.pca, type = 'l', main = "Principal Components: Variances")
  summary(model.pca)
  # biplot(model.pca) # projects the data on the first two PCs (show the proportions of each variable along the two PCs)
}


####### MODEL VI: SVM (LINEAR KERNEL) #######
if (model.run==6) {
  ln.tuning.param <- seq(log(1e-4), log(1e2), length.out=100)
  tuning.param <- exp(ln.tuning.param)
  cv.error <- vector(mode='numeric', length=100)
  for (i in 1:100) {
    model.svm <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel='vanilladot', C=tuning.param[i], cross=5)
    cv.error[i] <- model.svm@cross #cross(model.svm) #svm_i@cross
    print(i)
  }
  
  plot(log(tuning.param), cv.error, type='l', main='Misclassification Error')
  c.optimal <- tuning.param[which.min(cv.error)] # optimal tuning parameter C # 0.32
  
  # svm model with standard functional margin
  model.svm <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel='vanilladot', C=c.optimal, cross=5)
  pred.svm <- predict(model.svm, data.matrix(test.x), type='response')
  test.error.svm <- mean(abs(as.numeric(pred.svm) - test.y))
  print(paste0("SVM (std functional margin) Testing Error:   ", test.error.svm))
  
  # confusion matrix
  print("SVM (std functional margin) Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.svm), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
  
  # svm model with looser functional margin
  model.svm.alt <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel='vanilladot', C=c.optimal, cross=5, prob.model=TRUE)
  pred.svm.alt <- predict(model.svm.alt, data.matrix(test.x), type='prob')
  cutoff <- mean(pred.svm.alt[,1]) # mean(pred.svm[,1]) # mean(pred.svm[,1]) - 1*sd(pred.svm[,1]) # mean(pred.svm[,1]) + 1*sd(pred.svm[,1])
  pred.svm.temp <- rep(0,length(test.y))
  pred.svm.temp[pred.svm.alt[,1] >= cutoff] <- 1
  test.error.svm.alt <- mean(abs(as.numeric(pred.svm.temp) - test.y))
  cat("\n")
  print(paste0("SVM (looser functional margin) Testing Error:   ", test.error.svm.alt))
  
  # confusion matrix
  print("SVM (looser functional margin) Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.svm.temp), threshold = 0.5)
  print(confusion.mat) # we're ok with false-negatives
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
}


####### MODEL VII: SVM (SET KERNEL IN BEGINNING OF FILE) #######
if (model.run==7) {
  ln.tuning.param <- seq(log(1e-4), log(1e2), length.out=100)
  tuning.param <- exp(ln.tuning.param)
  cv.error <- vector(mode='numeric', length=100)
  for (i in 1:100) {
    model.svm <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel=modelVII.kernel, C=tuning.param[i], cross=5)
    cv.error[i] <- model.svm@cross #cross(model.svm) #svm_i@cross
    print(i)
  }
  
  plot(log(tuning.param), cv.error, type='l', main='Misclassification Error')
  c.optimal <- tuning.param[which.min(cv.error)] # optimal tuning parameter C
  
  # svm model with standard functional margin
  model.svm <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel=modelVII.kernel, C=c.optimal, cross=5, scale = T)
  pred.svm <- predict(model.svm, data.matrix(test.x), type='response')
  test.error.svm <- mean(abs(as.numeric(pred.svm) - test.y))
  print(paste0("SVM (std functional margin) Testing Error:   ", test.error.svm))
  
  # confusion matrix
  print("SVM (std functional margin) Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.svm), threshold = 0.5)
  print(confusion.mat) # 0% true positives success rate
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
  cat("\n")
  cat("\n")
  
  # svm model with looser functional margin
  model.svm.alt <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel=modelVII.kernel, C=c.optimal, cross=5, prob.model=T, scale = T) # default minstep = 1e-10 is hardcoded into source code so we can't adjust it
  pred.svm.alt <- predict(model.svm.alt, data.matrix(test.x), type='prob')
  cutoff <- mean(pred.svm.alt[,1]) # mean(pred.svm[,1]) # mean(pred.svm[,1]) - 1*sd(pred.svm[,1]) # mean(pred.svm[,1]) + 1*sd(pred.svm[,1])
  pred.svm.temp <- rep(0,length(test.y))
  pred.svm.temp[pred.svm.alt[,1] >= cutoff] <- 1
  test.error.svm.alt <- mean(abs(as.numeric(pred.svm.temp) - test.y))
  cat("\n")
  print(paste0("SVM (looser functional margin) Testing Error:   ", test.error.svm.alt))
  
  # confusion matrix
  print("SVM (looser functional margin) Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.svm.temp), threshold = 0.5)
  print(confusion.mat) # we're ok with false-negatives
  print(paste0("Value WR hit rate w/ classification:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification:   ", sum(test.y==1)/length(test.y)))
}


####### TEST CODE #######
#---- Test C's for C-SVC ----#
if (model.run=="test") {
  c_s <- seq(from = 0.01, to = 10, by = 0.2)
  c_s
  mat.c_s.result <- as.data.frame(matrix(NA, nrow = length(c_s), ncol = 4, dimnames = list(NULL, c("c","Test.Error","Hit.Prob.w.Classification","Hit.Prob.w.o.Classification"))))
  for (k in 1:length(c_s)) {
    mat.c_s.result[k,1] <- c_s[k]
    
    # svm model with looser functional margin
    model.svm <- ksvm(x=data.matrix(train.x), y=train.y, type='C-svc', kernel='vanilladot', C=c_s[k], cross=5, prob.model=TRUE)
    pred.svm <- predict(model.svm, data.matrix(test.x), type='prob')
    cutoff <- mean(pred.svm[,1]) # mean(pred.svm[,1]) # mean(pred.svm[,1]) - 1*sd(pred.svm[,1]) # mean(pred.svm[,1]) + 1*sd(pred.svm[,1])
    pred.svm.temp <- rep(0,length(test.y))
    pred.svm.temp[pred.svm[,1] >= cutoff] <- 1
    test.error.svm <- mean(abs(as.numeric(pred.svm.temp) - test.y))
    mat.c_s.result[k,2] <- test.error.svm
    
    # confusion matrix
    confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(pred.svm.temp), threshold = 0.5)
    mat.c_s.result[k,3] <- confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])
    mat.c_s.result[k,4] <- sum(test.y==1)/length(test.y)
    print(paste0("C = ", c_s[k]))
  }
  print(mat.c_s.result)
  print(max(mat.c_s.result$Hit.Prob.w.Classification))
}


####### SAVE MODEL IN RDATA FILE #######
if (save.model.bool==T) {
  save.image(paste0("optimizationCode/data_warehouse/datasets/cheapWR/models/", save.model.name))
}

