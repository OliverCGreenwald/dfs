#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we construct a dataset to classify cheap WRs as "value" (1) or "not value" (0).
# Section I: Prepare dataset
# Section II: Machine Learning Algorithms for Classifying Value WRs
#   Model X:  Asymmetric Cost SVM (Linear Kernel)
#     - Instead of penalty parameter C, we introduce C+ and C-, which penalize false positives and false negatives, respectively. Useful for dataset with very unbalanced number of positive and negative examples. (http://svmlight.joachims.org/)
#     - Set modelIX.cost.factor.ratio.vec +-0.10 seq around sum(dataset.all$Value==1)/sum(dataset.all$Value==0)


####### IMPORT LIBRARIES #########
library('stringr')
library("SDMTools")
library("glmnet")
library("kernlab")
library("rpart")
library("rpart.plot")
library("randomForest")
library("klaR")


####### SET MODEL TO RUN #######
model.run <- "10" # 10
week.min <- 7 # must be >= 4 if using lag 3 in params
week.max <- 15 # for loop only


####### WRITE TO FILE? #######
write.bool <- F # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### SET PARAMETERS #######
salary.threshold <- 5000 # define cheap
fpts.threshold <- 18.5 # 18.5 # define value

historicalfpts.lag <- 3 # num wks of lagged historical fpts (use 3 for weeks 7-16, 1 for weeks 2-6, NA if not using this)

fantasydata.snapcounts.bool <- F # TRUE if want to add features from fantasydata/snapcounts (caution: lots of NAs, rows with NAs are removed)
fantasydata.stats.bool <- F # TRUE if want to add features from fantasydata/stats (caution: lots of NAs, rows with NAs are removed)
lag.num <- 3 # number of weeks lag (used for fantasydata/snapcounts)

outputweekly.bool <- F # TRUE if want to output weekly csv's (otherwise outputs all weeks combined csv). Note this can override all other boolean parameters in this section.


####### SECTION I: PREPARE DATAFRAME OF CHEAP WR #######
#---- Initializing df for storing all weeks (if loop over weeks is used) ----#
dataset.all <- NULL
dataset.all.copy <- NULL # copy that keeps name and actual fpts (used for analyzing false positives)

for (wk in week.min:week.max) { # uncomment for loop if don't want to loop over weeks
  #---- Read and clean DFN features ----#
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', wk, ".csv"), stringsAsFactors = F)
  temp$Inj <- NULL
  temp$Likes[is.na(temp$Likes)] <- 0
  temp$Defense.Pass.Yds.G <- as.numeric(sub("%", "", temp$Defense.Pass.Yds.G))*0.01
  temp$Defense.Rush.Yds.G <- as.numeric(sub("%", "", temp$Defense.Rush.Yds.G))*0.01
  temp$DvP <- as.numeric(sub("%", "", temp$DvP))*0.01
  
  #---- Subset cheap WRs ----#
  if (historicalfpts.lag==3 | is.na(historicalfpts.lag)==T) {
    temp.cheap <- temp[temp$Salary <= salary.threshold & temp$Pos=='WR', c('Player.Name','Likes','Pos','Salary','Team','Opp','Vegas.Pts','Defense.Pass.Yds.G','Defense.Rush.Yds.G','DvP','L3.Rush.Att','S.Rush.Att','Proj.Rush.Att','Red.Zone.Rush.Att','Yards.Per.Rush.Att', "L3.Targets", "S.Targets", "Proj.Targets", "Red.Zone.Targets", "Yards.Per.Target", "L3.FP", "S.FP", 'Projected.Usage','Floor.FP','Ceil.FP','Proj.FP','Actual.FP')]
  } else {
    temp.cheap <- temp[temp$Salary <= salary.threshold & temp$Pos=='WR', c('Player.Name','Likes','Pos','Salary','Team','Opp','Vegas.Pts','Defense.Pass.Yds.G','Defense.Rush.Yds.G','DvP','S.Rush.Att','Red.Zone.Rush.Att','Yards.Per.Rush.Att', "L3.Targets", "S.Targets", "Red.Zone.Targets", "Yards.Per.Target", "L3.FP", "S.FP",'Floor.FP','Ceil.FP','Proj.FP','Actual.FP')] 
  }
  
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
  if (historicalfpts.lag > 1) {
    temp.ind <- ncol(temp.cheap) + 1
    temp.cheap[,temp.ind:(temp.ind+(historicalfpts.lag-1))] <- NA # add extra cols
    for (i in 1:historicalfpts.lag) {
      colnames(temp.cheap)[temp.ind-1+i] <- paste0("Wk.Lag", i, ".Fpts")
      historical.fpts[is.na(historical.fpts[,wk-i+1]),wk-i+1] <- 0 # set NA's to 0 (note that we don't differentiate games where a player didn't play from games where a player didn't score any fpts...always 0)
      temp.cheap[,temp.ind-1+i] <- historical.fpts[,wk-i+1][match(temp.cheap$Player.Name, historical.fpts$FullName)] # match
    }
  } else if (historicalfpts.lag==1) {
    temp.ind <- ncol(temp.cheap) + 1
    temp.cheap[,temp.ind:(temp.ind+0)] <- NA # add extra cols
    for (i in 1:1) {
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
  
  #---- Print number of Value WR ----#
  print(paste0("Wk ", wk, " Num of Value WR / Num Cheap WR:   ", sum(temp.cheap$Value>0), " / ", nrow(temp.cheap)))
  
  #---- Remove rows with NAs ----#
  temp.dataset <- na.omit(temp.cheap)
  
  #---- Print number of Value WR after removing NAs ----#
  print(paste0("Wk ", wk, " Num of Value WR / Num Cheap WR (NAs Removed):   ", sum(temp.dataset$Value>0), " / ", nrow(temp.dataset)))
  
  #---- Remove from feature set ----#
  temp.dataset$Pos <- NULL
  temp.dataset$Team <- NULL
  temp.dataset$Opp <- NULL
  temp.dataset$Player.Name.Temp <- NULL
  temp.dataset.copy <- temp.dataset # make copy that includes name and actual fpts (for looking at player names and their actual fpts when training/testing models)
  
  #---- Write dataset to file (if set parameters for weekly output) ----#
  if (write.bool==T) {
    if (outputweekly.bool==T & (fantasydata.snapcounts.bool==T | fantasydata.stats.bool==T)) {
      write.csv(temp.dataset, file = paste0("projectionsCreation/classificationModels/datasets/cheapWR/weekly_data/includes_fantasydata/cheapwr_data_week", wk, ".csv"), row.names = F)
    } else if (outputweekly.bool==T & is.na(historicalfpts.lag) == T) {
      write.csv(temp.dataset, file = paste0("projectionsCreation/classificationModels/datasets/cheapWR/weekly_data/includes_allhistoricalfpts/cheapwr_data_week", wk, ".csv"), row.names = F)
    } else if (outputweekly.bool==T & is.na(historicalfpts.lag) == F) {
      write.csv(temp.dataset, file = paste0("projectionsCreation/classificationModels/datasets/cheapWR/weekly_data/includes_historicalfpts",historicalfpts.lag,"wklag/cheapwr_data_week", wk, ".csv"), row.names = F)
    } else {
      # temporarily nothing
    }
  }
  
  #---- Remove from feature set ----#
  temp.dataset$Player.Name <- NULL
  temp.dataset$Actual.FP <- NULL
  
  #---- Append each week's dataset ----#
  dataset.all <- rbind(dataset.all, temp.dataset)
  dataset.all.copy <- rbind(dataset.all.copy, temp.dataset.copy)
}


#---- Print number of Value WR over all weeks (no NAs) ----#
print(paste0("All Weeks, Num of Value WR / Num Cheap WR (NAs Removed):   ", sum(dataset.all$Value>0), " / ", nrow(dataset.all)))

#---- Print number of Value WR after removing NAs ----#
if (write.bool==T & outputweekly.bool==F) {
  write.csv(dataset.all, file = paste0("projectionsCreation/classificationModels/datasets/cheapWR/cheapwr_data_allwks.csv"), row.names = F)
}


####### SPLIT INTO TRAINING AND TESTING DATA #######
testing.ind <- sample(x=1:nrow(dataset.all), size=nrow(dataset.all)/5)
data.test <- dataset.all[testing.ind, ]
data.train <- dataset.all[-testing.ind, ]

train.x <- data.train[,1:(ncol(data.train)-1)]
train.y <- data.train[,ncol(data.train)]
test.x <- data.test[,1:(ncol(data.test)-1)]
test.y <- data.test[,ncol(data.test)]


####### MODEL X: ASYMMETRIC COST SVM (LINEAR KERNEL) #######
if (model.run==10) {
  ptm <- proc.time() # running time
  
  #------ Training: Cross Validation ------#
  modelX.cost.factor.ratio.vec <- seq(from = 0.03, 0.10, by = 0.005)
  modelX.cost.factor.ratio.vec
  
  k <- 5 # num of folds in cv
  N <- nrow(data.train) # total sample size (training set)
  n <- floor(N/k) # split sample size (for validation set)
  
  error.all <- rep(NA, length(modelX.cost.factor.ratio.vec)) # records the cv error with different parameter values
  
  list.confusion.mat.00 <- replicate(k, matrix(NA, nrow = 1, ncol = length(modelX.cost.factor.ratio.vec), dimnames = list(NULL, modelX.cost.factor.ratio.vec)), simplify=F) # for storing number of pred 0, obs 0
  list.confusion.mat.01 <- replicate(k, matrix(NA, nrow = 1, ncol = length(modelX.cost.factor.ratio.vec), dimnames = list(NULL, modelX.cost.factor.ratio.vec)), simplify=F) # for storing number of pred 0, obs 1
  list.confusion.mat.10 <- replicate(k, matrix(NA, nrow = 1, ncol = length(modelX.cost.factor.ratio.vec), dimnames = list(NULL, modelX.cost.factor.ratio.vec)), simplify=F) # for storing number of pred 1, obs 0
  list.confusion.mat.11 <- replicate(k, matrix(NA, nrow = 1, ncol = length(modelX.cost.factor.ratio.vec), dimnames = list(NULL, modelX.cost.factor.ratio.vec)), simplify=F) # for storing number of pred 1, obs 1
  
  for (j in (1:length(modelX.cost.factor.ratio.vec))) {
    ind <- sample(N) # random permutation of all the samples
    error <- rep(0, k) # records the misclassification error for each split as the testing data within cv
    
    for (i in 1:k){
      cat("======= C_+/C_-: ", modelX.cost.factor.ratio.vec[j], ", CV Fold: ", i, "=======", "\n")
      
      test <- ind[(((i-1)*n+1):(min(i*n, N)))] # choose the ith split as the testing data
      model.svmlight <- svmlight(Value ~ ., data = data.train[-test,], pathsvm = "binaries/svm_light_osx.8.4_i7", type = "C", svm.options = paste0("-t 0 -j ", modelX.cost.factor.ratio.vec[j]), scal = T)
      pred.svmlight <- predict(model.svmlight, newdata = train.x[test,], scal = T)
      
      error[i] <- sum(abs(as.numeric(as.character(data.train[test,'Value'])) - as.numeric(as.character(pred.svmlight$class)))) / length(test)
      
      # confusion matrix
      confusion.mat <- confusion.matrix(obs = as.numeric(as.character(data.train[test,'Value'])), pred = as.numeric(as.character(pred.svmlight$class)), threshold = 0.5)
      list.confusion.mat.00[[i]][1,j] <- confusion.mat[1,1]
      list.confusion.mat.01[[i]][1,j] <- confusion.mat[1,2]
      list.confusion.mat.10[[i]][1,j] <- confusion.mat[2,1]
      list.confusion.mat.11[[i]][1,j] <- confusion.mat[2,2]
      
      # Sys.sleep(10)
    }
    # Sys.sleep(10)
    error.all[j] <- mean(error) # mean error over k folds of cv
  }
  
  confusion.mat.00.all <- Reduce("+", list.confusion.mat.00) / length(list.confusion.mat.00) # mean count of pred 0 obs 0 over k folds of cv
  confusion.mat.01.all <- Reduce("+", list.confusion.mat.01) / length(list.confusion.mat.01) # mean count of pred 0 obs 1 over k folds of cv
  confusion.mat.10.all <- Reduce("+", list.confusion.mat.10) / length(list.confusion.mat.10) # mean count of pred 1 obs 0 over k folds of cv
  confusion.mat.11.all <- Reduce("+", list.confusion.mat.11) / length(list.confusion.mat.11) # mean count of pred 1 obs 1 over k folds of cv
  
  
  #------ Training: Optimizing Over Various Quantities ------#
  # Most promising quantities: (2), (6)
  
  # (1) Minimize CV error
  # min(error.all)
  # cost.factor.ratio.optimal <- modelX.cost.factor.ratio.vec[which(error.all==min(error.all))] # take first match if multiple
  # cost.factor.ratio.optimal
  
  # (2) Minimize CV error - % true positives out of total (reward true positives)
  adj.error.all <- error.all - 5*confusion.mat.11.all / (confusion.mat.00.all+confusion.mat.01.all+confusion.mat.10.all+confusion.mat.11.all)
  min(adj.error.all)
  cost.factor.ratio.optimal <- modelX.cost.factor.ratio.vec[which(adj.error.all==min(adj.error.all))] # take first match if multiple
  cost.factor.ratio.optimal
  
  # (3) Minimize CV error - % true positives out of pred 1's (reward true positives)
  # adj.error.all <- error.all - 1*confusion.mat.11.all / (confusion.mat.10.all+confusion.mat.11.all)
  # min(adj.error.all)
  # cost.factor.ratio.optimal <- modelX.cost.factor.ratio.vec[which(adj.error.all==min(adj.error.all))] # take first match if multiple
  # cost.factor.ratio.optimal

  # (4) Minimize CV error + % false positives out of total (penalize false positives)
  # adj.error.all <- error.all + 1*confusion.mat.10.all / (confusion.mat.00.all+confusion.mat.01.all+confusion.mat.10.all+confusion.mat.11.all)
  # min(adj.error.all)
  # cost.factor.ratio.optimal <- modelX.cost.factor.ratio.vec[which(adj.error.all==min(adj.error.all))] # take first match if multiple
  # cost.factor.ratio.optimal
    
  # (5) Minimize CV error + % false positives out of pred 1's (penalize false positives)
  # adj.error.all <- error.all + 1*confusion.mat.10.all / (confusion.mat.10.all+confusion.mat.11.all)
  # min(adj.error.all)
  # cost.factor.ratio.optimal <- modelX.cost.factor.ratio.vec[which(adj.error.all==min(adj.error.all))] # take first match if multiple
  # cost.factor.ratio.optimal
  
  # (6) Maximize % true positives out of total
  # adj.error.all <- confusion.mat.11.all / (confusion.mat.00.all+confusion.mat.01.all+confusion.mat.10.all+confusion.mat.11.all)
  # max(adj.error.all)
  # cost.factor.ratio.optimal <- modelX.cost.factor.ratio.vec[which(adj.error.all==max(adj.error.all))] # take first match if multiple
  # cost.factor.ratio.optimal
  
  # (7) Maximize % true positives out of pred 1's
  # adj.error.all <- confusion.mat.11.all / (confusion.mat.10.all+confusion.mat.11.all)
  # max(adj.error.all)
  # cost.factor.ratio.optimal <- modelX.cost.factor.ratio.vec[which(adj.error.all==max(adj.error.all))] # take first match if multiple
  # cost.factor.ratio.optimal
  
  
  #------ Testing ------#
  # Retrain model using optimal parameter from CV
  model.svmlight <- svmlight(Value ~ ., data = data.train, pathsvm = "binaries/svm_light_osx.8.4_i7", type = "C", svm.options = paste0("-t 0 -j ", cost.factor.ratio.optimal), scal = T)
  
  # compute error on testing set
  pred.svmlight <- predict(model.svmlight, newdata = test.x, scal = T)
  test.error.svmlight <- mean(abs(as.numeric(as.character(pred.svmlight$class)) - test.y))
  print(paste0("Cost Factor SVM Testing Error:   ", test.error.svmlight))
  
  # confusion matrix (testing)
  print("Cost Factor SVM Confusion Matrix")
  confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(as.character(pred.svmlight$class)), threshold = 0.5)
  print(confusion.mat)
  print(paste0("Value WR hit rate w/ classification (testing set):   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
  print(paste0("Value WR hit rate w/o classification(testing set):   ", sum(test.y==1)/length(test.y)))
  

  # running time
  ptm <- proc.time() - ptm
  print(ptm)
  
  
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.085_wks7-15_minfpts18.5_lag6.RData") # factor:
  
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.035_wks2-4_minfpts18.5.RData") # factor: 10
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.04_wks2-5_minfpts18.5.RData") # factor: 10
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.07_wks4-6_minfpts18.5.RData") # min cv
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.1_wks4-7_minfpts18.5.RData") # factor: 1.5
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.075_wks4-8_minfpts18.5.RData") # factor: 5
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.065_wks4-9_minfpts18.5.RData") # factor: 5
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.06_wks4-10_minfpts18.5.RData") # factor: 5
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.01_wks4-11_minfpts18.5.RData") # factor: 5
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.075_wks4-12_minfpts18.5.RData") # factor: 10
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.08_wks4-13_minfpts18.5.RData") # factor: 5
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.075_wks4-14_minfpts18.5.RData") # factor: 10
  # save.image(file = "optimizationCode/data_warehouse/datasets/cheapWR/models/svmlight_linear_costfactor0.08_wks4-15_minfpts18.5.RData") # factor: 5
}
