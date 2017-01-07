#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we test models for classifying ValueWR.
# Option to write to file (data_warehouse/2016_cleaned_input/wk[x]/includes_thu-mon/model)
# Notes:
#   - don't use test.data$Inj <- NULL for wks 9-16 linear kernel


####### IMPORT LIBRARIES #########
library("klaR")
library("kernlab")
library("SDMTools")


####### LOAD MODEL #######
#
# "svmlight_linear_costfactor0.07_wks6-16_minfpts18.5.RData" # "svmlight_linear_costfactor0.06_wks6-16_minfpts18.5.RData"
# "svmlight_linear_costfactor0.075_wks6-15_minfpts18.5.RData"
# "svmlight_linear_costfactor0.04_wks6-14_minfpts18.5.RData"
# "svmlight_linear_costfactor0.07_wks6-13_minfpts18.5.RData"
# "svmlight_linear_costfactor0.03_wks6-12_minfpts18.5.RData"
# "svmlight_linear_costfactor0.04_wks6-11_minfpts18.5.RData"
# "svmlight_linear_costfactor0.06_wks6-10_minfpts18.5.RData"
# "svmlight_linear_costfactor0.06_wks6-9_minfpts18.5.RData"
# 
# "svmlight_linear_costfactor0.03_wks4-15_minfpts18.5.RData" (tuning: , result: decent)
# "svmlight_linear_costfactor0.04_wks4-14_minfpts18.5.RData" (tuning: less obvious, result: decent, gets the top 3 dudes)
# "svmlight_linear_costfactor0.04_wks4-13_minfpts18.5.RData" (tuning: obvious, result: decent, barely beats baseline, cuts out a lot of 0.0 fpts)
# "svmlight_linear_costfactor0.05_wks4-12_minfpts18.5.RData" (tuning: quite obvious, result: pretty good)
# "svmlight_linear_costfactor0.05_wks4-11_minfpts18.5.RData" (tuning: quite obvious, result: ehh, doesn't beat baseline)
# "svmlight_linear_costfactor0.03_wks4-10_minfpts18.5.RData" (tuning: less obvious, result: decent, model chooses value wr hits 2/3 vs baseline value wr hits 1/3) (consider 0.025 and 0.035)
# "svmlight_linear_costfactor0.035_wks4-9_minfpts18.5.RData" (tuning: obvious, result: decent (3/3) but doesn't beat baseline)
#
# "svmlight_rbf_costfactor0.025_param1e-06_wks4-15_minfpts18.5.RData" (tuning weight: 1.25, result: good)
# "svmlight_rbf_costfactor0.045_param1.5e-07_wks4-14_minfpts18.5.RData" (tuning weight: 1.25, result: good)
# "svmlight_rbf_costfactor0.035_param2.2e-07_wks4-13_minfpts18.5.RData" (tuning weight: 1.25, result: good) # "svmlight_rbf_costfactor0.02_param2.15e-06_wks4-13_minfpts18.5.RData" (tuning weight: 3, result: meh)
# "svmlight_rbf_costfactor0.03_param1e-06_wks4-12_minfpts18.5.RData" (tuning weight: 1.25, result: good)
# "svmlight_rbf_costfactor0.01_param2.15e-06_wks4-11_minfpts18.5.RData" (tuning weight 2.0, result: slightly less than baseline but has less 0.0 fpts players)
# "svmlight_rbf_costfactor0.035_param6.8e-07_wks4-10_minfpts18.5.RData" (tuning weight: 1.25, result: good)
# "svmlight_rbf_costfactor0.015_param3.16e-06_wks4-9_minfpts18.5.RData" (turning wieght: 1.25, result: good)
#
# Best Linear Kernel Models Weekly (gets better week over week):
# Wk 16: "svmlight_linear_costfactor0.08_wks4-15_minfpts18.5.RData"
# Wk 15: "svmlight_linear_costfactor0.075_wks4-14_minfpts18.5.RData"
# Wk 14: "svmlight_linear_costfactor0.08_wks4-13_minfpts18.5.RData"
# Wk 13: "svmlight_linear_costfactor0.075_wks4-12_minfpts18.5.RData"
# Wk 12: "svmlight_linear_costfactor0.01_wks4-11_minfpts18.5.RData"
# Wk 11: "svmlight_linear_costfactor0.06_wks4-10_minfpts18.5.RData"
# Wk 10: "svmlight_linear_costfactor0.065_wks4-9_minfpts18.5.RData"
# Wk 9: "svmlight_linear_costfactor0.075_wks4-8_minfpts18.5.RData"
# Wk 8: "svmlight_linear_costfactor0.1_wks4-7_minfpts18.5.RData"
# Wk 7: "svmlight_linear_costfactor0.07_wks4-6_minfpts18.5.RData"
# Wk 6: "svmlight_linear_costfactor0.04_wks2-5_minfpts18.5.RData"
#
# Best RBF Kernel Models Weekly (gets better week over week):
# Wk 16: "svmlight_rbf_costfactor0.07_gamma1.47e-06_wks4-15_minfpts18.5.RData"
# Wk 15: 
# Wk 14: 
# Wk 13: 
# Wk 12: 
# Wk 11: 
# Wk 10: 


save.model.name <- "svmlight_linear_costfactor0.06_wks2-5_minfpts18.5.RData"
load(paste0("optimizationCode/data_warehouse/datasets/cheapWR/models/", save.model.name))


####### WRITE TO FILE? #######
write.bool <- F # this needs to be here to ovewrite loaded model variable


####### PARAMETERS #######
wk <- 6
salary.threshold <- 5000
fpts.threshold <- 18.5 # if this is not 18.5 then need to change the baseline files (rerun valueWR.R and change threshold)

spike.bool <- T

####### LOAD DATA FOR WEEK TO TEST #######
test.data <- read.csv(file = paste0("optimizationCode/data_warehouse/datasets/cheapWR/weekly_data/includes_historicalfpts",historicalfpts.lag,"wklag/includes_names-fpts/cheapwr_data_week", wk, ".csv"), stringsAsFactors = F) # note: historicalfpts.lag is in saved model RData file
test.data$Inj <- NULL # set to null if trained without this feature
temp.names <- test.data$Player.Name
temp.fpts <- test.data$Actual.FP
test.data$Player.Name <- NULL
test.data$Actual.FP <- NULL
test.data$Player.Name <- temp.names
test.data$Actual.FP <- temp.fpts
test.x <- test.data[,1:(ncol(test.data)-3)]
test.y <- test.data$Value


####### Print Value WR for this week #######
test.data[test.data$Value==1, c('Player.Name','Actual.FP','Salary')] # players that actually are 1


####### PREDICT USING MODEL (SVMLIGHT) #######
# compute error 
pred.svmlight <- predict(model.svmlight, newdata = test.x, scal = T)
pred.svmlight$class
test.error.svmlight <- mean(abs(as.numeric(as.character(pred.svmlight$class)) - test.y))
print(paste0("Cost Factor SVM Testing Error:   ", test.error.svmlight))

# confusion matrix
print("Confusion Matrix")
confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(as.character(pred.svmlight$class)), threshold = 0.5)
print(confusion.mat)
print(paste0("Value WR hit rate w/ model:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
print(paste0("Value WR hit rate w/o model:   ", sum(test.y==1)/length(test.y))) 

# print players predicted 1, actual 1, and missed
pred.value <- test.data[pred.svmlight$class==1, c('Player.Name','Actual.FP','Salary')] # players that model predicts 1 (all cheap wr in set)
pred.value
test.data[pred.svmlight$class==1 & test.data$Value==1, c('Player.Name','Actual.FP','Salary')] # players that model predicts 1 and actually are 1 (value wr hits)
players.missed <- test.data$Player.Name[test.data$Value==1][!(test.data$Player.Name[test.data$Value==1] %in% test.data$Player.Name[pred.svmlight$class==1])]
test.data[test.data$Player.Name %in% players.missed, c('Player.Name','Actual.FP','Salary')] # players that the model predicts 0 but are actually 1 (value wr not in set)

# sort by posteriors
pred.posterior <- as.data.frame(cbind(pred.svmlight$posterior, test.data[,c('Player.Name','Actual.FP','Salary')]))
pred.posterior <- pred.posterior[order(pred.posterior$`1`, decreasing = T),]
pred.posterior <- pred.posterior[pred.posterior$`1` > 0.5,]
pred.posterior.spike <- pred.posterior[1:(floor(nrow(pred.posterior)*0.50)),] # keep upper half of sorted posteriors
pred.posterior.spike <- pred.posterior.spike[order(pred.posterior.spike$Actual.FP, decreasing = T),]
pred.posterior.stay <- pred.posterior[-(1:(floor(nrow(pred.posterior)*0.50))),]
pred.posterior.stay <- pred.posterior.stay[order(pred.posterior.stay$Actual.FP, decreasing = T),]
# View(pred.posterior.spike)
# View(pred.posterior.stay)
sum(test.data$Value) # total number of value wr
sum(pred.posterior.spike$Actual.FP >= fpts.threshold) # number of value wr in the spike


####### PREDICT USING MODEL (SVM) #######
# # compute error 
# pred.svm <- predict(model.svm, newdata = test.x)
# test.error.svm <- mean(abs(as.numeric(as.character(pred.svm)) - test.y))
# print(paste0("SVM Testing Error:   ", test.error.svm))
# 
# # confusion matrix
# print("Confusion Matrix")
# confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(as.character(pred.svm)), threshold = 0.5)
# print(confusion.mat)
# print(paste0("Value WR hit rate w/ model:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
# print(paste0("Value WR hit rate w/o model:   ", sum(test.y==1)/length(test.y))) 
# 
# # print players predicted 1, actual 1, and missed
# pred.value <- test.data[pred.svm==1, c('Player.Name','Actual.FP','Salary')] # players that model predicts 1 (all cheap wr in set)
# pred.value
# test.data[pred.svm==1 & test.data$Value==1, c('Player.Name','Actual.FP','Salary')] # players that model predicts 1 and actually are 1 (value wr hits)
# players.missed <- test.data$Player.Name[test.data$Value==1][!(test.data$Player.Name[test.data$Value==1] %in% test.data$Player.Name[pred.svm==1])]
# test.data[test.data$Player.Name %in% players.missed, c('Player.Name','Actual.FP','Salary')] # players that the model predicts 0 but are actually 1 (value wr not in set)


####### BASELINE (ValueWR currently used in 2016_cleaned_input) #######
# load data
baseline.data <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/includes_thu-mon/offensive_players.csv"), stringsAsFactors = F)
baseline.data <- baseline.data[baseline.data$Salary <= salary.threshold & baseline.data$Position=="WR",]

dfn.updated.data <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', wk, '.csv'), stringsAsFactors = F)

# add Actual.FP
baseline.data$Name <- sub(' Sr.', '', baseline.data$Name)
baseline.data$Name <- sub(' Jr.', '', baseline.data$Name)
baseline.data$Actual.FP <- dfn.updated.data$Actual.FP[match(paste0(baseline.data$Name,baseline.data$Position), paste0(dfn.updated.data$Player.Name,dfn.updated.data$Pos))]

# add ValueWR.Actual
baseline.data$ValueWR.Actual <- 0
baseline.data$ValueWR.Actual[baseline.data$Actual.FP > fpts.threshold] <- 1

# print
paste0("Num Value WR hits (baseline): ", sum(baseline.data$ValueWR==1 & baseline.data$ValueWR.Actual==1))
paste0("Num total Value WR labels (baseline): ", sum(baseline.data$ValueWR))
paste0("Value WR hit rate (baseline): ",sum(baseline.data$ValueWR==1 & baseline.data$ValueWR.Actual==1) / sum(baseline.data$ValueWR))

# print players predicted 1, actual 1, and missed
baseline.data[baseline.data$ValueWR==1, c('Name','Actual.FP','Salary')] # (all cheap wr in set)
baseline.data[baseline.data$ValueWR==1 & baseline.data$ValueWR.Actual==1, c('Name','Actual.FP','Salary')] # (value wr hits)
baseline.data[baseline.data$ValueWR==0 & baseline.data$ValueWR.Actual==1, c('Name','Actual.FP','Salary')] # (value wr not in set)


####### WRITE TO FILE #######
if (write.bool==T) {
  ####### UPDATE includes_thu-mon/model1 OFFENSIVE_PLAYERS CSV #######
  temp <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/includes_thu-mon/offensive_players.csv"), stringsAsFactors = F)
  temp$Name[temp$Name=="Will Fuller V"] <- "Will Fuller"
  if (spike.bool == F) {
    temp$ValueWR <- 0 # reset
  }
  
  ####### UPDATE model1 OFFENSIVE_PLAYERS CSV #######
  # temp <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), stringsAsFactors = F)
  # temp$Name[temp$Name=="Will Fuller V"] <- "Will Fuller"
  # if (spike.bool == F) {
  #   temp$ValueWR <- 0 # reset
  # }
  
  
  ####### Uncomment one of the three options #######
  #----- Set ValueWR to model's predictions -----#
  # valuewr.set.model <- test.data[pred.svmlight$class==1, c('Player.Name','Actual.FP','Salary')] # players that model predicts 1 (all cheap wr in set)
  # temp$ValueWR[temp$Name %in% valuewr.set.model$Player.Name & temp$Salary %in% valuewr.set.model$Salary] <- 1
  # print(sum(temp$ValueWR))
  # print(nrow(valuewr.set.model))
  # print(valuewr.set.model$Player.Name[!(valuewr.set.model$Player.Name %in% temp$Name)]) # missing players
  #----------#
  
  
  #-----Spike upper 75th percentile posteriors -----#
  # temp$Projection_dfn[temp$Name %in% pred.posterior.spike$Player.Name] <- 1.25*temp$Projection_dfn[temp$Name %in% pred.posterior.spike$Player.Name]
  # print(sum(temp$Name %in% pred.posterior.spike$Player.Name))
  # print(length(pred.posterior.spike$Player.Name))
  #----------#
  
  
  #-----Spike the predicted 1's (from baseline) -----#
  temp$Projection_dfn[temp$Name %in% pred.value$Player.Name] <- 1.5*temp$Projection_dfn[temp$Name %in% pred.value$Player.Name]
  print(sum(temp$Name %in% pred.value$Player.Name))
  print(length(pred.value$Player.Name))
  #----------#
  
  
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/includes_thu-mon/model1/offensive_players.csv"), row.names = F)
  # write.csv(temp, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/model1/offensive_players.csv"), row.names = F)
}
  
