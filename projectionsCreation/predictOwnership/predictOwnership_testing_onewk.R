#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we test models for predicting the ownership rate. Offense only. Sunday only.
# Note that we are only testing on players with full feature set (we removed those with NAs in
# predictOwnership.R). Did not differentiate by position.


####### IMPORT LIBRARIES #########
library("klaR")
library("kernlab")
library("SDMTools")


####### LOAD MODEL #######
# Best Models:
# Wk 17: "elasticnet_alpha1.0_wks4-16.RData"
# Wk 16: "elasticnet_alpha0.35_wks4-15.RData"
# Wk 15: "elasticnet_alpha0.65_wks4-14.RData"
# Wk 14: "elasticnet_alpha0.25_wks4-13.RData"
# Wk 13: "elasticnet_alpha0.2_wks4-12.RData"
# Wk 12: "elasticnet_alpha0.05_wks4-11.RData"
# Wk 11: "elasticnet_alpha0.1_wks4-10.RData"
# Wk 10: "elasticnet_alpha0.6_wks4-9.RData"
# Wk 9: "elasticnet_alpha0.75_wks4-8.RData"
# Wk 8: "elasticnet_alpha0.6_wks4-7.RData"
# Wk 7: "elasticnet_alpha0.7_wks4-6.RData"


save.model.name <- "elasticnet_alpha0.7_wks4-6.RData"
load(paste0("projectionsCreation/predictOwnership/data_warehouse/models/", save.model.name))


####### WRITE TO FILE? #######
write.bool <- T # this needs to be here to ovewrite loaded model variable


####### PARAMETERS #######
wk <- 7


####### LOAD DATA FOR WEEK TO TEST #######
# read file
test.data <- read.csv(file = paste0("projectionsCreation/predictOwnership/data_warehouse/finalized_datasets/includes_historicalfpts",historicalfpts.lag,"wklag/ownership_data_week", wk, ".csv"), stringsAsFactors = F) # wk's data (e.g. wk 17 if predicting on wk 4-16)

# remove player.name and actual.fp columns (kept for analysis of predictions in predictOwnership.R)
temp.names <- test.data$Player.Name
temp.fpts <- test.data$Actual.FP
test.data$Player.Name <- NULL
test.data$Actual.FP <- NULL
test.data$Player.Name <- temp.names
test.data$Actual.FP <- temp.fpts
test.x <- test.data[,1:(ncol(test.data)-3)]
test.y <- test.data$Ownership.Pctg


####### PREDICT USING MODEL (SVMLIGHT) #######
# prediction
pred.elasticnet <- predict(object=model.elasticnet, newdata=test.x)

# MSE
print(paste0("MSE (Wk ", wk , "):   ", sum((pred.elasticnet-test.y)^2) / (length(test.y)-2)))
print(paste0("MSE Train (Wk 4-", wk-1, "):   ", mse.train))
print(paste0("MSE Test (Wk 4-", wk-1, "):   ", mse.test))

# R-squared
print(paste0("R-Squared (Wk ", wk , "):   ", 1 - sum((pred.elasticnet-test.y)^2) / ((length(test.y)-1)*var(test.y))))
print(paste0("R-Squared Train (Wk 4-", wk-1, "):   ", rsq.train))
print(paste0("R-Squared Test (Wk 4-", wk-1, "):   ", rsq.test))


####### Print top 5 highest Ownership.Pctg and Ownership.Pctg.Pred players for this week #######
test.results <- test.data
test.results$Ownership.Pctg.Pred <- as.vector(pred.elasticnet)
test.results[sort(test.results$Ownership.Pctg, decreasing = T, ind = T)$ix[1:5], c('Player.Name','Proj.FP','Actual.FP','Ownership.Pctg','Ownership.Pctg.Pred')]
test.results[sort(test.results$Ownership.Pctg.Pred, decreasing = T, ind = T)$ix[1:5], c('Player.Name','Proj.FP','Actual.FP','Ownership.Pctg','Ownership.Pctg.Pred')]


####### ADJUST DFN PROJECTIONS IN CLEANED INPUT FILES USING Ownership.Pctg.Pred #######
# load cleaned input file for wk
temp <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), stringsAsFactors = F)

# clean names for matching
temp$Name <- sub(' Sr.', '', temp$Name)
temp$Name <- sub(' Jr.', '', temp$Name)
temp$Name[temp$Name=="Will Fuller V"] <- "Will Fuller"

# add Ownership.Pctg.Pred column (will be removed later)
temp$Ownership.Pctg.Pred <- test.results$Ownership.Pctg.Pred[match(paste0(temp$Name, temp$Salary), paste0(test.results$Player.Name, test.results$Salary))] # david johnson, Ryan Griffin
temp$Ownership.Pctg.Pred <- test.results$Ownership.Pctg.Pred[match(paste0(temp$Name), paste0(test.results$Player.Name))] # david johnson, Ryan Griffin
sum(!is.na(temp$Ownership.Pctg.Pred)) # should be length of test.data

# remove rows with NAs b/c we can't use these players (not full feature set) and remove players projected 0.0 b/c will never pick these
temp.cleaned <- temp[!is.na(temp$Ownership.Pctg.Pred) & temp$Projection_dfn>0,]

# Epsilon perturb equal projections (higher projection for lower Ownership.Pctg.Pred)
temp.unique <- unique(temp.cleaned$Projection_dfn) # unique projections
count.equal <- 0 # store num sets of players with same projected value
for (i in 1:length(temp.unique)) {
  # subset by unique projection values
  temp.subset <- temp.cleaned[temp.cleaned$Projection_dfn==temp.unique[i],]
  
  # epsilon perturb if there is more than one player with this projection value
  if (nrow(temp.subset) > 1) {
    # define epsilon pertubations (start with 0 b/c lowest Ownership.Pctg.Pred player's prediction left unchanged)
    epsilons <- seq(from = 0, by = 0.001, length.out = nrow(temp.subset))
    
    # sort in ascending order and add epsilon pertubations
    temp.subset[order(temp.subset$Ownership.Pctg.Pred, decreasing = T),'Projection_dfn'] <- temp.subset[order(temp.subset$Ownership.Pctg.Pred, decreasing = T),'Projection_dfn'] + epsilons
    
    # replace original predictions with epsilon perturbed predictions
    temp.cleaned[temp.cleaned$Projection_dfn==temp.unique[i], 'Projection_dfn'] <- temp.subset[,'Projection_dfn']
    
    # add one to count.equal
    count.equal <- count.equal + 1
  }
}
print(paste0("Num Equal Projections Epsilon Perturbed:   ", count.equal))

# replace original cleaned input file with the epsilon perturbed projections (note that we omitted the players with NAs)
temp[!is.na(temp$Ownership.Pctg.Pred) & temp$Projection_dfn>0,] <- temp.cleaned


####### WRITE TO FILE #######
if (write.bool==T) {
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/ownership_model/offensive_players.csv"), row.names = F)
}

