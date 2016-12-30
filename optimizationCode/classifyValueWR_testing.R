#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we test models for classifying ValueWR.


####### LOAD MODEL #######
# "svmlight_linear_costfactor0.075_wks6-15_minfpts18.5.RData"
# "svmlight_linear_costfactor0.04_wks6-14_minfpts18.5.RData"
# "svmlight_linear_costfactor0.07_wks6-13_minfpts18.5.RData"
# "svmlight_linear_costfactor0.03_wks6-12_minfpts18.5.RData"
# "svmlight_linear_costfactor0.04_wks6-11_minfpts18.5.RData"
# "svmlight_linear_costfactor0.06_wks6-10_minfpts18.5.RData"
# "svmlight_linear_costfactor0.06_wks6-9_minfpts18.5.RData"
save.model.name <- "svmlight_linear_costfactor0.06_wks6-9_minfpts18.5.RData"
load(paste0("optimizationCode/data_warehouse/datasets/cheapWR/models/", save.model.name))


####### PARAMETERS #######
wk <- 10
salary.threshold <- 5000
fpts.threshold <- 18.5

####### IMPORT LIBRARIES #########
library("klaR")


####### LOAD DATA FOR WEEK TO TEST #######
test.data <- read.csv(file = paste0("optimizationCode/data_warehouse/datasets/cheapWR/weekly_data/includes_historicalfpts3wklag/includes_names-fpts/cheapwr_data_week", wk, ".csv"), stringsAsFactors = F)
temp.names <- test.data$Player.Name
temp.fpts <- test.data$Actual.FP
test.data$Player.Name <- NULL
test.data$Actual.FP <- NULL
test.data$Player.Name <- temp.names
test.data$Actual.FP <- temp.fpts
test.x <- test.data[,1:(ncol(test.data)-3)]
test.y <- test.data$Value

sum(test.y)

# test.x <- test.x[order(test.x$Likes),]

#######  #######
# compute error 
pred.svmlight <- predict(model.svmlight, newdata = test.x, scal = T)
pred.svmlight$class
test.error.svmlight <- mean(abs(as.numeric(as.character(pred.svmlight$class)) - test.y))
print(paste0("Cost Factor SVM Testing Error:   ", test.error.svmlight))

# confusion matrix (testing)
print("Cost Factor SVM Confusion Matrix")
confusion.mat <- confusion.matrix(obs = test.y, pred = as.numeric(as.character(pred.svmlight$class)), threshold = 0.5)
print(confusion.mat)
print(paste0("Value WR hit rate w/ model:   ", confusion.mat[2,2]/(confusion.mat[2,1] + confusion.mat[2,2])))
print(paste0("Value WR hit rate w/o model:   ", sum(test.y==1)/length(test.y))) 


####### BASELINE COMPARISON #######
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

#   (Baseline)
sum(baseline.data$ValueWR==1 & baseline.data$ValueWR.Actual==1) / sum(baseline.data$ValueWR)

sum(baseline.data$ValueWR==1 & baseline.data$ValueWR.Actual==1)
sum(baseline.data$ValueWR)

