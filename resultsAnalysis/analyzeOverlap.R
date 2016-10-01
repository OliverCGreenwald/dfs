#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/resultsAnalysis")

load("../resultsAnalysis/data_warehouse/testing_lineups/RData_files/pnlMatrix_week3_dfn_formulation2_exposure_1.RData")
allResults <- pnlMatrix
colnames(allResults)[2] <- "wk3_form2"

load("../resultsAnalysis/data_warehouse/testing_lineups/RData_files/pnlMatrix_week2_dfn_formulation2_exposure_1.RData")
allResults <- cbind(allResults, pnlMatrix[,'PnL'])
colnames(allResults)[3] <- "wk2_form2"

load("../resultsAnalysis/data_warehouse/testing_lineups/RData_files/pnlMatrix_week3_dfn_formulation3_exposure_1.RData")
allResults <- cbind(allResults, pnlMatrix[,'PnL'])
colnames(allResults)[4] <- "wk3_form3"

load("../resultsAnalysis/data_warehouse/testing_lineups/RData_files/pnlMatrix_week2_dfn_formulation3_exposure_1.RData")
allResults <- cbind(allResults, pnlMatrix[,'PnL'])
colnames(allResults)[5] <- "wk2_form3"