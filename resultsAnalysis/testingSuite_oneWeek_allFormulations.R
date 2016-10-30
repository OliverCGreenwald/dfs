#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")




####### SET PARAMETER VALUES #########
week.num <- 2
contest.entry.fee <- "$20"
predictions.source <- "_dfn" # Either "" or "_dfn" or "_dfn_perturbed" or "_fc"
formulation <- 7
overlap.lo <- 1 # overlap.lo and overlap.hi must be the same if exposure.range is not from 1 to 1
overlap.hi <- 9
exposure.range <- seq(from = 0.1, to = 1, by = 0.1) # must be from 1 to 1 if overlap.lo != overlap.hi
pnl_one_graph <- TRUE 







####### INITALIZE PNL MATRIX FOR STORING RESULTS #########
pnlMatrix <- matrix(data = NA, nrow = length(exposure.range), ncol = overlap.hi - overlap.lo + 2, dimnames = list(NULL, c(paste("Week", week.num),"Overlap1","Overlap2","Overlap3","Overlap4","Overlap5","Overlap6","Overlap7","Overlap8","Overlap9")))
pnlMatrix[1:10] <- exposure.range
  
####### LOAD FULL CONTEST RESULTS #########
file.name <- paste0("resultsAnalysis/data_warehouse/contest_results/", contest.entry.fee, "_contest_full_results_week", week.num, ".csv")
full.results.data <- read.csv(file = file.name, stringsAsFactors = F)
  
####### IMPORT AND CLEAN DK HISTORICAL FPTS DATA #########
file.name <- paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", week.num, ".csv")
player.performance <- read.csv(file = file.name, stringsAsFactors = F)
player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0
  
player.performance$Player <- sub(' Sr.','', player.performance$Player)
player.performance$Player <- sub(' Jr.','', player.performance$Player)
  
######## IMPORT PAYOUT STRUCTURE ########
file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", contest.entry.fee, "_payout_structure_week", week.num, ".csv")
payout.data <- read.csv(file = file.name, stringsAsFactors = F)
  
######## LOOP THROUGH OVERLAP PARAMETER VALUES ########
for (k in overlap.lo:overlap.hi) {
    
  # Loop through exposures
  for (exposure in exposure.range) {
      
    ####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
    file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, ".csv")
    lineups <- read.csv(file = file.name, stringsAsFactors = F)
      
    ######## CALCULATE FPTS FOR EACH LINEUP ########
    for (i in 1:ncol(lineups)) {
      lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
      lineups[,i] <- sub(' Sr.','', lineups[,i])
      lineups[,i] <- sub(' Jr.','', lineups[,i]) 
    }
    lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)
      
    total_results <- player.performance[,c('Player', 'Actual.Score')]
    lineups$total <- 0
      
    for (index in 1:nrow(lineups)){
      row <- t(lineups[index,])
      colnames(row) <- 'Player'
      row <- merge(row, total_results, by = 'Player')
      lineups$total[index] <- sum(row$Actual.Score)
    }
    
    if(pnl_one_graph != TRUE) {
      plot(lineups$total, main = paste0("Week ", week.num, ", Overlap ", k, ", Exposure ", exposure), xlab = "Lineup Index", ylab = "Lineup FPts")
      abline(h=200, col = "black", lty= 2)
      abline(h=220, col = "green")  
    }
    
      
    ######## CALCULATE PLACE AND PAYOUT FOR EACH LINEUP ########
    # print(paste0("Number of NAs: ", sum(is.na(lineups$total))))
    # lineups$total[1] <- 243 # sanity check
    for (i in 1:nrow(lineups)) {
      lineups$place[i] <- which.min(abs(full.results.data$Points-lineups$total[i])) # not precise but good estimate (can be done better probably)
      if (lineups$place[i] > payout.data$Place_hi[nrow(payout.data)]) {
        lineups$payout[i] <- 0
      }
      else {
        for (j in 1:nrow(payout.data)) {
          if (lineups$place[i] >= payout.data$Place_lo[j] && lineups$place[i] <= payout.data$Place_hi[j]) {
            lineups$payout[i] <- payout.data$Payout[j]
            break
          }
        }
      }
    }
      
    ######## FUNCTION FOR CALCULATING TOTAL PNL OF LINEUPS ########
    # won't be exact b/c not accounting for ties
    calculatePnL <- function(numberEntries, lineups) {
      lineups <- lineups[1:numberEntries,]  
      return(sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))
    }
      
    ######## ADD TO PNL MATRIX ########
    paste0("Total PnL: ", sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))
    pnlMatrix[pnlMatrix[,paste("Week", week.num)]==exposure, k+1] <- sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups)

      
    ######## PNL VS NUMBER OF LINEUPS ########
    pnls <- rep(0,nrow(lineups))
    for (i in 1:length(pnls)) {
      pnls[i] <- calculatePnL(nrow(lineups)-i+1, lineups)
    }
      
    numLineups <- seq(from = nrow(lineups), to = nrow(lineups)-length(pnls)+1)
    if(k == 1 & exposure == 0.1 & pnl_one_graph == TRUE)
    {
      plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l", col= k)#, ylim = c(-3000,2000))
      abline(h=0, col = "red")
    } else if (pnl_one_graph == TRUE){
      lines(numLineups, pnls, col = k)
    } else {
      plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l")
      abline(h=0, col = "red")
    }
    
  }
    
}
  
  
print(pnlMatrix)
#if (length(exposure.range) == 1) {
#  saveRDS(pnlMatrix, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", week.num, predictions.source, "_formulation", formulation, "_exposure_", 1, ".rds"))
#} else {
#  saveRDS(pnlMatrix, file = paste0("../resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", week.num, predictions.source, "_formulation", formulation, "_overlap_", overlap.lo, ".rds"))
#}
  
