#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/resultsAnalysis")

####### SET WEEK NUMBER, LINEUP FILE, AND ENTRY FEE FOR TESTING #########
week.num <- 3 # change this! (any past week)

#file.name <- paste0("../optimizationCode/submitted_lineups/week", week.num, "_lineups.csv") # change this! (some file path)
file.name <- paste0("../resultsAnalysis/data_warehouse/testing_lineups/week", week.num, "_300lineups.csv") # change this! (some file path)
lineups <- read.csv(file = file.name, stringsAsFactors = F)

contest.entry.fee <- "$3" # change this! ($3 or $20)
  
####### IMPORT AND CLEAN DK HISTORICAL FPTS DATA FOR THE WEEK #########
file.name <- paste0("data_warehouse/player_weekly_performance/draftkings_player_production_week", week.num, ".csv")
player.performance <- read.csv(file = file.name, stringsAsFactors = F)
player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0

player.performance$Player <- sub(' Sr.','', player.performance$Player)
player.performance$Player <- sub(' Jr.','', player.performance$Player)

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

plot(lineups$total, main = paste0("Week ", week.num), xlab = "Lineup Index", ylab = "Lineup FPts")

######## IMPORT PAYOUT STRUCTURE ########
file.name <- paste0("data_warehouse/weekly_payout_structure/", contest.entry.fee, "_payout_structure_week", week.num, ".csv")
payout.data <- read.csv(file = file.name, stringsAsFactors = F)

######## CALCULATE PLACE AND PAYOUT FOR EACH LINEUP ########
file.name <- paste0("data_warehouse/contest_results/", contest.entry.fee, "_contest_full_results_week", week.num, ".csv")
full.results.data <- read.csv(file = file.name, stringsAsFactors = F)

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

# the following won't be exact b/c not accounting for ties
# print(paste0("Total payout: ", sum(lineups$payout)))
# if (contest.entry.fee == "$3") {
#   print(paste0("Total PnL: ", sum(lineups$payout) - 3*nrow(lineups)))
# }
# if (contest.entry.fee == "$20") {
#   print(paste0("Total PnL: ", sum(lineups$payout) - 20*nrow(lineups)))
# }
  
######## FUNCTION FOR CALCULATING TOTAL PAYOUT OF LINEUPS ########
calculatePnL <- function(numberEntries, lineups) {
  lineups <- lineups[1:numberEntries,]  
  return(sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))
}

######## FIND OPTIMAL NUMBER OF LINEUPS ########
pnls <- rep(0,nrow(lineups))
for (i in 1:length(pnls)) {
  pnls[i] <- calculatePnL(nrow(lineups)-i+1, lineups)
  print(pnls[i])
}

numLineups <- seq(from = nrow(lineups), to = nrow(lineups)-length(pnls)+1)
plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l")
abline(h=0, col = "red")
