#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/resultsAnalysis")

######## PERFORMANCE (PnL) OF VARIOUS STRATEGIES ########
# Some takeaways:
# - Using "one_lineup_Type_2" (QB-WR), we tend to lose less on bad weeks. Theoretically, we should win more on good weeks,
#   but that doesn't seem to consistently be the case over different num_overlap values. Perhaps "one_lineup_Type_2" is
#   better than "one_lineup_Type_3".
# - In general, formulations with looser constraints tend to perform better on bad weeks, which makes sense because
#   with tighter constraints we are going for high positive correlation within lineups so it's more boom or bust. The bust
#   part is clearly happening (week 2), but interestingly the boom part is not (no big wins yet using "one_lineup_Type_3").
# - Note the $1154 profit for "one_lineup_Type_2" (week 3, num_overlap 2), and the $651 profit for "one_lineup_Type_1" (week 3,
#   num_overlap 2). It seems that num_overlap 2 does well, but could be a coincidence. 
# - This is a loose pattern, but it appears that when num_overlap is small, larger N (number of lineups) tend to underperform
#   smaller N. In constrast, the distribution is fairly uniform for mid to large num_overlap.
# - "one_lineup_Type_1" appears to get unstable at higher num_overlap (week 3 was good week, but this strat begins losing money)

# WEEK 3:
# Formulation "one_lineup_Type_3":
# 150 Lineups:
#   num_overlap 1: -254
#   num_overlap 2: 241 (interesting plot) 
#     exposure 0.1: -158
#     exposure 0.2: -24
#     exposure 0.3:
#     exposure 0.4:
#     exposure 0.5:
#     exposure 0.6:
#     exposure 0.7:
#     exposure 0.8:
#     exposure 0.9:
#   num_overlap 3: 125
#   num_overlap 4: 224 (realized)
#   num_overlap 5: 261 (great plot)
#   num_overlap 6: 345 (an even better plot damn)
#   num_overlap 7: 361 (wow just keep going up)
#   num_overlap 8: 249
#   num_overlap 9: 750 (lol, same lineup 150x)

# Formulation "one_lineup_Type_2":
# 150 Lineups:
#   num_overlap 1: -253
#   num_overlap 2: 1154 (wow, there was a killer lineup)
#   num_overlap 3: 215
#   num_overlap 4: 279
#   num_overlap 5: 186
#   num_overlap 6: 203
#   num_overlap 7: 115
#   num_overlap 8: 175
#   num_overlap 9: 450

# Formulation "one_lineup_Type_1":
# 150 Lineups:
#   num_overlap 1: -124
#   num_overlap 2: 651 (wow, killer lineup)
#   num_overlap 3: 199
#   num_overlap 4: 225
#   num_overlap 5: 61
#   num_overlap 6: -1
#   num_overlap 7: -102
#   num_overlap 8: -139
#   num_overlap 9: -450

# WEEK 2:
# Formulation "one_lineup_Type_3":
# 150 Lineups:
#   num_overlap 1: -352 (awful)
#   num_overlap 2: -144 (gets bad after first ~40 lineups)
#   num_overlap 3: -120 (get bad after first ~55 lineups)
#   num_overlap 4: -55 (realized)
#   num_overlap 5: -82 (screwed basically for all N)
#   num_overlap 6: -129 (yikes literally negative for all N)
#   num_overlap 7: -67 (positive a few times)
#   num_overlap 8: -119 (positive a few times)
#   num_overlap 9: -450

# Formulation "one_lineup_Type_2":
# 150 Lineups:
#   num_overlap 1: -323
#   num_overlap 2: -76
#   num_overlap 3: -80
#   num_overlap 4: -31
#   num_overlap 5: 11 (interesting graph: fluctuates around 0s)
#   num_overlap 6: -5 (also interesting: pretty good then pretty bad)
#   num_overlap 7: 47
#   num_overlap 8: -7
#   num_overlap 9: 300

# Formulation "one_lineup_Type_1":
# 150 Lineups:
#   num_overlap 1: -291
#   num_overlap 2: -131
#   num_overlap 3: 111 (big win at ~80)
#   num_overlap 4: 59
#   num_overlap 5: 98
#   num_overlap 6: 191
#   num_overlap 7: 58
#   num_overlap 8: 95
#   num_overlap 9: 600

####### FUNCTION FOR SETTING WEEK NUMBER, LINEUP FILE, AND PARAMETERS FOR TESTING #########
testParameters <- function(week.num, entry.fee, formulation, overlap, exposure) {
  
  #file.name <- paste0("../optimizationCode/submitted_lineups/week", week.num, "_lineups.csv") # change this! (some file path)
  #file.name <- paste0("../resultsAnalysis/data_warehouse/testing_lineups/week", week.num, "_300lineups.csv") # change this! (some file path)
  file.name <- paste0("../resultsAnalysis/data_warehouse/testing_lineups/week", week.num, "_dfn_formulation", formulation, "_overlap_", overlap, "_exposure_", exposure, ".csv")
  
  lineups <- read.csv(file = file.name, stringsAsFactors = F)
  list <- list(week.num, entry.fee, lineups)
  return(list)
}

####### TEST VARIOUS PARAMETERS #########
returnedParams <- testParameters(week.num = 2, entry.fee = "$3", formulation = 2, overlap = 4, exposure = 1)
week.num <- returnedParams[[1]]
contest.entry.fee <- returnedParams[[2]]
lineups <- returnedParams[[3]]
  
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
  
######## FUNCTION FOR CALCULATING TOTAL PAYOUT OF LINEUPS ########
# won't be exact b/c not accounting for ties
calculatePnL <- function(numberEntries, lineups) {
  lineups <- lineups[1:numberEntries,]  
  return(sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))
}

paste0("Total PnL: ",sum(lineups$payout) - as.numeric(substring(contest.entry.fee, 2)) * nrow(lineups))

######## FIND OPTIMAL NUMBER OF LINEUPS ########
pnls <- rep(0,nrow(lineups))
for (i in 1:length(pnls)) {
  pnls[i] <- calculatePnL(nrow(lineups)-i+1, lineups)
}

numLineups <- seq(from = nrow(lineups), to = nrow(lineups)-length(pnls)+1)
plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l")
abline(h=0, col = "red")


