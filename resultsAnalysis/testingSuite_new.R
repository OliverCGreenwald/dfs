#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we compute the PnLs of lineups for testing purposes.
# Special notes for contest.entry.fee: for week 9, use $4 in lieu of $3 and for week 10, $27 in lieu of $20


####### IMPORT LIBRARIES #########
library('stringr')


####### SET PARAMETER VALUES #########
week.lo <- 2
week.hi <- 13
contest.entry.fee <- "$20" # note: if "$20", we use "$27" after week 9; if "$3", we use "$4" for week 10
predictions.source <- "_dfn" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
formulation <- 4
overlap.lo <- 4 # overlap.lo and overlap.hi must be the same if exposure.range is not from 1 to 1
overlap.hi <- 4
exposure.range <- seq(from = 0.4, to = 0.4, by = 0) # must be from 1 to 1 if overlap.lo != overlap.hi
freqInd <- "" # _FreqInd or ""
num.lineups <- "" # "" or "_numlineups_1000"
missing.data.1M.contest.wk <- c(10) # enter weeks that we don't have complete data for in the $1M to 1st contest
missing.data.50k.contest.wk <- c() # enter weeks that we don't have complete data for in the $50k to 1st contest

#
if (contest.entry.fee == '$3') {
contest.name <- "$50K"
} else {
contest.name <- "$1M"
}

####### INITALIZE PNL MATRIX FOR STORING RESULTS #########
if (week.lo != week.hi & overlap.lo == overlap.hi & length(exposure.range) == 1) {
weeks.count <- week.hi-week.lo+1
pnlMatrix <- matrix(data = NA, nrow = weeks.count, ncol = 2, dimnames = list(NULL, c("Week","PnL")))
pnlMatrix[1:weeks.count,'Week'] <- week.lo:week.hi
} else if (length(exposure.range) == 1) {
pnlMatrix <- matrix(data = NA, nrow = 9, ncol = 2, dimnames = list(NULL, c("Overlap","PnL")))
pnlMatrix[1:9,'Overlap'] <- 1:9  
} else {
pnlMatrix <- matrix(data = NA, nrow = 10, ncol = 2, dimnames = list(NULL, c("Exposure","PnL")))
pnlMatrix[1:10, 'Exposure'] <- exposure.range
}

# Loop through weeks
par(mfrow=c(2,2))
for (week.num in week.lo:week.hi) {
if ((week.num %in% missing.data.1M.contest.wk & contest.entry.fee == '$20') | (week.num %in% missing.data.50k.contest.wk & contest.entry.fee == '$3')) {
# do nothing
} else {
####### LOAD FULL CONTEST RESULTS #########
if (contest.entry.fee=='$3' & week.num == 10) {
full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", '$4', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
} else if (contest.entry.fee=='$20' & week.num > 9) {
full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", '$27', "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F)
} else {
full.results.data <- read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/", contest.entry.fee, "_contest_full_results_week", week.num, ".csv"), stringsAsFactors = F) 
}

####### IMPORT AND CLEAN DK HISTORICAL FPTS DATA #########
# player.performance <- read.csv(file = paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", week.num, ".csv"), stringsAsFactors = F)
# player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0
# 
# player.performance$Player <- sub(' Sr.','', player.performance$Player)
# player.performance$Player <- sub(' Jr.','', player.performance$Player)

player.performance <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', week.num, ".csv"), stringsAsFactors = F)
player.performance.def <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week', week.num, ".csv"), stringsAsFactors = F)

# clean defense names
temp.def.names <- str_split_fixed(player.performance.def$Player.Name, " ", 3) # split at " "
for (z in 1:nrow(temp.def.names)) {
if (temp.def.names[z,3] == "") {
player.performance.def$Player.Name[z] <- temp.def.names[z,2]
} else {
player.performance.def$Player.Name[z] <- temp.def.names[z,3]
}
}

player.performance$Actual.FP[is.na(player.performance$Actual.FP)] <- 0 # be careful with this
player.performance.def$Actual.FP[is.na(player.performance.def$Actual.FP)] <- 0 # be careful with this

######## IMPORT PAYOUT STRUCTURE ########
if (contest.entry.fee=='$3' & week.num == 10) {
file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", '$4', "_payout_structure_week", week.num, ".csv") 
} else if (contest.entry.fee=='$20' & week.num > 9) {
file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", '$27', "_payout_structure_week", week.num, ".csv") 
} else {
file.name <- paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/", contest.entry.fee, "_payout_structure_week", week.num, ".csv") 
}
payout.data <- read.csv(file = file.name, stringsAsFactors = F)

######## LOOP THROUGH OVERLAP PARAMETER VALUES ########
# k <- overlap.lo
for (k in overlap.lo:overlap.hi) {

# Loop through exposures
# exposure <- exposure.range
for (exposure in exposure.range) {

####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, ".csv")
lineups <- read.csv(file = file.name, stringsAsFactors = F)

######## CALCULATE FPTS FOR EACH LINEUP ########
for (i in 1:ncol(lineups)) {
lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
lineups[,i] <- sub(' Sr.','', lineups[,i])
lineups[,i] <- sub(' Jr.','', lineups[,i]) 
}
lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)

# total_results <- player.performance[,c('Player', 'Actual.Score')]
total_results <- player.performance[,c('Player.Name', 'Actual.FP')]
total_results <- rbind(total_results, player.performance.def[,c('Player.Name', 'Actual.FP')])
lineups$total <- 0

for (index in 1:nrow(lineups)){
  # index <- 143
      row <- t(lineups[index,])
    # colnames(row) <- 'Player'
      # row <- merge(row, total_results, by = 'Player')
      # lineups$total[index] <- sum(row$Actual.Score)
        colnames(row) <- 'Player.Name'
        row <- merge(row, total_results, by = 'Player.Name')
        lineups$total[index] <- sum(row$Actual.FP)
      }

plot(lineups$total, main = paste0("Week ", week.num, ", Overlap ", k), xlab = "Lineup Index", ylab = "Lineup FPts")


######## CALCULATE PLACE AND PAYOUT FOR EACH LINEUP ########
# print(paste0("Number of NAs: ", sum(is.na(lineups$total))))
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

######## ADD TO PNL MATRIX ########
if (contest.entry.fee=='$3' & week.num == 10) {
  temp.contest.entry.fee <- '$4'
} else if (contest.entry.fee=='$20' & week.num > 9) {
    temp.contest.entry.fee <- '$27'
  } else {
      temp.contest.entry.fee <- contest.entry.fee
    }

temp.pnl <- sum(lineups$payout) - as.numeric(substring(temp.contest.entry.fee, 2)) * nrow(lineups)
if (week.lo != week.hi & overlap.lo == overlap.hi & length(exposure.range) == 1) {
    pnlMatrix[week.num-week.lo+1, 'PnL'] <- temp.pnl
  } else if (length(exposure.range) == 1) {
      pnlMatrix[k, 'PnL'] <- temp.pnl
    } else {
        pnlMatrix[pnlMatrix[,'Exposure']==exposure, 'PnL'] <- temp.pnl
      }

######## FUNCTION FOR CALCULATING TOTAL PNL OF LINEUPS ########
# won't be exact b/c not accounting for ties
  calculatePnL <- function(numberEntries, lineups, temp.contest.entry.fee) {
      lineups <- lineups[1:numberEntries,]  
      return(sum(lineups$payout) - as.numeric(substring(temp.contest.entry.fee, 2)) * nrow(lineups))
    }
  
  ######## PNL VS NUMBER OF LINEUPS ########
  pnls <- rep(0,nrow(lineups))
  for (i in 1:length(pnls)) {
      pnls[i] <- calculatePnL(nrow(lineups)-i+1, lineups, temp.contest.entry.fee)
    }

    numLineups <- seq(from = nrow(lineups), to = nrow(lineups)-length(pnls)+1)
    plot(numLineups, pnls, xlab="Number of Lineups", ylab="PnL", type = "l")
    abline(h=0, col = "red")
  }
}

# print
if (week.lo != week.hi & overlap.lo == overlap.hi & length(exposure.range) == 1) {
print(paste0('Formulation: ', formulation, '; Overlap: ', overlap.lo, '; Exposure: ', exposure.range[1], '; Contest: ', contest.name))
} else {
print(paste0('Week: ', week.num, '; Overlap: ', overlap.lo, '; Formulation: ', formulation))
}
print(pnlMatrix)
}
# if (length(exposure.range) == 1) {
#   saveRDS(pnlMatrix, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", week.num, predictions.source, "_formulation", formulation, "_exposure_", 1, ".rds"))
# } else {
#   saveRDS(pnlMatrix, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/formulation_pnl/pnlMatrix_week", week.num, predictions.source, "_formulation", formulation, "_overlap_", overlap.lo, ".rds"))
# }
}

# write lineups to file
# write.csv(lineups, file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", k, "_exposure_", exposure, num.lineups, "_results.csv"), row.names = F)

# plotting across weeks
par(mfrow=c(1,1))
pnlMatrix <- as.data.frame(pnlMatrix)
plot(pnlMatrix$Week, pnlMatrix$PnL, ylim = c(-2000,2000), type = 'b', xlab = "Week", ylab = "PnL", main = paste0("Form4-Ovrlap4-Exp0.4 (Contest: ", contest.name, ")"))
abline(0,0)
