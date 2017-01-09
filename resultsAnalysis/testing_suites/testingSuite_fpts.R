#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we compute the fantasy points (no PnL) of lineups for testing purposes.
# Note: change "SET FILE" to whatever path generated lineups are stored at


####### IMPORT LIBRARIES #########
library('stringr')


####### SET PARAMETER VALUES #########
week.lo <- 2
week.hi <- 16

contest.entry.fee <- "$4"
thu_mon.bool <- F # True if using thursday-monday games, False if using only Sunday games

predictions.source <- "_dfn" # "_dfn" or "" or "_dfn_perturbed" or "_actual"
source.actual.fpts <- 'DFN' # 'FC' or 'DFN'

formulation <- 14

overlap <- 4

exposure <- 0.4

exposure.def <- 0.25
exposure.wr <- 0.25
exposure.rb <- 0.75
exposure.te <- 0.75
exposure.qb <- 0.5
exposure.valuewr <- "_valuewrexp_0.15"

freqInd <- "" # _FreqInd or ""

num.lineups <- "" # "" or "_numlineups_1000"

missing.data.1M.contest.wk <- c(10) # enter weeks that we don't have complete data for in the $1M to 1st contest
missing.data.50k.contest.wk <- c() # enter weeks that we don't have complete data for in the $50k to 1st contest


# Init
result.mat <- matrix(data = NA, nrow = week.hi-week.lo+1, ncol = 10, dimnames = list(NULL, c("Week","Mean","Max","Min","Cashing","160-170","170-180","180-190","190-200",">200")))

# if (thu_mon.bool == T) {
#   num.cashing.full.slate.mat <- matrix(data = NA, nrow = week.hi-week.lo+1, ncol = 2, dimnames = list(NULL, c("Week","Num.Cashing")))
#   num.cashing.full.slate.mat[,'Week'] <- week.lo:week.hi
#   cashing.full.slate.dat <- read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/includes_thu-mon/full_slate_cashing.csv"), stringsAsFactors = F) 
# } else {
#   num.cashing.50K.contest.mat <- matrix(data = NA, nrow = week.hi-week.lo+1, ncol = 2, dimnames = list(NULL, c("Week","Num.Cashing")))
#   num.cashing.50K.contest.mat[,'Week'] <- week.lo:week.hi
#   cashing.50K.contest.dat <- read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/contest_50K_cashing.csv"), stringsAsFactors = F)
# }

# num.above.190 <- matrix(data = NA, nrow = week.hi-week.lo+1, ncol = 2, dimnames = list(NULL, c("Week","Num.Above.190")))
# num.above.190[,'Week'] <- week.lo:week.hi

# Loop through weeks
par(mfrow=c(1,2))
for (week.num in week.lo:week.hi) {
  if ((week.num %in% missing.data.1M.contest.wk & contest.entry.fee == '$20') | (week.num %in% missing.data.50k.contest.wk & contest.entry.fee == '$3')) {
    # do nothing
  } else {
    ####### SET FILE #########
    # for baseline
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_exposure_", exposure, num.lineups, ".csv") # form 4
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv") # form 15
    
    # for model1 (thu-mon)
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_exposure_", exposure, num.lineups, ".csv") # form 4
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv") # form 15
    
    # for model1 (sun only)
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_exposure_", exposure, num.lineups, ".csv") # form 4
    file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, num.lineups, ".csv") # form 14
    # file.name <- paste0("resultsAnalysis/data_warehouse/testing_lineups/model1/week", week.num, predictions.source, freqInd, "_formulation", formulation, "_overlap_", overlap, "_defexp_", exposure.def, "_wrexp_", exposure.wr, "_rbexp_", exposure.rb, "_teexp_", exposure.te,"_qbexp_", exposure.qb, exposure.valuewr, num.lineups, ".csv") # form 15
    
    ####### IMPORT AND CLEAN DK HISTORICAL FPTS DATA #########
    # Use Fantasy Cruncher for actual fpts data
    if (source.actual.fpts == 'FC') {
      player.performance <- read.csv(file = paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", week.num, ".csv"), stringsAsFactors = F)
      player.performance$Player <- sub(' Sr.','', player.performance$Player)
      player.performance$Player <- sub(' Jr.','', player.performance$Player)
      player.performance$Player.Name <- player.performance$Player # to keep column name consistent
      player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0
      player.performance$Actual.FP <- player.performance$Actual.Score # to keep column name consistent
    }
    # Use DFN for actual fpts data
    else if (source.actual.fpts == 'DFN') {
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
    }
    
    ####### LOAD LINEUPS FOR THIS SET OF PARAMETERS #########
    lineups <- read.csv(file = file.name, stringsAsFactors = F)
    
    ######## CALCULATE FPTS FOR EACH LINEUP ########
    for (i in 1:ncol(lineups)) {
      lineups[,i] <- substr(lineups[,i], 1, regexpr('\\(', lineups[,i]) - 2)
      lineups[,i] <- sub(' Sr.','', lineups[,i])
      lineups[,i] <- sub(' Jr.','', lineups[,i]) 
    }
    lineups[,ncol(lineups)] <- substr(lineups[,ncol(lineups)], 1, nchar(lineups[,ncol(lineups)])-1)
    
    total_results <- player.performance[,c('Player.Name', 'Actual.FP','Salary')]
    if (source.actual.fpts == 'DFN') {
      total_results <- rbind(total_results, player.performance.def[,c('Player.Name', 'Actual.FP','Salary')])
    }
    lineups$total <- 0
    
    for (index in 1:nrow(lineups)){
      row <- t(lineups[index,])
      colnames(row) <- 'Player.Name'
      row <- merge(row, total_results, by = 'Player.Name')
      lineups$total[index] <- sum(row$Actual.FP)
    }
    
    plot(lineups$total, main = paste0("Week ", week.num, ", Overlap ", overlap), xlab = "Lineup Index", ylab = "Lineup FPts")
    
    hist(lineups$total)
    
    # if (thu_mon.bool == T) {
    #   num.cashing.full.slate.mat[week.num-week.lo+1,'Num.Cashing'] <- sum(lineups$total > cashing.full.slate.dat[week.num,'Min'])
    # } else {
    #   num.cashing.50K.contest.mat[week.num-week.lo+1,'Num.Cashing'] <- sum(lineups$total > cashing.50K.contest.dat[week.num,'Min'])
    # }
    # num.above.190[week.num-week.lo+1,'Num.Above.190'] <- sum(lineups$total > 190)
  }
  
  # compute cashing threshold
  if (thu_mon.bool==T) {
    cashing.dat <- read.csv(file = "resultsAnalysis/data_warehouse/weekly_payout_structure/includes_thu-mon/full_slate_cashing.csv", stringsAsFactors = F) 
  } else {
    cashing.dat <- read.csv(file = "resultsAnalysis/data_warehouse/weekly_payout_structure/contest_1M_cashing.csv", stringsAsFactors = F) 
  }
  cashing.threshold <- cashing.dat$Min[cashing.dat$Week==(week.num-week.lo+1)]
  
  # fill in results matrix
  result.mat[week.num-week.lo+1,1] <- week.num
  result.mat[week.num-week.lo+1,2] <- mean(lineups$total)
  result.mat[week.num-week.lo+1,3] <- max(lineups$total)
  result.mat[week.num-week.lo+1,4] <- min(lineups$total)
  if (is.na(cashing.threshold)) {
    result.mat[week.num-week.lo+1,5] <- sum(lineups$total > 150) 
  } else {
    result.mat[week.num-week.lo+1,5] <- sum(lineups$total > cashing.threshold) 
  }
  result.mat[week.num-week.lo+1,6] <- sum(lineups$total > 160 & lineups$total <= 170)
  result.mat[week.num-week.lo+1,7] <- sum(lineups$total > 170 & lineups$total <= 180)
  result.mat[week.num-week.lo+1,8] <- sum(lineups$total > 180 & lineups$total <= 190)
  result.mat[week.num-week.lo+1,9] <- sum(lineups$total > 190 & lineups$total <= 200)
  result.mat[week.num-week.lo+1,10] <- sum(lineups$total > 200)
}

View(result.mat)

# mean(lineups$total)
# max(lineups$total)
# min(lineups$total)
# sum(lineups$total > 150)
# sum(lineups$total >= 160 & lineups$total <= 170)
# sum(lineups$total >= 180 & lineups$total <= 190)
# sum(lineups$total >= 190 & lineups$total <= 200)
# sum(lineups$total > 200)



# par(mfrow=c(1,1))
# # number of lineups that cash
# if (thu_mon.bool == T) {
#   num.cashing.full.slate.mat
#   plot(week.lo:week.hi, num.cashing.full.slate.mat[,'Num.Cashing'], type = 'b', xlab = 'Week', ylab = 'Num Cashing Lineups', main = 'Number of Cashing Lineups (150K Contest)')
# } else {
#   num.cashing.50K.contest.mat
#   plot(week.lo:week.hi, num.cashing.50K.contest.mat[,'Num.Cashing'], type = 'b', xlab = 'Week', ylab = 'Num Cashing Lineups', main = 'Number of Cashing Lineups (50K Contest)')
# }
# 
# # number of lineups above 190 fpts
# plot(week.lo:week.hi, num.above.190[,'Num.Above.190'], type = 'b', xlab = 'Week', ylab = 'Num Cashing Lineups', main = 'Number Lineups > 190 Fpts (150K Contest)')











# #############################
# for (i in 2:15) {
#   if (i == 10) {
#     assign(paste0("contest_50K_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$4_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
#     assign(paste0("payout_50K_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$4_payout_structure_week", i, ".csv"), stringsAsFactors = F))
#   } else {
#     assign(paste0("contest_50K_results_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$3_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
#     assign(paste0("payout_50K_structure_wk", i), read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$3_payout_structure_week", i, ".csv"), stringsAsFactors = F))
#   }
#   print(i)
# }
# 
# contest_50K_cashing <- matrix(data = NA, nrow = 0, ncol = 3, dimnames = list(NULL, c("Week","Min","Max")))
# contest_50K_cashing  <- rbind(contest_50K_cashing, c(1, '', ''))
# for (i in 2:15) {
#   temp.results <- eval(parse(text=paste0("contest_50K_results_wk", i)))
#   temp.payout <- eval(parse(text=paste0("payout_50K_structure_wk", i)))
#   place.last <- temp.payout$Place_hi[nrow(temp.payout)]
#   temp.max <- temp.results$Points[1]
#   temp.min <- temp.results$Points[place.last]
#   contest_50K_cashing  <- rbind(contest_50K_cashing, c(i, temp.min, temp.max))
# }
# write.csv(contest_50K_cashing, file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/contest_50K_cashing.csv"), row.names = F)
# 



# load("/Users/Alan/Downloads/player_exposure_pnl_full_slate.RData")



