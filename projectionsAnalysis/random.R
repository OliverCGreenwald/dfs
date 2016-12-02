#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #######
# In this file we look at how the fpts distribution of contest entries has changed week to week.


####### IMPORT LIBRARIES #########
library('stringr')


####### $20 CONTESTS #######
# weeks we have
wks.20 <- c(2:9)
wks.27 <- c(11:12)

# load $20 contest results and payout structure
for (i in wks.20) {
  name <- paste0("contest_20_results_wk", i)
  assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$20_contest_full_results_week", i, ".csv"), stringsAsFactors = F))

  name <- paste0("payout_20_structure_wk", i)
  assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$20_payout_structure_week", i, ".csv"), stringsAsFactors = F))
}

# load $27 contest results (as $20 results for naming convention) and payout structure
for (i in wks.27) {
  name <- paste0("contest_20_results_wk", i)
  assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$27_contest_full_results_week", i, ".csv"), stringsAsFactors = F))

  name <- paste0("payout_20_structure_wk", i)
  assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/weekly_payout_structure/$27_payout_structure_week", i, ".csv"), stringsAsFactors = F))
}

# Plot histogram of placing lineups fpts
par(mfrow=c(3,4)) # hard coded for now
for (i in c(wks.20, wks.27)) {
  # load dfs
  temp.results <- eval(parse(text=paste0("contest_20_results_wk", i)))
  temp.payout <- eval(parse(text=paste0("payout_20_structure_wk", i)))
  
  # subset to placing lineups only
  place.last <- temp.payout$Place_hi[nrow(temp.payout)]
  temp.results <- temp.results[1:place.last,]
  
  # plot histogram
  hist(temp.results$Points, main = paste0("Wk ", i), xlab = paste0("Fpts (min: ", min(temp.results$Points), ", max: ", max(temp.results$Points),")"))
}

# Plot histogram of generated lineups fpts
par(mfrow=c(3,4)) # hard coded for now
for (w in c(wks.20, wks.27)) {
  # load dfs and clean
  lineups <- read.csv(paste0("resultsAnalysis/data_warehouse/testing_lineups/week", w, "_dfn_formulation4_overlap_4_exposure_0.4.csv"))
  
  # player.performance <- read.csv(file = paste0("resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week", w, ".csv"), stringsAsFactors = F)
  # player.performance$Actual.Score[is.na(player.performance$Actual.Score)] <- 0
  # player.performance$Player <- sub(' Sr.','', player.performance$Player)
  # player.performance$Player <- sub(' Jr.','', player.performance$Player)
  
  player.performance <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', w, ".csv"), stringsAsFactors = F)
  player.performance.def <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week', w, ".csv"), stringsAsFactors = F)
  
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
  
  # compute fpts
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
    row <- t(lineups[index,])
    # colnames(row) <- 'Player'
    # row <- merge(row, total_results, by = 'Player')
    # lineups$total[index] <- sum(row$Actual.Score)
    colnames(row) <- 'Player.Name'
    row <- merge(row, total_results, by = 'Player.Name')
    lineups$total[index] <- sum(row$Actual.FP)
  }
  
  # plot histogram
  hist(lineups$total, main = paste0("Wk ", w), xlab = paste0("Fpts (min: ", min(lineups$total), ", max: ", max(lineups$total),")"), xlim = c(50, 270))
}


####### PLOT BEST POSSIBLE LINEUPS GIVEN ACTUAL FPTS #######
par(mfrow=c(3,4)) # hard coded for now
for (i in 2:12) { # hard coded for now
  lineups <- read.csv(file = paste0("resultsAnalysis/data_warehouse/testing_lineups/week", i, "_actual_formulation0_overlap_8_exposure_1_numlineups_1000_results.csv"), stringsAsFactors = F)
  plot(sort(lineups$total, decreasing = T), main = paste0("Wk ", i), xlab = paste0("min: ", min(lineups$total), ", max: ", max(lineups$total)), ylab = paste0("Fpts (SD: ", format(round(sd(lineups$total), 2),nsmall = 2), ")"))
}

