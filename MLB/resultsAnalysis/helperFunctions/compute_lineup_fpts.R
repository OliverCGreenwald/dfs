####### DESCRIPTION #########
# This file loads the full contest results into R
# Given: Player_Performance_DF, Lineups
# Returns the Lineups DF with an added column of the total fpts per lineup
# If the file does not exist, returns 0


compute_lineup_fpts <- function(player_performance_df, lineups) {
  ######## CALCULATE FPTS FOR EACH LINEUP ########
  
  total_results <- player_performance_df[,c('Player', 'Actual.Score', 'Salary')]
  lineups$total <- 0
  
  for (index in 1:nrow(lineups)){
    row <- t(lineups[index,])
    colnames(row) <- 'Player'
    row <- merge(row, total_results, by = 'Player')
    lineups$total[index] <- sum(row$Actual.Score)
  }
  
  return(lineups)
}

