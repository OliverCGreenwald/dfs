


singleContest_manyLineups_PnL_comparison <- function(contest_info_row_number, regex_input = '*.csv') {
  
  
  original_wd <- getwd()
  
  #Load Helper Function
  source('MLB/resultsAnalysis/helperFunctions/compute_lineup_fpts.R')
  
  #Read in Contest Info
  contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
  contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)
  
  # Go to Desired Contest Directory
  base_contest_path = paste0("MLB/data_warehouse/", contest_info$Contest_Date[contest_info_row_number], "/" , 
                             paste0(contest_info$Entry_Fee[contest_info_row_number],"entry_",
                                    gsub(" ", "", contest_info$Contest_Name[contest_info_row_number])))
  setwd(base_contest_path)
  
  #Load in needed files
  contest_standings <- read.csv('contest-standings.csv', stringsAsFactors = F)
  payout_structure <- read.csv('payout_structure.csv', stringsAsFactors = F)
  player_performance <- read.csv('../player_results.csv', stringsAsFactors = F)
  
  # Go to Lineups Folder
  setwd('lineups')
  
  
  file_names = list.files(pattern = regex_input)
  lineups = lapply(file_names, read.csv, stringsAsFactors = F)
  
  setwd(original_wd)
  
  results <- as.data.frame(matrix(0,length(file_names),3))
  names(results) <- c('Name', 'PnL', 'Lineups')
  results$Name <- file_names
  
  for (counter in 1:length(file_names)) {
    if (counter %% 10 == 0) {
      print(paste0("Working on Lineup ", counter, " of ", length(file_names)))
    }
    
    lineup <- lineups[[counter]]
    
    output <- compute_lineup_fpts(player_performance, payout_structure, lineup, contest_info$Entry_Fee[contest_info_row_number], 
                                  contest_standings)
    PnL <- output[[2]]
    results$PnL[counter] <- PnL
    
    results$Lineups[counter] <- list(output[[1]])
    #print(paste0("PnL: ", PnL, "| Counter = ", counter, " | Lineup: ", temp[counter]))
  }
  
  return(results)
  
}