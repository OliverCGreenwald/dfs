if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}



### Save DFS Directory Path

original_wd <- getwd()


source('MLB/resultsAnalysis/helperFunctions/compute_lineup_fpts.R')

### Read in Contest File
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)
i <- 141


base_contest_path = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])))
setwd(base_contest_path)

contest_standings <- read.csv('contest-standings.csv', stringsAsFactors = F)
payout_structure <- read.csv('payout_structure.csv', stringsAsFactors = F)
player_performance <- read.csv('../player_results.csv', stringsAsFactors = F)
setwd('lineups')

temp = list.files(pattern="*.csv")
lineups = lapply(temp, read.csv, stringsAsFactors = F)

setwd(original_wd)

lineup <- lineups[[22]]

output <- compute_lineup_fpts(player_performance, payout_structure, lineup, contest_info$Entry_Fee[i])
View(output[[1]])


for (counter in 1:81) {
  lineup <- lineups[[counter]]
  
  output <- compute_lineup_fpts(player_performance, payout_structure, lineup, contest_info$Entry_Fee[i])
  PnL <- output[[2]]
  print(paste0("PnL: ", PnL, "| Counter = ", counter, " | Lineup: ", temp[counter]))
}
