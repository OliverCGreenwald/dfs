if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create julia inputs (hitters and pitchers) for each date and contest.
#
# TODO:
# - iterate through the date folders and contests


####### Import Functions #######
source("MLB/functions_global/aggregate_projections.R")


####### Create Julia Inputs #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

# subset by yesterday's date
date <- Sys.Date()-1
contest_info <- contest_info[contest_info$Contest_Date==as.Date(date),]

aggregated_data_hitters <- list()
aggregated_data_pitchers <- list()
for (i in 1:nrow(contest_info)) {
  projections.dat <- aggregate_projections(contest.date = contest_info$Contest_Date[i], contest.name = paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])))
  aggregated_data_hitters[[i]] <- projections.dat[[1]]
  aggregated_data_pitchers[[i]] <- projections.dat[[2]]
  
  ####### Write to CSV file #######
  write.csv(aggregated_data_hitters[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), row.names = F)
  write.csv(aggregated_data_pitchers[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/pitchers.csv"), row.names = F)
}


