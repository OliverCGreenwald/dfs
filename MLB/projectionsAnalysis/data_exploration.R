if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Analyze projections. Using players from the $5 entry Knuckleball contest each day.

# TODO:
# - iterate over all contests


####### Import Functions #######
source("MLB/functions_global/aggregate_projections.R")


####### Create Aggregated Projected and Actual Fpts Dataframe For Each Day #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

# find one 150 entry contest per day
contest_info_filtered <- NULL
dates <- seq(from = as.Date("2017-04-03"), to = Sys.Date() - 1, by = "day")
for (i in 1:length(dates)) {
  # subset by the date and max_entry=150
  contest_info_temp <- contest_info[contest_info$Contest_Date==dates[i] & contest_info$Max_Entry==150, ]
  
  # use knuckleball contest if exists, else use first 150 entry contest in df
  temp.found <- F
  for (j in 1:nrow(contest_info_temp)) {
    if (grepl("KNUCKLEBALL", contest_info_temp$Contest_Name[j])==T) {
      contest_info_filtered <- rbind(contest_info_filtered, contest_info_temp[j,])
      temp.found <- T
    }
  }
  if (temp.found==F) {
    contest_info_filtered <- rbind(contest_info_filtered, contest_info_temp[1,])
  }
}

# aggregate projectioned and actual fpts for each contest
for (i in 1:nrow(contest_info_filtered)) {
  # projections.dat <- aggregate_projections(contest_info_filtered$Contest_Date[i], "$5.00entry_MLB$25KKnuckleball")
  # hitters.dat <- projections.dat[[1]]
  # pitchers.dat <- projections.dat[[2]]
}





