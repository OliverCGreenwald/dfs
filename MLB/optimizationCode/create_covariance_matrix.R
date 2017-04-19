if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Constructs the covariance matrix for MVO.

contest.date <- "2017-04-02"

####### Get List of All Players that have Played a Game Up to Today-1 #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
# dates <- seq(from = as.Date("2017-04-04"), to = as.Date("2017-04-05"), by = "day")
dates <- seq(from = as.Date("2017-04-02"), to = Sys.Date() - 1, by = "day")
list_all_players <- NULL
for (d in 1:length(dates)) {
  # subset contest_info by date
  temp_contest_info <- contest_info[contest_info$Contest_Date==dates[d],]
  
  # get list of players for the day
  list_players_day <- NULL
  for (i in 1:nrow(temp_contest_info)) {
    temp_hitters <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
    temp_pitchers <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/pitchers.csv"), stringsAsFactors = F, header = T)
    temp_players_day <- rbind(temp_hitters, temp_pitchers)
    temp_players_day <- temp_players_day[, !(colnames(temp_players_day) %in% c("Projection","Projection_dfn","Projection_rotogrinders","Projection_baseballmonster","Projection_rotowire"))]
    temp_players_day$Date <- dates[d]
    list_players_day <- rbind(list_players_day, temp_players_day)
  }
  
  # append to the unique rows (players) to the running list
  list_all_players <- rbind(list_all_players, unique(list_players_day))
  remove(list_players_day) # remove
}



