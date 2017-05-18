if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# date.start <- "2017-04-02"
# date.end <- "2017-05-14"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/2017-05-14/$50.00entry_MLB$300KMother'sDaySpecial/hitters.csv"), stringsAsFactors = F, header = T)
# min_games_pctg <- NULL
# 
# date.start <- "2017-04-02"
# date.end <- "2017-05-13"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/2017-05-13/$40.00entry_MLB$250KSaturdaySlugfest/hitters.csv"), stringsAsFactors = F, header = T)
# min_games_pctg <- NULL
# 
# date.start <- "2017-04-02"
# date.end <- "2017-05-08"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/2017-05-08/$8.00entry_MLB$175KCrazy8's/hitters.csv"), stringsAsFactors = F, header = T)
# min_games_pctg <- NULL
# 
# date.start <- "2017-04-02"
# date.end <- "2017-05-01"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/2017-05-01/$33.00entry_MLB$325KFastball[$50Kto1st]/hitters.csv"), stringsAsFactors = F, header = T)
# min_games_pctg <- NULL

####### Import Libraries #######
require(stringr)


###### Import Functions #######
source("MLB/functions_global/createHistoricalFptsMatrix.R")


#####
baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)
date.start <- "2017-04-02"
k <- 25
contest_date <- baseline_contests$Date[k]
julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_date, "/", baseline_contests$Contest_names[k], "/hitters.csv"), stringsAsFactors = F, header = T)
min_games_pctg <- NULL

date.end <- as.Date(contest_date) - 1 # offset by 1 to avoid lookahead bias
#####



# date sequence
dates <- seq(from = as.Date(date.start), to = as.Date(date.end), by = "day")

# aggregate all past players if NULL, otherwise only players in contest
####### Aggregate All Player Data for Each Day #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
list_all_players <- NULL
for (d in 1:length(dates)) {
  # print(dates[d])
  
  # subset contest_info by date
  temp_contest_info <- contest_info[contest_info$Contest_Date==dates[d],]
  
  # get list of players for the day
  list_players_day <- NULL
  for (i in 1:nrow(temp_contest_info)) {
    # load
    temp_hitters <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
    temp_pitchers <- read.csv(file = paste0("MLB/data_warehouse/", dates[d],"/", paste0(temp_contest_info$Entry_Fee[i],"entry_",gsub(" ", "", temp_contest_info$Contest_Name[i])), "/pitchers.csv"), stringsAsFactors = F, header = T)
    
    # subset columns and append
    temp_hitters <- temp_hitters[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts")]
    temp_pitchers <- temp_pitchers[, c("Position", "Name", "Salary", "GameInfo", "teamAbbrev", "Actual_fpts")]
    if (is.null(julia_hitter_df)) {
      temp_players_day <- rbind(temp_hitters, temp_pitchers) 
    } else {
      temp_players_day <- temp_hitters
    }
    
    # add date
    temp_players_day$Date <- dates[d]
    
    # remove Position, Salary, GameInfo (currently just uses first game in a double header. TODO: fix this)
    temp_players_day$Position <- NULL
    temp_players_day$Salary <- NULL
    temp_players_day$GameInfo <- NULL
    
    # append
    list_players_day <- rbind(list_players_day, temp_players_day)
  }
  
  # append to the unique rows (players) to the running list
  list_all_players <- rbind(list_all_players, unique(list_players_day))
  remove(list_players_day) # remove temp
}


####### Construct Matrix of Historical Fpts #######
# print("Constructing Historical Fpts Matrix...")

# list of unique player names and their position
temp_names <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
if (is.null(julia_hitter_df)) {
  names_unique_players <- unique(temp_names)
} else {
  # unique(temp_names)
  names_unique_players <- paste0(julia_hitter_df$Name, "_", julia_hitter_df$teamAbbrev) # must match order of julia_hitter_df (this line should be equivlaent to running unique(temp_names) but different order)
}

# call createHistoricalFptsMatrix function
hist_fpts_mat <- createHistoricalFptsMatrix(name_team_vec = names_unique_players, list_all_players = list_all_players, dates_vec = dates, min_games_pctg = min_games_pctg)














# Question 3

# copy
gdp_copy <- hist_fpts_mat

# split by team
temp_team_inds <- str_split_fixed(rownames(gdp_copy), "_", 2)[,2]
temp_all_teams <- unique(temp_team_inds)

for (team in temp_all_teams) {
  gdp_copy <- hist_fpts_mat
  
  temp_name <- team
  gdp_copy_temp <- gdp_copy[which(temp_team_inds == temp_name),]
  gdp_copy <- gdp_copy_temp
  
  # Question 3.1
  # Delete rows of entirely NAs
  
  gdp <- NULL
  for (i in 1:nrow(gdp_copy)) {
    if (sum(is.na(gdp_copy[i,])) < ncol(gdp_copy)*0.75) {
      gdp <- rbind(gdp, gdp_copy[i,])
    }
  }
  
  
  # Replace NA's with mean of row
  for (i in 1:dim(gdp)[1]) {
    for (j in 1:dim(gdp)[2]) {
      if (is.na(gdp[i,j])) {
        gdp[i,j] <- rowMeans(gdp, na.rm = TRUE)[i]
      }
    }
  }
  
  
  # Question 3.2
  # 1
  d <- dim(gdp)[1]
  M <- matrix(0, d, d)
  
  # 2
  library(glmnet)
  mat <- matrix(0, d, d)
  for(i in 1:dim(gdp)[1]){
    mat[i,] <- append(coef(glmnet(x=t(gdp[-i,]), y=t(gdp[i,]), family='gaussian', lambda=1))[-1], 0, after = (i-1))
  }
  
  # 3
  for (i in 1:d) {
    for (j in 1:d) {
      if (mat[i,j]!=0 & mat[j,i]!=0) {
        M[i,j] <- 1
        M[j,i] <- 1
      }
    }
  }
  
  # 4
  library(igraph)
  ag=graph.adjacency(M, mode="undirected")
  V(ag)$colors=ifelse(degree(ag)<3, 'SkyBlue2', 'red')
  par(mai=c(0,0,0,0))
  plot.igraph(ag, vertex.color=V(ag)$colors, vertex.size=6, vertex.label.cex=0.8, layout=layout_nicely(ag))
  
  # 5
  rownames(gdp)[which(V(ag)$colors=="red")]

  
  print(paste0(team, ": ", length(which(V(ag)$colors=="red"))))
  print(rownames(gdp)[which(V(ag)$colors=="red")])
}

print(contest_date)