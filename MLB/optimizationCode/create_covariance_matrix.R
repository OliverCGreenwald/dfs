if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Constructs the covariance matrix for MVO.
#
# TODO:
# - determine if pairwise.complete.obs is appropriate for our problem. considered dangerous


####### Import Libraries #######
library(stringr)


####### Aggregate All Player Data for Each Day #######
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
  remove(list_players_day) # remove temp
}

####### Construct Matrix of Historical Fpts and Compute Covariance Matrix #######
# list of unique player names and their position
temp_names <- paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)
names_unique_players <- unique(temp_names)

# initialize historical fpts matrix
hist_fpts_mat <- as.data.frame(matrix(data = NA, nrow = length(names_unique_players), ncol = length(dates)))
colnames(hist_fpts_mat) <- dates
rownames(hist_fpts_mat) <- names_unique_players

# fill historical fpts matrix
for (i in 1:nrow(hist_fpts_mat)) {
  for (j in 1:ncol(hist_fpts_mat)) {
    temp <- list_all_players$Actual_fpts[paste0(list_all_players$Name, "_", list_all_players$teamAbbrev)==rownames(hist_fpts_mat)[i] & list_all_players$Date==colnames(hist_fpts_mat)[j]]
    if (length(temp)==0) {
      hist_fpts_mat[i,j] <- NA
    } else {
      hist_fpts_mat[i,j] <- temp
    }
  }
}

# remove rows with NA count > round(length(dates)/2)
inds.remove <- NULL
for (i in 1:nrow(hist_fpts_mat)) {
  if (sum(is.na(hist_fpts_mat[i,])) > round(length(dates)/2)) {
    inds.remove <- c(inds.remove, i)
  }
}
hist_fpts_mat <- hist_fpts_mat[-inds.remove,]

# Set elements to NA where players are not on same team and do not satisfy (player, opposing pitcher)
# temp_teams <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2]
# add opponent column to list_all_players
list_all_players$Team1 <- str_split_fixed(str_split_fixed(list_all_players$GameInfo, " ", 2)[,1], "@", 2)[,1]
list_all_players$Team2 <- str_split_fixed(str_split_fixed(list_all_players$GameInfo, " ", 2)[,1], "@", 2)[,2]
for (i in 1:nrow(list_all_players)) {
  if (list_all_players$teamAbbrev[i]==list_all_players$Team1[i]) {
    list_all_players$Opponent[i] <- list_all_players$Team2[i]
  } else {
    list_all_players$Opponent[i] <- list_all_players$Team1[i]
  }
}
list_all_players$Team1 <- NULL
list_all_players$Team2 <- NULL

# add team column to hist_fpts_mat
# hist_fpts_mat$teamAbbrev <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2]

# construct full covariance matrix
cov_mat_temp <- cov(t(hist_fpts_mat), use = "pairwise.complete.obs") # complete.obs
# View(cov_mat_temp)

# construct covariance matrix, only computed when players are on same team or playing an opposing pitcher
# initialize covariance matrix
cov_mat <- cov_mat_temp
cov_mat[,] <- NA

# initialize matrix of counts of non-NA game used for covariance calculation
cov_mat_counts <- cov_mat_temp
cov_mat_counts[,] <- NA

# transpose for covariance function
hist_fpts_mat_temp <- as.data.frame(t(hist_fpts_mat))

# fill elements above diagonal in covariance matrix
for (i in 1:nrow(cov_mat)) { # nrow(cov_mat)
  for (j in i:ncol(cov_mat)) { # ncol(cov_mat)
    # rownames(temp)
    # colnames(cov_mat)
    # str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2]
    # hist_fpts_mat_temp[,i]
    # hist_fpts_mat_temp[,j]
    # colnames(cov_mat)[i]
    # str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][i]
    # list_all_players$teamAbbrev[list_all_players$Date==dates[17]]
    temp <- hist_fpts_mat_temp[,i]
    for (k in 1:nrow(hist_fpts_mat_temp)) {
      # must be on same team
      if (str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][i] != str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][j]) {
        # hist_fpts_mat_temp[k,i] <- NA
        # hist_fpts_mat_temp[k,j] <- NA
        temp[k] <- NA
      }
      
      # or opposing pitcher
      # TODO
    }
    cov_mat[i,j] <- cov(temp, hist_fpts_mat_temp[,j], use = "pairwise.complete.obs")
    cov_mat_counts[i,j] <- sum(which(!is.na(temp)) %in% which(!is.na(hist_fpts_mat_temp[,j])))
    print(paste0("Entries [", i, ", ", j, ") Completed"))
  }
}





splitPlayers <- function(x) {
  return(str_split_fixed(x, "QB | RB | WR | TE | FLEX | DST ", 10)[2:10])
}
temp.players <- data.frame(matrix(unlist(lapply(X = temp.results$Lineup[1:nrow(temp.results)], FUN = splitPlayers)), nrow=nrow(temp.results), byrow=T))






# remove the diagonal (variances)
cov_mat_unique <- cov_mat
temp_inds <- 1:nrow(cov_mat_unique)
for (i in 1:length(temp_inds)) {
  cov_mat_unique[temp_inds[i], temp_inds[i]] <- NA 
}
    
# find the top n largest covariances (decreasing order)
n <- 25
x <- which(cov_mat_unique >= sort(cov_mat_unique, decreasing = T)[n], arr.ind = T)
x.order <- order(cov_mat_unique[x], decreasing = T) # decreasing order
x[x.order,]

for (i in 1:n) {
  player_a_ind <- x[x.order,][i,][1]
  player_b_ind <- x[x.order,][i,][2]
  player_a <- rownames(cov_mat_unique)[player_a_ind]
  player_b <- rownames(cov_mat_unique)[player_b_ind]
  print(paste0(player_a, ", ", player_b, ", Covariance: ", cov_mat_unique[player_a_ind, player_b_ind], ", Num_Games: ", cov_mat_counts[player_a_ind, player_b_ind]))
}


