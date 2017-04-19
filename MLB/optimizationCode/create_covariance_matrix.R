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

contest.date <- "2017-04-02"

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
temp_names <- paste0(list_all_players$Name, list_all_players$teamAbbrev)
names_unique_players <- unique(temp_names)

# initialize historical fpts matrix
hist_fpts_mat <- as.data.frame(matrix(data = NA, nrow = length(names_unique_players), ncol = length(dates)))
colnames(hist_fpts_mat) <- dates
rownames(hist_fpts_mat) <- names_unique_players

# fill historical fpts matrix
for (i in 1:nrow(hist_fpts_mat)) {
  for (j in 1:ncol(hist_fpts_mat)) {
    temp <- list_all_players$Actual_fpts[paste0(list_all_players$Name, list_all_players$teamAbbrev)==rownames(hist_fpts_mat)[i] & list_all_players$Date==colnames(hist_fpts_mat)[j]]
    if (length(temp)==0) {
      hist_fpts_mat[i,j] <- NA
    } else {
      hist_fpts_mat[i,j] <- temp
    }
  }
}
# temp <- hist_fpts_mat
# remove rows with NA count > round(length(dates)/2)
inds.remove <- NULL
for (i in 1:nrow(hist_fpts_mat)) {
  if (sum(is.na(hist_fpts_mat[i,])) > round(length(dates)/2)) {
    inds.remove <- c(inds.remove, i)
  }
}
hist_fpts_mat <- hist_fpts_mat[-inds.remove,]


# construct covariance matrix
cov_mat <- cov(t(hist_fpts_mat), use = "pairwise.complete.obs") # complete.obs
View(cov_mat)

# indicies of max covariance
which(cov_mat == max(cov_mat, na.rm = T), arr.ind = TRUE)

# find the top n largest values (decreasing order)
n <- 25
x <- which(cov_mat >= sort(cov_mat, decreasing = T)[n], arr.ind = T)
x.order <- order(cov_mat[x], decreasing = T) # decreasing order
x[x.order,]


