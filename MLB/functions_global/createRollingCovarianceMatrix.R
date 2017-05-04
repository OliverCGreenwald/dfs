if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for constructing the covariance matrix for MVO. Only computed for players on same team.
#
# Arguments:
# - julia_hitter_df: set to NULL if computing full covariance matrix, otherwise pass in hitter df for a contest
# - min_games_pctg: set between 0-1
#
# TODO:
# - determine if pairwise.complete.obs is appropriate for our problem. considered dangerous
# - add opposing pitcher into covariance matrix calculation
# - add opposing batters? (ballpark effect?)
#
# Notes:
# - we remove players that have played less than half the games
# - consider using Vectorize library in the future to vectorize for outer function rather than code up yourself


####### Function for Computing Covariance Matrix Given Start and End Date #######
createRollingCovarianceMatrix <- function(date.start, date.end, julia_hitter_df, min_games_pctg) {
  ####### Import Libraries #######
  library(stringr)
  
  
  ###### Import Functions #######
  source("MLB/functions_global/createHistoricalFptsMatrix.R")
  
  
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
  

  ####### Construct Covariance Matrix #######
  # print("Constructing covariance matrix...")

  # construct full covariance matrix (misleading bc includes independent events)
  cov_mat_temp <- cov(t(hist_fpts_mat), use = "pairwise.complete.obs") # complete.obs

  # construct covariance matrix, only computed when players are on same team [TODO: or playing an opposing pitcher]
  # initialize covariance matrix
  cov_mat <- cov_mat_temp
  cov_mat[,] <- NA

  # transpose for covariance function
  hist_fpts_mat_temp <- as.data.frame(t(hist_fpts_mat))

  # function to construct covariance matrix (fast way)
  fill_cov_mat <- function(x, y) {
    # vectorize indicies for outer function
    # each (x_i, y_i) pair is the indicies of an element in the matrix we desire to construct
    z <- as.data.frame(cbind(x, y))

    # append player_x and player_y historical fpts columns to z for fast vectorized computation
    temp_num_dates <- nrow(hist_fpts_mat_temp)
    z[,3:(3+temp_num_dates-1)] <- hist_fpts_mat[z$x,]
    z[,(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)] <- hist_fpts_mat[z$y,]

    # inds of upper half of covariance matrix
    temp_mat <- upper.tri(cov_mat, diag = TRUE)
    inds_upper_cov <- which(temp_mat, arr.ind = T)

    # function for computing covariance, excluding when players not on same team
    only_keep_teammates <- function(row) {
      if (row[1] %% row[2] == 0 & row[2] %% 10 == 0) {
        # print(paste0("Index ", row[1], ", ", row[2], " / (", nrow(cov_mat), ", ", ncol(cov_mat), ")", " Completed"))
        print(paste0("Column ", row[2], " / ", nrow(cov_mat), " Completed"))
      }
      team_player_a <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[1])] # team of player a
      team_player_b <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[2])] # team of player b
      if (team_player_a != team_player_b) {
        return(NA) # exclude from covariance calculation
      } else {
        # subset for readibility
        player_a_hist_fpts <- row[3:(3+temp_num_dates-1)]
        player_b_hist_fpts <- row[(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)]

        # compute covariance
        cov_row <- cov(as.numeric(player_a_hist_fpts), as.numeric(player_b_hist_fpts), use = "pairwise.complete.obs")

        return(cov_row)
      }
    }

    # subset by upper half of covariance matrix (save half the computation)
    cov_upper <- z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]),]
    cov_upper$covariance <- apply(X = cov_upper, MARGIN = 1, FUN = only_keep_teammates)

    # compute covariance for the subsetted part of matrix
    z[paste0(z$x, ", ", z$y) %in% paste0(inds_upper_cov[,1], ", ", inds_upper_cov[,2]), 'covariance'] <- cov_upper$covariance

    return(z$covariance)
  }

  # contruct covariance matrix (fast way)
  cov_mat <- as.data.frame(outer(1:nrow(cov_mat), 1:ncol(cov_mat), FUN=fill_cov_mat))
  colnames(cov_mat) <- rownames(hist_fpts_mat)
  rownames(cov_mat) <- rownames(hist_fpts_mat)

  # copy upper triangle of cov matrix into lower triangle
  cov_mat[lower.tri(cov_mat)] <- t(cov_mat)[lower.tri(cov_mat)]


  ####### Construct Covariance Counts Matrix #######
  # function to construct counts matrix (fast way)
  fill_cov_counts_mat <- function(x, y) {
    # vectorize indicies for outer function
    # each (x_i, y_i) pair is the indicies of an element in the matrix we desire to construct
    z <- as.data.frame(cbind(x, y))

    # append player_x and player_y historical fpts columns to z for fast vectorized computation
    temp_num_dates <- nrow(hist_fpts_mat_temp)
    z[,3:(3+temp_num_dates-1)] <- hist_fpts_mat[z$x,]
    z[,(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)] <- hist_fpts_mat[z$y,]

    # inds for count computation (don't need to compute for covariance NAs)
    temp_mat <- cov_mat
    temp_mat[is.na(temp_mat)] <- "FALSE" # needs to be string for some type reason
    temp_mat_inds <- which(temp_mat!="FALSE", arr.ind = T)

    # function for computing covariance, excluding when players not on same team
    only_keep_teammates_count_games <- function(row) {
      team_player_a <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[1])] # team of player a
      team_player_b <- str_split_fixed(rownames(hist_fpts_mat), "_", 2)[,2][as.numeric(row[2])] # team of player b
      if (team_player_a != team_player_b) {
        cov_and_count <- list(NA, 0) # exclude from covariance calculation, count of non-NAs is 0
        return(0)
      } else {
        # subset for readibility
        player_a_hist_fpts <- row[3:(3+temp_num_dates-1)]
        player_b_hist_fpts <- row[(3+temp_num_dates):((3+temp_num_dates)+temp_num_dates-1)]

        # count of non-NAs used in covariance calculation
        count_row <- sum(which(!is.na(player_a_hist_fpts)) %in% which(!is.na(player_b_hist_fpts)))

        return(count_row)
      }
    }

    # subset by the non-NA covariance indicies (save computation)
    cov_temp_count <- z[paste0(z$x, ", ", z$y) %in% paste0(temp_mat_inds[,1], ", ", temp_mat_inds[,2]),]
    cov_temp_count$count_games <- apply(X = cov_temp_count, MARGIN = 1, FUN = only_keep_teammates_count_games)

    # compute covariance for the subsetted part of matrix
    z[paste0(z$x, ", ", z$y) %in% paste0(temp_mat_inds[,1], ", ", temp_mat_inds[,2]), 'count_games'] <- cov_temp_count$count_games

    return(z$count_games)
  }

  # contruct counts matrix (fast way)
  cov_mat_counts <- as.data.frame(outer(1:nrow(cov_mat), 1:ncol(cov_mat), FUN=fill_cov_counts_mat))
  colnames(cov_mat_counts) <- rownames(hist_fpts_mat)
  rownames(cov_mat_counts) <- rownames(hist_fpts_mat)

  # copy upper triangle of cov matrix into lower triangle
  cov_mat_counts[lower.tri(cov_mat_counts)] <- t(cov_mat_counts)[lower.tri(cov_mat_counts)]


  # return covariance matrix and counts matrix
  return(list(cov_mat, cov_mat_counts, hist_fpts_mat))
}


# debug

# date.start <- "2017-04-02"
# date.end <- "2017-04-29"
# julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/2017-04-29/$3.00entry_MLB$5KMoonshot(Afternoon)/hitters.csv"), stringsAsFactors = F, header = T)
# min_games_pctg <- NULL

# asdf <- read.csv(file = "MLB/data_warehouse/2017-04-29/$33.00entry_MLB$110KFastball(Early)/covariance_mat_chg75p_exp(spike).csv", stringsAsFactors = F, header = T, check.names = F)


