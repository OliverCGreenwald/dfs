if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create covariance matrices for any given start and end date.


####### Import Functions #######
source("MLB/functions_global/createRollingCovarianceMatrix.R")
source("MLB/functions_global/convertTeamNames.R")


####### Construct Covariance and Counts Matrix and Write to CSV file #######
# end date in covariance matrix function
date_last <- Sys.Date()-2 # Sys.Date()-2 because need 2 day lag

# construct covariance and counts matrices
cov.dat <- createRollingCovarianceMatrix(date.start = as.Date("2017-04-02"), date.end = date_last, julia_hitter_df = NULL)
cov_mat <- cov.dat[[1]]
cov_mat_counts <- cov.dat[[2]]

# set NAs to 0 in covariance matrix for julia code
cov_mat[is.na(cov_mat)] <- 0
cov_mat_counts[is.na(cov_mat_counts)] <- 0

# write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
write.csv(cov_mat, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_mat.csv"), row.names = F)
write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_counts_mat.csv"), row.names = F)

# loop through dates
# dates <- seq(from = as.Date("2017-04-04"), to = Sys.Date()-2, by = "day")
# for (i in 1:length(dates)) {
#   # end date in covariance matrix function
#   date_last <- dates[i]
# 
#   # construct covariance and counts matrices
#   cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = NULL)
#   cov_mat <- cov.dat[[1]]
#   cov_mat_counts <- cov.dat[[2]]
# 
#   # set NAs to 0 in covariance matrix for julia code
#   cov_mat[is.na(cov_mat)] <- 0
#   cov_mat_counts[is.na(cov_mat_counts)] <- 0
# 
#   # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
#   write.csv(cov_mat, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_mat.csv"), row.names = F)
#   write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_counts_mat.csv"), row.names = F)
# 
#   print(date_last)
# }


####### Create Covariance Matrices for a Given Day's Contests #######
# end date in covariance matrix function
date_last <- Sys.Date() - 2

# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

# subset by date
contest_info <- contest_info[contest_info$Contest_Date==as.Date(date_last+1),]

# identify contests that have the same julia input file so that we don't need to run the covariance code multiple times for the same set of players
contest_info$Match_ID <- NA
list_contest <- NULL
for (i in 1:nrow(contest_info)) {
  # load julia input file
  temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
  
  # if no other hitter dfs in list then add first df to list
  if (is.null(list_contest)==TRUE) {
    list_contest[[1]] <- temp_julia_hitter_df
  } else {
    for (j in 1:length(list_contest)) {
      if (sum(!(list_contest[[j]]$Name %in%temp_julia_hitter_df$Name)) == 0) {
        contest_info$Match_ID[i] <- j
        break
      }
      if (is.na(contest_info$Match_ID[i])==TRUE) {
        contest_info$Match_ID[i] <- i
        list_contest[[length(list_contest)+1]] <- temp_julia_hitter_df
      }
    }
  }
}

for (i in 1:nrow(contest_info)) {
  # read in julia input file for this date
  temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
  print(paste0("Begin (Contest ", i, " / ", nrow(contest_info),"): ", date_last, " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i]))))

  # construct covariance and counts matrices
  cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = temp_julia_hitter_df)
  cov_mat <- cov.dat[[1]]
  cov_mat_counts <- cov.dat[[2]]

  # set NAs to 0 in covariance matrix for julia code
  cov_mat[is.na(cov_mat)] <- 0
  cov_mat_counts[is.na(cov_mat_counts)] <- 0

  # copy upper triangle of cov matrix into lower triangle
  cov_mat[lower.tri(cov_mat)] <- t(cov_mat)[lower.tri(cov_mat)]
  cov_mat_counts[lower.tri(cov_mat_counts)] <- t(cov_mat_counts)[lower.tri(cov_mat_counts)]

  # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
  write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat.csv"), row.names = F)
  write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat.csv"), row.names = F)
  
  print(paste0("Completed: ", date_last, " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), ": ", nrow(temp_julia_hitter_df), "=", nrow(cov_mat)))
}

# loop through days if desired
# dates_last <- seq(from = as.Date("2017-04-05"), to = Sys.Date() - 2, by = "day") # Sys.Date() - 2
# for (d in 1:length(dates_last)) {
#   # load contest info file
#   contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
# 
#   # subset by date
#   date_last <- dates_last[d]
#   contest_info <- contest_info[contest_info$Contest_Date==as.Date(date_last+1),]
# 
#   for (i in 1:nrow(contest_info)) {
#     # read in julia input file for this date
#     temp_julia_hitter_df <- read.csv(file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/hitters.csv"), stringsAsFactors = F, header = T)
#     
#     # construct covariance and counts matrices
#     cov.dat <- createRollingCovarianceMatrix(date.start = "2017-04-02", date.end = date_last, julia_hitter_df = temp_julia_hitter_df)
#     cov_mat <- cov.dat[[1]]
#     cov_mat_counts <- cov.dat[[2]]
#     
#     # set NAs to 0 in covariance matrix for julia code
#     cov_mat[is.na(cov_mat)] <- 0
#     cov_mat_counts[is.na(cov_mat_counts)] <- 0
#     
#     # write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
#     write.csv(cov_mat, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_mat.csv"), row.names = F)
#     write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/covariance_counts_mat.csv"), row.names = F)
#     
#     print(paste0(date_last, " ", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), ": ", nrow(temp_julia_hitter_df), "=", nrow(cov_mat)))
#   }
# }


