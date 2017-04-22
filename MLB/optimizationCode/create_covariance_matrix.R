if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create covariance matrices for any given start and end date.


####### Import Functions #######
source("MLB/functions_global/create_rolling_covariance_matrix.R")


####### Construct Covariance and Counts Matrix and Write to CSV file #######
# end date in covariance matrix function
date_last <- Sys.Date()-1

# construct covariance and counts matrices
cov.dat <- create_rolling_covariance_matrix(date.start = "2017-04-02", date.end = date_last)
cov_mat <- cov.dat[[1]]
cov_mat_counts <- cov.dat[[2]]

# set NAs to 0 in covariance matrix for julia code
cov_mat[is.na(cov_mat)] <- 0
cov_mat_counts[is.na(cov_mat_counts)] <- 0

# write to date_last+1 folder because cov matrix used in julia code on day d is constructed using results from day d-1 and earlier
write.csv(cov_mat, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_mat.csv"), row.names = F)
write.csv(cov_mat_counts, file = paste0("MLB/data_warehouse/", date_last+1, "/covariance_counts_mat.csv"), row.names = F)

# loop through dates
# dates <- seq(from = as.Date("2017-04-04"), to = as.Date("2017-04-20"), by = "day")
# for (i in 1:length(dates)) {
#   # end date in covariance matrix function
#   date_last <- dates[i]
#   
#   # construct covariance and counts matrices
#   cov.dat <- create_rolling_covariance_matrix(date.start = "2017-04-02", date.end = date_last)
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
# }


####### Output Player Pairs with Highest Covariance #######
# remove the diagonal (variances)
cov_mat_unique <- cov_mat
temp_inds <- 1:nrow(cov_mat_unique)
for (i in 1:length(temp_inds)) {
  cov_mat_unique[temp_inds[i], temp_inds[i]] <- NA 
}

# view the top n largest covariances (decreasing order)
cov_mat_unique <- as.matrix(cov_mat_unique)
n <- 100
x <- which(cov_mat_unique >= sort(cov_mat_unique, decreasing = T)[n], arr.ind = T)
x.order <- order(cov_mat_unique[x], decreasing = T) # decreasing order

top_cov_pairs <- as.data.frame(matrix(data = NA, nrow = n, ncol = 4, dimnames = list(NULL, c("Player_A","Player_B","Covariance","Num_Games"))))
for (i in 1:n) {
  player_a_ind <- x[x.order,][i,][1]
  player_b_ind <- x[x.order,][i,][2]
  
  top_cov_pairs$Player_A[i] <- rownames(cov_mat_unique)[player_a_ind]
  top_cov_pairs$Player_B[i] <- rownames(cov_mat_unique)[player_b_ind]
  top_cov_pairs$Covariance[i] <- cov_mat_unique[player_a_ind, player_b_ind]
  top_cov_pairs$Num_Games[i] <- cov_mat_counts[player_a_ind, player_b_ind]
}

View(top_cov_pairs)


####### Plot Covariance of the Top 10 Pairs over Time #######
# top_cov_pairs[1:10,c("Player_A", "Player_B")]



