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


####### Plot Covariance of the Top num_top_pairs Pairs (on Latest Date) over Time #######
num_top_pairs <- 10

# set date range
end_day <- Sys.Date() - 1
dates <- seq(from = as.Date("2017-04-05"), to = end_day, by = "day")

# initializations
list_cov_mats <- list()
cov_time <- as.data.frame(matrix(data = NA, nrow = num_top_pairs, ncol = length(dates), dimnames = list(paste0(top_cov_pairs$Player_A[1:num_top_pairs], ", ", top_cov_pairs$Player_B[1:num_top_pairs]), as.character(dates))))

# loop through dates
for (i in 1:length(dates)) {
  temp_cov_mat <- read.csv(file = paste0("MLB/data_warehouse/", dates[i], "/covariance_mat.csv"), header = T, stringsAsFactors = F, check.names=FALSE)
  list_cov_mats[[i]] <- temp_cov_mat
  
  for (j in 1:num_top_pairs) {
    ind_a <- which(colnames(temp_cov_mat) == top_cov_pairs$Player_A[j])
    ind_b <- which(colnames(temp_cov_mat) == top_cov_pairs$Player_B[j])
    if (length(ind_a)!=0 & length(ind_b)!=0) {
      cov_time[j,i] <- temp_cov_mat[ind_a, ind_b] 
    }
  }
}

plot(as.numeric(cov_time[1,]), type = 'l', col = 1, ylim = c(min(cov_time, na.rm = T), max(cov_time, na.rm = T)), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
# plot(as.numeric(cov_time[1,]), type = 'l', col = 1, ylim = c(0, 100), ylab = "Covariance", xlab = "Days Since 2017-04-05", main = paste0("Top Player Pairs by Covariance (as of ", end_day-1, ")"))
for (i in 2:num_top_pairs) {
  points(as.numeric(cov_time[i,]), type = 'l', col = i)
}
legend(x = "topleft",legend = c(rownames(cov_time)), lwd = 1, col = 1:num_top_pairs, cex = 0.5)

# x axis: number of times covariance has increased by threshold
# y axis: current covariances
# filter out pairs that are big covariance for low x


