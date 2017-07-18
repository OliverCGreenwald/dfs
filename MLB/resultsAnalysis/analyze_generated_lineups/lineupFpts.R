if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### DESCRIPTION #######
# Function that takes a lineup as input and returns the fpts, mean historical fpts,
# and sd historical fpts of each player in the lineup as output.

lineupFpts <- function(contest_date, contest_name, formulation_name, lineup_index) {
  # import libraries
  require(stringr)
  
  # import functions
  source("MLB/functions_global/cleanPlayerNames.R") 
  
  # load lineup file for input contest
  lineup_file <- read.csv(file = paste0("MLB/data_warehouse/", contest_date, "/", contest_name, "/lineups/", formulation_name, ".csv"), stringsAsFactors = F, header = T)
  
  # subset single lineup in lineup file (lineup_index)
  lineup <- lineup_file[lineup_index,]
  
  # initialize output df
  output.df <- as.data.frame(matrix(data = NA, nrow = length(lineup), ncol = 5, dimnames = list(NULL, c("Player","Fpts", "Avg_Hist_Fpts", "Sd_Hist_Fpts", "Num_Games"))))
  output.df$Player <- t(lineup)
  
  # load FC results to fill Fpts column in output.df
  temp_fc_results <- read.csv(paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/player_results.csv"), stringsAsFactors = F, header = T)
  temp_fc_results$Player <- cleanPlayerNames(temp_fc_results$Player)
  
  # match player in output.df to FC results
  for (k in 1:length(lineup)) {
    if (length(which(temp_fc_results$Player==output.df$Player[k])) != 0) {
      output.df$Fpts[k] <- temp_fc_results$Actual.Score[temp_fc_results$Player==output.df$Player[k]] 
    } else {
      output.df$Fpts[k] <- NA
    }
  }
  
  # load hist_fpts_mat (batters) to fill Avg_Hist_Fpts column in output.df
  hist_fpts_mat <- read.csv(file = paste0("MLB/data_warehouse/", baseline_contests$Date[i], "/", baseline_contests$Contest_names[i], "/hist_fpts_mat.csv"), stringsAsFactors = F, header = T, check.names = F)
  
  # remove team name from player name
  hist_fpts_mat[,1] <- str_split_fixed(hist_fpts_mat[,1], "_", 2)[,1]
  
  # match player in output.df to hist_fpts_mat. compute mean and std dev
  for (k in 1:length(lineup)) {
    temp_fpts <- hist_fpts_mat[hist_fpts_mat[,1]==output.df$Player[k],][-1]
    output.df$Avg_Hist_Fpts[k] <- mean(as.numeric(temp_fpts), na.rm = T)
    output.df$Sd_Hist_Fpts[k] <- sd(as.numeric(temp_fpts), na.rm = T)
    output.df$Num_Games[k] <- sum(!is.na(temp_fpts))
  }
  
  return(output.df)
}

# load baseline contest info
baseline_contests <- read.csv(file = "MLB/optimizationCode/baseline_contests.csv", stringsAsFactors = F, header = T)
i <- 25
temp_df <- as.data.frame(matrix(data = NA, nrow = 150, ncol = 2, dimnames = list(NULL, c("Total_Fpts","output.df"))))

for (j in 1:150) {
  # cat("\n")
  output <- lineupFpts(contest_date = baseline_contests$Date[i], contest_name = baseline_contests$Contest_names[i], formulation_name = "formulations.formulation5_covar_stacksize_5_overlap_5_lineups_150_lambda_0.001_exposure_P0.8_exposure_B10.3_exposure_B20.4_exposure_B30.6_exposure_C0.3_exposure_SS0.5_exposure_OF0.6_covar_chg75p_exp(spike)", lineup_index = j)
  # print(paste0("Total fpts: ", sum(output$Fpts)))
  # print(output)
  temp_df$Total_Fpts[j] <- sum(output$Fpts)
  temp_df$output.df[j] <- list(output)
}

# sort in decreasing order of total fpts
temp_df <- temp_df[order(temp_df$Total_Fpts, decreasing = T),]

# print top 5 and bottom 5 lineups
for (k in c(1:5, 146:150)) {
  cat("\n")
  print(paste0("Total fpts: ", temp_df$Total_Fpts[k]))
  print(temp_df$output.df[[k]])
}



