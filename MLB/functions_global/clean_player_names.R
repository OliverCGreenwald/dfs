if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for cleaning player names.


clean_player_names <- function(df_name_column) {
  # remove Sr. and Jr.
  df_name_column <- sub(' Sr.', '', df_name_column)
  df_name_column <- sub(' Jr.', '', df_name_column)
  
  # remove middle initial and period
  df_name_column <- gsub(" [A-Z]\\. ", " ", df_name_column)
  
  # remove all periods
  df_name_column <- sub("\\.", "", df_name_column)
  
  return(df_name_column)
}