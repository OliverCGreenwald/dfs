#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we create a dataframe of all weekly historical fpts for each player. We then add a column
# to 2016_CLEANED_INPUT files that is in indicator function that takes on
# value 1 if 
# and value 0 otherwise.

####### LOAD DFN FILES (UPDATES FOLDER B/C WE NEED HISTORICAL ACTUAL FPTS) #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
  
  # add week column
  temp.df <- eval(parse(text=name))
  temp.df$Week.Num <- i
  
  # add Unique.ID column for matching purposes
  temp.df$Unique.ID <- paste0(temp.df$Player.Name, temp.df$Team, temp.df$Pos)
  
  # assign
  assign(name, temp.df)
}

####### CREATE DATAFRAME OF ALL HISTORICAL ACTUAL FPTS #######
# load list of player names (use this b/c players vary week to week. this aggregated all of them)
load(file = "projectionsAnalysis/player.names.RData")

# intialize df and add player names
historical.fpts.data <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
historical.fpts.data[,1] <- player.names

# add column names to df
colnames(historical.fpts.data)[1] <- "Unique.ID"
for (i in 2:(week.latest+1)) {
  colnames(historical.fpts.data)[i] <- paste0("Week", i-1)
}

# split Unique.ID column (using @ symbol) for matching purposes
historical.fpts.data$FullName <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,1]
historical.fpts.data$Team <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,2]
historical.fpts.data$Pos <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,3]

# further split FullName column (using ", ") for matching purposes
historical.fpts.data$Last.Name <- str_split_fixed(historical.fpts.data$FullName, ", ", 2)[,1]
historical.fpts.data$First.Name <- str_split_fixed(historical.fpts.data$FullName, ", ", 2)[,2]

# replace Unique.ID for matching
historical.fpts.data$Unique.ID <- paste0(historical.fpts.data$First.Name," ",historical.fpts.data$Last.Name,historical.fpts.data$Team,historical.fpts.data$Pos)

for (i in 2:(week.latest+1)) {
  dfn.df <- eval(parse(text=paste("dfn_offense_week", i-1, sep = "")))
  historical.fpts.data[,i] <- dfn.df$Actual.FP[match(paste0(historical.fpts.data$First.Name," ",historical.fpts.data$Last.Name,historical.fpts.data$Team,historical.fpts.data$Pos), paste0(dfn.df$Player.Name, dfn.df$Team, dfn.df$Pos))]
  # historical.fpts.data[is.na(historical.fpts.data[,i]),i] <- 0
}



