if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create the DKSalaries.csv file for 2017-04-23 (missed download).


####### Import Functions #######
source("MLB/functions_global/parseContestStandings.R")
source("MLB/functions_global/cleanPlayerNames.R")
source("MLB/functions_global/convertTeamNames.R")


####### Parse Standings for each Contest #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

# subset by date
contest.date <- "2017-04-23"
contest_info <- contest_info[contest_info$Contest_Date==as.Date(contest.date),]

# parse standings for each contest
list_contest_standings <- list()
for (i in 1:nrow(contest_info)) {
  # parse
  list_contest_standings[[i]] <- parseContestStandings(contest.date = contest_info$Contest_Date[i], contest.name = paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])))
  
  # apply clean names function
  position_col_inds <- which(colnames(list_contest_standings[[i]]) %in% c('P1','P2','C','1B','2B','3B','SS','OF1','OF2','OF3'))
  for (j in 1:length(position_col_inds)) {
    list_contest_standings[[i]][,position_col_inds[j]] <- cleanPlayerNames(df_name_column = list_contest_standings[[i]][,position_col_inds[j]])
  }
}


####### Find the Unique Teams in each Contest #######
for (i in 1:nrow(contest_info)) {
  # contest standings
  temp_contest_standings <- list_contest_standings[[i]]
  
  # load DFN projections file for matching
  path.hitters.dfn <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/hitters_", contest.date, ".csv")
  path.pitchers.dfn <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/pitchers_", contest.date, ".csv")
  if (file.exists(path.hitters.dfn) & file.exists(path.pitchers.dfn)) {
    # hitters
    temp.dfn.hitters <- read.csv(file = path.hitters.dfn, stringsAsFactors = F, header = T)
    temp.dfn.hitters$Player.Name <- cleanPlayerNames(temp.dfn.hitters$Player.Name)
    
    # pitchers
    temp.dfn.pitchers <- read.csv(file = path.pitchers.dfn, stringsAsFactors = F, header = T)
    temp.dfn.pitchers$Player.Name <- cleanPlayerNames(temp.dfn.pitchers$Player.Name)
  } else {
    stop("DFN file not found.")
  }
  
  # append DFN pitchers and hitters files
  temp.dfn <- rbind(temp.dfn.pitchers[,c(1,3:6)], temp.dfn.hitters[,c(1,3:6)])
  
  # match team
  temp.ind.range <- (ncol(temp_contest_standings)+1):(ncol(temp_contest_standings)+1+length(position_col_inds)-1)
  temp_contest_standings[,temp.ind.range] <- NA
  for (j in 1:length(temp.ind.range)) {
    temp_contest_standings[,temp.ind.range[j]] <- temp.dfn$Team[match(temp_contest_standings[,position_col_inds[j]], temp.dfn$Player.Name)]
  }
  
  # number of unique teams
  temp_unique_teams <- unique(unlist(c(temp_contest_standings[,temp.ind.range])))
  temp_unique_teams <- temp_unique_teams[-which(is.na(temp_unique_teams))]
  
  # add to contest_info df
  contest_info$Num_Teams[i] <- length(temp_unique_teams)
  contest_info$Teams[i] <- list(temp_unique_teams)
  print(paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])," Num Teams: ",length(temp_unique_teams)))
  
  
  ####### Construct DKSalaries.csv File #######
  # only keep teams in the contest in DFN file
  temp.dfn <- temp.dfn[temp.dfn$Team %in% temp_unique_teams,]
  
  # rename columns to match DKSalaries
  colnames(temp.dfn)[1] <- "Name"
  colnames(temp.dfn)[2] <- "Position"
  colnames(temp.dfn)[4] <- "teamAbbrev"
  
  # replace teamAbbrev with DK convention
  temp.dfn$teamAbbrev <- convertTeamNames(team_vec = temp.dfn$teamAbbrev, from_source = "DFN", to_source = "DK")
  temp.dfn$Temp_Opp <- convertTeamNames(team_vec = sub("@", "", temp.dfn$Opp), from_source = "DFN", to_source = "DK")
  for (j in 1:nrow(temp.dfn)) {
    if (grepl("@", temp.dfn$Opp[j])==TRUE) {
      temp.dfn$Temp_Opp[j] <- paste0("@", temp.dfn$Temp_Opp[j])
    }
  }
  temp.dfn$Opp <- temp.dfn$Temp_Opp
  temp.dfn$Temp_Opp <- NULL
  
  # add game info
  temp.dfn$Temp_GameInfo <- grepl("@", temp.dfn$Opp)
  for (j in 1:nrow(temp.dfn)) {
    if (temp.dfn$Temp_GameInfo[j] == TRUE) {
      temp.dfn$GameInfo[j] <- paste0(temp.dfn$teamAbbrev[j], temp.dfn$Opp[j], " 00:00PM ET")
    } else {
      temp.dfn$GameInfo[j] <- paste0(temp.dfn$Opp[j], "@", temp.dfn$teamAbbrev[j], " 00:00PM ET")
    }
  }
  temp.dfn$Temp_GameInfo <- NULL # remove temp column
  
  # put filler (NA) avg pts per game stat
  temp.dfn$AvgPointsPerGame <- NA
  
  # remove Opp column
  temp.dfn$Opp <- NULL
  
  # reorder columns
  temp.dfn <- temp.dfn[,c(2,1,3,5,6,4)]
  
  # Write to CSV
  write.csv(temp.dfn, file = paste0("MLB/data_warehouse/", contest.date, "/", contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i]), "/DKSalaries.csv"), row.names = F)
}

