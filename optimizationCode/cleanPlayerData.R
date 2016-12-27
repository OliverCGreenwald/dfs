#setwd("~/Projects/DFS/optimizationCode")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### WRITE TO FILE? #######
write.bool <- T # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### Set Parameters #######
live.bool <- T # True if live week (i.e. pull current data from RG), False if historical
slate.days <- "" # "thu-mon" or "sun-mon" or ""
week.num <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) # live
# week.num <- 15 # historical


####### Get RG projections (csv format) #######
if (live.bool == T) {
  # create offense csv
  qb.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-qb.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(qb.data) <- qb.data[nrow(qb.data),]
  qb.data <- qb.data[-nrow(qb.data),]
  
  flex.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-flex.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(flex.data) <- flex.data[nrow(flex.data),]
  flex.data <- flex.data[-nrow(flex.data),]
  
  off.data <- rbind(qb.data, flex.data)
  if (write.bool==T) {
    write.csv(off.data, file = paste0("optimizationCode/data_warehouse/rotogrinders/roto_offense_week", week.num, ".csv"), row.names = F) # write to file 
  }
  
  # create defense csv
  def.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-defense.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(def.data) <- def.data[nrow(def.data),]
  def.data <- def.data[-nrow(def.data),]
  if (write.bool==T) {
    write.csv(def.data, file = paste0("optimizationCode/data_warehouse/rotogrinders/roto_defense_week", week.num, ".csv"), row.names = F) # write to file
  }
} else {
  off.data <- read.csv(file = paste0("optimizationCode/data_warehouse/rotogrinders/roto_offense_week", week.num, ".csv"), stringsAsFactors = F)
  def.data <- read.csv(file = paste0("optimizationCode/data_warehouse/rotogrinders/roto_defense_week", week.num, ".csv"), stringsAsFactors = F)
}


####### Clean DK salaries data #######
if (slate.days == "thu-mon") {
  file.name <- paste0("optimizationCode/data_warehouse/draftkings/includes_thu-mon/DKSalaries_week", week.num, ".csv")
  data <- read.csv(file = file.name, stringsAsFactors = F)
} else if (slate.days == "sun-mon") {
  file.name <- paste0("optimizationCode/data_warehouse/draftkings/includes_sun-mon/DKSalaries_week", week.num, ".csv")
  data <- read.csv(file = file.name, stringsAsFactors = F)
} else {
  file.name <- paste0("optimizationCode/data_warehouse/draftkings/DKSalaries_week", week.num, ".csv")
  data <- read.csv(file = file.name, stringsAsFactors = F)
}

data["FirstName"] <- ""
data["LastName"] <- ""
data["Opponent"] <- ""
#data["HomeTeam"] <- ""
#data["AwayTeam"] <- ""

#colnames(data)[5] <- "Projection"
colnames(data)[7] <- "Team"

for (i in 1:nrow(data)) {
  gameInfo <- data[i, "GameInfo"]
  name <- data[i, "Name"]
  
  AwayTeam <- substr(gameInfo, 1, regexpr('@', gameInfo) - 1)
  #data[i, "AwayTeam"] <- AwayTeam
  
  HomeTeam <- substr(gameInfo, regexpr('@', gameInfo) + 1, regexpr(' ', gameInfo) - 1)
  #data[i, "HomeTeam"] <- HomeTeam
  
  FirstName <- substr(name, 1, regexpr(' ', name) - 1)
  data[i, "FirstName"] <- FirstName
  
  LastName <- substr(name, regexpr(' ', name) + 1, nchar(name))
  LastName <- sub(' Sr.', '', LastName)
  LastName <- sub(' Jr.', '', LastName)
  data[i, "LastName"] <- LastName
  
  if(AwayTeam == (data[i, "Team"])) {
    data[i, "Opponent"] <- HomeTeam
  } else {
    data[i, "Opponent"] <- AwayTeam
  }
}

#Create inputs for Julia Optimization
offensive_players <- subset(data, Position != "DST")
#offensive_players <- offensive_players[,c("FirstName", "LastName", "Salary", "Position", "Team", "Opponent", "Projection")]

defense <- subset(data, Position == "DST")
#defense <- defense[,c("FirstName", "Salary", "Team", "Opponent", "Projection")]
colnames(defense)[1] <- "Name"


####### Replace DK offensive player predictions with RG predictions #######
# read in data
dk.offense.data <- offensive_players
roto.offense.data <- off.data

# compare dk and roto data
nrow(dk.offense.data[dk.offense.data$Projection>0,])
nrow(roto.offense.data[roto.offense.data$fpts>0,])

# split roto names into first and last name columns
#library(stringr)
#first.last.name <- str_split_fixed(roto.offense.data$player, " ", 2)
#colnames(first.last.name) <- c("FirstName", "LastName")
#roto.offense.data <- cbind(first.last.name, roto.offense.data[,2:ncol(roto.offense.data)])

# combine dk first and last names
dk.offense.data$FullName <- paste(dk.offense.data$FirstName, dk.offense.data$LastName)

# replace dk projections with roto projections
dk.offense.data$RotoProjection <- roto.offense.data$fpts[match(paste(dk.offense.data$FullName,dk.offense.data$Position), paste(roto.offense.data$player,roto.offense.data$pos))]
dk.offense.data$RotoProjection[is.na(dk.offense.data$RotoProjection)] <- 0
dk.offense.data$Projection <- dk.offense.data$RotoProjection
dk.offense.data$RotoProjection <- NULL

####### Add Daily Fantasy Nerd Projections #######
file.name <- paste0("optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week", week.num, ".csv")
if(file.exists(file.name)) {
  dfn_offense <- read.csv(file = file.name, header = T, stringsAsFactors = F)

  dfn_offense$Player.Name <- sub(' Sr.', '', dfn_offense$Player.Name)
  dfn_offense$Player.Name <- sub(' Jr.', '', dfn_offense$Player.Name)

  dfn_offense <- dfn_offense[,c('Player.Name','Proj.FP', 'Pos')]
  colnames(dfn_offense) <- c('Name', 'Projection_dfn', 'Position')

  #dk.offense.data <- merge(dk.offense.data, dfn_offense, by.x = 'Name')
  
  dk.offense.data$Projection_dfn <- dfn_offense$Projection_dfn[match(paste(dk.offense.data$FullName,dk.offense.data$Position), paste(dfn_offense$Name,dfn_offense$Position))]
  dk.offense.data$Projection_dfn[is.na(dk.offense.data$Projection_dfn)] <- 0
  
  dk.offense.data$FullName <- NULL
}

#---- formatting changes to Roto made in wk 8 ----#
#dk.offense.data$Team <- dk.offense.data$teamAbbrev

# write to file
if (write.bool==T) {
  write.csv(dk.offense.data, file = 'optimizationCode/data_warehouse/offensive_players.csv', row.names = F) # input in julia code 
}


####### Replace DK defensive player predictions with RG predictions #######
# read in data
dk.defense.data <- defense
roto.defense.data <- def.data

# reconcile team name differences
team.names.data <- read.csv("optimizationCode/data_warehouse/rotogrinders/team_names.csv", header = T, stringsAsFactors = F)
dk.defense.data$roto_name <- team.names.data$roto_name[match(dk.defense.data$Team, team.names.data$dk_name)] # dk.defense.data$Team # dk.defense.data$teamAbbrev
dk.defense.data$RotoProjection <- roto.defense.data$fpts[match(dk.defense.data$roto_name, roto.defense.data$team)]
dk.defense.data$Projection <- dk.defense.data$RotoProjection
dk.defense.data$RotoProjection <- NULL
dk.defense.data$roto_name <- NULL
colnames(dk.defense.data)[1] <- 'Position'


####### Add Daily Fantasy Nerd Projections #######
file.name <- paste0("optimizationCode/data_warehouse/dailyfantasynerd/dfn_defense_week", week.num, ".csv")
if(file.exists(file.name)) {
  dfn_defense <- read.csv(file = file.name, header = T, stringsAsFactors = F)
  dfn_defense$Player.Name <- substr(dfn_defense$Player.Name, regexpr(" [^ ]*$", dfn_defense$Player.Name) + 1, nchar(dfn_defense$Player.Name))
  dfn_defense$Player.Name <- paste(dfn_defense$Player.Name, ' ', sep='')
  dfn_defense <- dfn_defense[,c('Player.Name','Proj.FP')]
  colnames(dfn_defense) <- c('Name', 'Projection_dfn')

  # merge
  dk.defense.data$Projection_dfn <- dfn_defense$Projection_dfn[match(dk.defense.data$Name, dfn_defense$Name)]
  
}

#---- formatting changes to Roto made in wk 8 ----#
#dk.defense.data$Team <- dk.defense.data$teamAbbrev

# write to file
if (write.bool==T) {
  write.csv(dk.defense.data, file = 'optimizationCode/data_warehouse/defenses.csv', row.names = F) # input in julia code 
}
