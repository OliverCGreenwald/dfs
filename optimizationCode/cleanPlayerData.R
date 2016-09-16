setwd("~/Projects/DFS/optimizationCode")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/optimizationCode")

#--------- Clean DK salaries data ---------#

data <- read.csv("data_warehouse/draftkings/DKSalaries_week2.csv", stringsAsFactors = F)

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

#write.csv(data, file = 'draftkings/cleaned_DKSalaries.csv')

#Create inputs for Julia Optimization
offensive_players <- subset(data, Position != "DST")
#offensive_players <- offensive_players[,c("FirstName", "LastName", "Salary", "Position", "Team", "Opponent", "Projection")]

defense <- subset(data, Position == "DST")
#defense <- defense[,c("FirstName", "Salary", "Team", "Opponent", "Projection")]
colnames(defense)[1] <- "Name"

# uncomment if you'd like to use DraftKings predictions not RotoGrinders predictions
#write.csv(offensive_players, file = 'data_warehouse/offensive_players.csv', row.names = FALSE)
#write.csv(defense, file = 'data_warehouse/defenses.csv', row.names = FALSE)



#--------- Replace DK offensive player predictions with RG predictions ---------#
# read in data
dk.offense.data <- offensive_players
roto.offense.data <- read.csv("data_warehouse/rotogrinders/roto_offense_week2.csv", header = T, stringsAsFactors = F)

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
dk.offense.data$RotoProjection <- roto.offense.data$fpts[match(dk.offense.data$FullName, roto.offense.data$player)]
dk.offense.data$RotoProjection[is.na(dk.offense.data$RotoProjection)] <- 0
dk.offense.data$Projection <- dk.offense.data$RotoProjection
dk.offense.data$RotoProjection <- NULL
dk.offense.data$FullName <- NULL

# write to file
write.csv(dk.offense.data, file = 'data_warehouse/offensive_players.csv', row.names = F) # input in julia code


#--------- Replace DK defensive player predictions with RG predictions ---------#
# read in data
dk.defense.data <- defense
roto.defense.data <- read.csv("data_warehouse/rotogrinders/roto_defense_week2.csv", header = T, stringsAsFactors = F)

# reconcile team name differences
team.names.data <- read.csv("data_warehouse/rotogrinders/team_names.csv", header = T, stringsAsFactors = F)
dk.defense.data$roto_name <- team.names.data$roto_name[match(dk.defense.data$Team, team.names.data$dk_name)]
dk.defense.data$RotoProjection <- roto.defense.data$fpts[match(dk.defense.data$roto_name, roto.defense.data$team)]
dk.defense.data$Projection <- dk.defense.data$RotoProjection
dk.defense.data$RotoProjection <- NULL
dk.defense.data$roto_name <- NULL

# write to file
write.csv(dk.defense.data, file = 'data_warehouse/defenses.csv', row.names = F) # input in julia code

