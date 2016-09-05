setwd("~/Desktop/Projects/football/")

data <- read.csv("DKSalaries.csv", stringsAsFactors = F)

data["FirstName"] <- ""
data["LastName"] <- ""
data["Opponent"] <- ""
#data["HomeTeam"] <- ""
#data["AwayTeam"] <- ""

colnames(data)[5] <- "Projection"
colnames(data)[6] <- "Team"

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
  data[i, "LastName"] <- LastName
  
  if(AwayTeam == (data[i, "Team"])) {
    data[i, "Opponent"] <- HomeTeam
  } else {
    data[i, "Opponent"] <- AwayTeam
  }
}

#write.csv(data, file = 'cleaned_DKSalaries.csv')

#Create inputs for Julia Optimization
offensive_players <- subset(data, Position != "DST")
offensive_players <- offensive_players[,c("FirstName", "LastName", "Salary", "Position", "Team", "Opponent", "Projection")]

defense <- subset(data, Position == "DST")
defense <- defense[,c("FirstName", "Salary", "Team", "Opponent", "Projection")]
colnames(defense)[1] <- "Name"

write.csv(offensive_players, file = 'data_warehouse/offensive_players.csv', row.names = FALSE)
write.csv(defense, file = 'data_warehouse/defenses.csv', row.names = FALSE)
