setwd("~/Projects/DFS/testingLineups")

abbrev <- read.csv("data_warehouse/team_abbrev_list.csv", header = T, stringsAsFactors = F)
data <- read.csv("data_warehouse/cleaned_2015_week_1.csv", header = T, stringsAsFactors = F)
projections <- read.csv("data_warehouse/2015_week_1_projections.csv", header = T, stringsAsFactors = F)
data <- merge(data,abbrev, by = 'Team')
data <- merge(data,projections, by = 'Name')
data$Salary <- substr(data$Salary, 2, nchar(data$Salary))
data$Salary <- as.numeric(gsub(",", "", data$Salary))
colnames(data)[12] <- 'Opponent'
data$Opponent <- gsub("@","", data$Opponent)

#Clean Offense so Julia Code can use it: 
offense <- subset(data, Position != "DEF")
# split roto names into first and last name columns
library(stringr)
first.last.name <- str_split_fixed(offense$Name, " ", 2)
colnames(first.last.name) <- c("FirstName", "LastName")
offense <- cbind(first.last.name, offense[,2:ncol(offense)])
# offense$Name <- NULL # DON'T need for Week 1

offense <- offense[,c(1,2,5,4,15,13,6,16)]
colnames(offense)[5] <- 'Team'
offense$Team <- substr(offense$Team, 2, nchar(offense$Team)) # Weird Error where `Opponent` had an extra space infront of it i.e ' OAK'
colnames(offense)[8] <- 'Projection'
offense <- subset(offense, ProductionPoints != 0)

defense <- subset(data, Position == "DEF")
defense <- defense[,c(1,4,14,12,5,15)]
colnames(defense)[3] <- 'Team'
defense$Team <- substr(defense$Team, 2, nchar(defense$Team)) # Weird Error where `Opponent` had an extra space infront of it i.e ' OAK'
colnames(defense)[6] <- 'Projection'

write.csv(offense, file = 'data_warehouse/offensive_players.csv', row.names = FALSE)
write.csv(defense, file = 'data_warehouse/defenses.csv', row.names = FALSE)
