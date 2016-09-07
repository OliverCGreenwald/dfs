setwd("~/Projects/DFS/testingLineups")

abbrev <- read.csv("data_warehouse/team_abbrev_list.csv", header = T, stringsAsFactors = F)
data <- read.csv("data_warehouse/cleaned_2015_week_1.csv", header = T, stringsAsFactors = F)
data <- merge(data,abbrev, by = 'Team')

#Clean Offense so Julia Code can use it: 
offense <- subset(data, Position != "DEF")
offense$Salary <- substr(offense$Salary, 2, nchar(offense$Salary))
offense$Salary <- as.numeric(gsub(",", "", offense$Salary))
# split roto names into first and last name columns
library(stringr)
first.last.name <- str_split_fixed(offense$Name, " ", 2)
colnames(first.last.name) <- c("FirstName", "LastName")
offense <- cbind(first.last.name, offense[,2:ncol(offense)])
offense$Name <- NULL
colnames(offense)[12] <- 'Opponent'
offense$Opponent <- gsub("@","", offense$Opponent)

#Get ready to send 

defense <- subset(data, Position == "DEF")
