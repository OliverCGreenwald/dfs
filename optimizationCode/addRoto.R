setwd("data_warehouse")

#--------- offense ---------#
# read in data
dk.offense.data <- read.csv("offensive_players.csv", header = T, stringsAsFactors = F)
roto.offense.data <- read.csv("roto_offense.csv", header = T, stringsAsFactors = F)

# compare dk and roto data
nrow(dk.offense.data[dk.offense.data$Projection>0,])
nrow(roto.offense.data[roto.offense.data$fpts>0,])

# split roto names into first and last name columns
library(stringr)
first.last.name <- str_split_fixed(roto.offense.data$player, " ", 2)
colnames(first.last.name) <- c("FirstName", "LastName")
roto.offense.data <- cbind(first.last.name, roto.offense.data[,2:ncol(roto.offense.data)])

#
sum(roto.offense.data$FirstName %in% dk.offense.data$FirstName == F) # number of players in roto but not in dk
#dk.offense.data[which(dk.offense.data$FirstName %in% roto.offense.data$FirstName), 'Projection'] <- roto.offense.data[which(roto.offense.data$FirstName %in% dk.offense.data$FirstName), 'fpts']


#--------- defense ---------#
# read in data
defense.data <- read.csv("defenses.csv", header = T, stringsAsFactors = F)
roto.defense.data <- read.csv("roto_defense.csv", header = T, stringsAsFactors = F)

