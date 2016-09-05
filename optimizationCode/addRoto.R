#setwd("data_warehouse")
#setwd("~/DFS/optimizationCode")
setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/optimizationCode")


#--------- offense ---------#
# read in data
dk.offense.data <- read.csv("data_warehouse/dk_offensive_players.csv", header = T, stringsAsFactors = F)
roto.offense.data <- read.csv("data_warehouse/roto_offense.csv", header = T, stringsAsFactors = F)

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
write.csv(dk.offense.data, file = 'data_warehouse/offensive_players.csv', row.names = F)


#--------- defense ---------#
# read in data
dk.defense.data <- read.csv("data_warehouse/dk_defenses.csv", header = T, stringsAsFactors = F)
roto.defense.data <- read.csv("data_warehouse/roto_defense.csv", header = T, stringsAsFactors = F)

# team names are in different formats

# write to file
write.csv(dk.defense.data, file = 'data_warehouse/defenses.csv', row.names = F)
