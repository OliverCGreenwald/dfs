setwd("~/Projects/DFS/testingLineups")
setwd("/Users/Alan/Documents/PrincetonFall16/fantasyfootball/DFS/testingLineups/")
#load("RData_files/output_2015_week1_2overlap.RData")
#load("RData_files/output_2015_week1_3overlap.RData")
#load("RData_files/output_2015_week1_4overlap.RData")
#load("RData_files/output_2015_week1_5overlap.RData")

data <- read.csv("output.csv", stringsAsFactors = F, header = F)
results_offense <- read.csv("data_warehouse/offensive_players.csv", stringsAsFactors = T, header = T)
results_defense <- read.csv("data_warehouse/defenses.csv", stringsAsFactors = T, header = T)
#plot(results_offense$Projection, results_offense$ProductionPoints, xlim=c(0, 35))
#View(results_offense[order(results_offense$ProductionPoints),])
occurences<-sort(table(unlist(data)), decreasing=T)

exposure<- occurences / nrow(data)
exposure

results_offense$Name <- paste(results_offense$FirstName, results_offense$LastName, sep = " ")
results_offense <- results_offense[,c('Name', 'Projection')]
results_defense <- results_defense[,c('Name', 'Projection')]

total_results <- rbind(results_defense, results_offense)
data$total <- 0

for (index in 1:nrow(data)){
  row <- t(data[index,])
  colnames(row) <- 'Name'
  row <- merge(row, total_results, by = 'Name')
  data$total[index] <- sum(row$Projection)
}

sort(data$total) # production points of the lineups in ascending order
View(data[order(data$total),])
