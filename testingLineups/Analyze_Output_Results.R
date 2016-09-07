setwd("~/Projects/DFS/testingLineups")

data <- read.csv("output.csv", stringsAsFactors = F, header = F)
results_offense <- read.csv("data_warehouse/offensive_players.csv", stringsAsFactors = T, header = T)
results_defense <- read.csv("data_warehouse/defenses.csv", stringsAsFactors = T, header = T)
occurences<-sort(table(unlist(data)), decreasing=T)

exposure<- occurences / nrow(data)
exposure

results_offense$Name <- paste(results_offense$FirstName, results_offense$LastName, sep = " ")
results_offense <- results_offense[,c('Name', 'ProductionPoints')]
results_defense <- results_defense[,c('Name', 'ProductionPoints')]

total_results <- rbind(results_defense, results_offense)
data$total <- 0

for (index in 1:nrow(data)){
  row <- t(data[index,])
  colnames(row) <- 'Name'
  row <- merge(row, total_results, by = 'Name')
  data$total[index] <- sum(row$ProductionPoints)
}