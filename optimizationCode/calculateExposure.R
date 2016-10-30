#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

data <- read.csv("optimizationCode/output.csv", stringsAsFactors = F, header = T)
# data <- read.csv("optimizationCode/submitted_lineups/week6_$3_lineups.csv")

occurences<-sort(table(unlist(data)), decreasing=T)

exposure<- occurences / nrow(data)
exposure
