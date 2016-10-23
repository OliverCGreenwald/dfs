#setwd("~/Projects/DFS/optimizationCode")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

data <- read.csv("optimizationCode/output.csv", stringsAsFactors = F, header = T)

occurences<-sort(table(unlist(data)), decreasing=T)

exposure<- occurences / nrow(data)
exposure