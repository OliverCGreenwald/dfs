setwd("~/Projects/DFS/optimizationCode")

data <- read.csv("output.csv", stringsAsFactors = F, header = F)

occurences<-sort(table(unlist(data)), decreasing=T)

exposure<- occurences / nrow(data)
exposure

