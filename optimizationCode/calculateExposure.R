setwd("~/Projects/DFS/optimizationCode")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/optimizationCode")

data <- read.csv("output_new.csv", stringsAsFactors = F, header = F)

occurences<-sort(table(unlist(data)), decreasing=T)

exposure<- occurences / nrow(data)
exposure

