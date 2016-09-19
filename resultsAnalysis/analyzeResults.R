setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/resultsAnalysis")

data.week1 <- read.csv("data_warehouse/draftkings_results_week1.csv", stringsAsFactors = F)
data.week2 <- read.csv("data_warehouse/draftkings_results_week2.csv", stringsAsFactors = F)

data.week1$Percentile_Place <- 1-data.week1$Place/data.week1$Contest_Entries
data.week2$Percentile_Place <- 1-data.week2$Place/data.week2$Contest_Entries

par(mfrow=c(1,2))
hist(data.week1$Percentile, ylim=c(0,35))
hist(data.week2$Percentile, ylim=c(0,35))

max.points.week2 <- 250.52
max.points.week2 <- 238.12

data.week1$Percentile_Points <- 1-data.week1$Points/
data.week2$Percentile_Points <- 1-data.week2$Points/num.entries.week2
