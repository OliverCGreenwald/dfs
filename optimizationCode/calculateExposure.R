#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

# output.data <- read.csv("optimizationCode/output.csv", stringsAsFactors = F, header = T)
# output.data <- read.csv(file = "resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week16_dfn_formulation15_overlap_4_defexp_0.25_wrexp_0.25_rbexp_0.75_teexp_0.75_qbexp_0.5_valuewrexp_0.15.csv", stringsAsFactors = F, header = T)
# output.data <- read.csv(file = "resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week19_dfn_formulation14_overlap_4_defexp_0.25_wrexp_0.25_rbexp_0.75_teexp_0.75_qbexp_0.5.csv", stringsAsFactors = F, header = T)
output.data <- read.csv(file = "resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week19_dfn_formulation14_overlap_4_defexp_0.25_wrexp_0.75_rbexp_0.75_teexp_0.75_qbexp_0.5.csv", stringsAsFactors = F, header = T)

# all players
occurences <- sort(table(unlist(output.data)), decreasing=T)
exposure<- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# QB
occurences <- sort(table(unlist(output.data$QB)), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# TE (unless form < 4, no TE as flex)
occurences <- sort(table(unlist(output.data$TE)), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# DST
occurences <- sort(table(unlist(output.data$DST)), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# RB1 and RB2
occurences <- sort(table(unlist(output.data[,c("RB","RB.1")])), decreasing=T)
exposure<- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# WR1, WR2, WR3
occurences <- sort(table(unlist(output.data[,c("WR","WR.1","WR.2")])), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# WR1
occurences <- sort(table(unlist(output.data$WR)), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# WR2
occurences <- sort(table(unlist(output.data$WR.1)), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure

# WR3
occurences <- sort(table(unlist(output.data$WR.2)), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))

# FLEX
occurences <- sort(table(unlist(output.data$FLEX)), decreasing=T)
exposure <- occurences / nrow(output.data)
exposure
paste0("Number of players: ", length(exposure))
