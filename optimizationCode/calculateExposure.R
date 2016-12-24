#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

output.data <- read.csv("optimizationCode/output.csv", stringsAsFactors = F, header = T)
# output.data <- read.csv("optimizationCode/submitted_lineups/random_wk16/output_adjustments_new.csv", stringsAsFactors = F, header = T)

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
