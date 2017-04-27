if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Calculates exposure to each player for a given set of lineups.

contest_date <- "2017-04-21"
entry_fee <- "$33.00"
contest_name <- "MLB$300KFastball"
lineup_file <- "formulations.formulation_feasibility_stacksize_5_overlap_6_lineups_150"
lineup.data <- read.csv(file = paste0("MLB/data_warehouse/", contest_date,"/" , paste0(entry_fee,"entry_",gsub(" ", "", contest_name)), "/lineups/", lineup_file, ".csv"), stringsAsFactors = F, header = T)

# All players
occurences <- sort(table(unlist(lineup.data)), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# Pitchers
occurences <- sort(table(unlist(lineup.data[,c("P", "P.1")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# Catchers
occurences <- sort(table(unlist(lineup.data[,c("C")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# 1st baseman
occurences <- sort(table(unlist(lineup.data[,c("X1B")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# 2nd baseman
occurences <- sort(table(unlist(lineup.data[,c("X2B")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# 3rd baseman
occurences <- sort(table(unlist(lineup.data[,c("X3B")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# SS baseman
occurences <- sort(table(unlist(lineup.data[,c("SS")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))

# Outfielders
occurences <- sort(table(unlist(lineup.data[,c("OF", "OF.1", "OF.2")])), decreasing=T)
exposure <- occurences / nrow(lineup.data)
exposure
paste0("Number of players: ", length(exposure))
