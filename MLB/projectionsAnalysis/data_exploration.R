if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Analyze projections. Using players from the $5 entry Knuckleball contest each week.

# TODO:
# - iterate over all contests


####### Import Functions #######
source("MLB/functions_global/aggregate_projections.R")


####### Create Aggregated Projected and Actual Fpts Dataframe For Each Week #######
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

julia.dat <- create_julia_inputs("2017-04-10", "$5.00entry_MLB$5KKnuckleball")
hitters.dat <- julia.dat[[1]]
pitchers.dat <- julia.dat[[2]]

