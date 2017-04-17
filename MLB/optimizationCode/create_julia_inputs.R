if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create julia inputs (hitters and pitchers) for each date and contest.
#
# TODO:
# - iterate through the date folders and contests


####### Import Functions #######
source("MLB/projectionsAnalysis/functions_global/aggregate_projections.R")


####### Create Julia Inputs #######
julia.dat <- create_julia_inputs("2017-04-10", "$5.00entry_MLB$5KKnuckleball")
hitters.dat <- julia.dat[[1]]
pitchers.dat <- julia.dat[[2]]