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
source("MLB/functions_global/aggregate_projections.R")


####### Create Julia Inputs #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

# find all Knuckleball contests
contest_info_knuckleball <- contest_info[contest_info$Contest_Date >= "2017-04-02" & contest_info$Contest_Date <= Sys.Date() & grepl("KNUCKLEBALL", contest_info$Contest_Name),]

# julia.dat <- aggregate_projections("2017-04-10", "$5.00entry_MLB$5KKnuckleball")
# hitters.dat <- julia.dat[[1]]
# pitchers.dat <- julia.dat[[2]]



