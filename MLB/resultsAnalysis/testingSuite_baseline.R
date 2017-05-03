######### Instructions #########

# After running, view Results using the following commands: 
# View(pnl[1:2],)
# View(pnl$Lineups[[NUMBER]])



#########   Variables   #########

# the Row number that corresponds to the desired contest in 'contest_info' 
contest_row_index <- c(235,269,340,396,141,49,44)
lineup_name <- ""
######### Code Begins #########

# After running, view Results using the following commands: 
# View(pnl[1:2],)
# View(pnl$Lineups[[NUMBER]])

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

### Save DFS Directory Path

original_wd <- getwd()

# Load in Helper Files
setwd('MLB/resultsAnalysis/helperFunctions')
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)

# Return to base directory
setwd(original_wd)

### Read in Contest File (Not necessary but useful to look at to find correct row)
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)

PnL <- data.frame(Name=character(),
                 PnL=numeric(), 
                 Lineups=list(), 
                 contest=character(),
                 Date=as.Date(character()),
                 stringsAsFactors=FALSE) 

for(contest_row in contest_row_index) {
  temp <- singleContest_manyLineups_PnL_comparison(contest_row, lineup_name)
  temp$contest <- contest_info$Contest_Name[contest_row]
  temp$Date <- as.Date(contest_info$Contest_Date)[contest_row]
  PnL <- rbind(df,temp)
}


