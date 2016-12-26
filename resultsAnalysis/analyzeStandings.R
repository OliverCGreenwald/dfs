#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we examine the standings (contest results), specifically looking at top users such as saahilsud
# and ChipotleAddict.


####### SET PARAMETERS #######
slate.days <- "" # "thu-mon" or "sun-mon" or ""
wks.20 <- c(2:9) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
wks.27 <- c(11:15) # c(11:15) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1 # don't need to change this ever

user.name <- "SaahilSud"


####### IMPORT LIBRARIES #########
library('stringr')


####### ANALYZE USER'S LINEUPS ACROSS WEEKS #######
# # load $20 contest results
# for (i in wks.20) {
#   name <- paste0("contest_1M_results_wk", i)
#   if (slate.days=="thu-mon") {
#     assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/$20_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
#   } else if (slate.days=="sun-mon") {
#     assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/$20_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
#   } else {
#     assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$20_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
#   }
#   print(i)
# }
# 
# # load $27 contest results
# for (i in wks.27) {
#   name <- paste0("contest_1M_results_wk", i)
#   if (slate.days=="thu-mon") {
#     assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/$27_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
#   } else if (slate.days=="sun-mon") {
#     assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/$27_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
#   } else {
#     assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/$27_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
#   }
#   print(i)
# }
# 
# 
# # cleaning
# splitPlayers <- function(x) {
#   return(str_split_fixed(x, "QB | RB | WR | TE | FLEX | DST ", 10)[2:10])
# }
# for (i in c(wks.20, wks.27)) {
#   temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
#   temp.results$TimeRemaining <- NULL
#   temp.results$X <- NULL
#   temp.results$Player <- NULL
#   temp.results$X.Drafted <- NULL
#   temp.ind <- ncol(temp.results)
#   temp.results[,(temp.ind+1):(temp.ind+9)] <- NA
#   colnames(temp.results)[(temp.ind+1):(temp.ind+9)] <- c('QB','RB1','RB2','WR1','WR2','WR3','TE','FLEX','DST')
#
#   temp.players <- data.frame(matrix(unlist(lapply(X = temp.results$Lineup[1:nrow(temp.results)], FUN = splitPlayers)), nrow=nrow(temp.results), byrow=T))
#   temp.results[,(temp.ind+1):(temp.ind+9)] <- temp.players
# 
#   temp.entry <- str_split_fixed(temp.results$EntryName, " ", 2) # split at @ symbol
#   temp.results$User.Name <- temp.entry[,1]
#   temp.results$Entry.Num <- temp.entry[,2]
#
#     if (slate.days=="thu-mon") {
#       write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/cleaned/1M_contest_full_results_week"),i,".csv", row.names = F)
#     } else if (slate.days=="sun-mon") {
#       write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/cleaned/1M_contest_full_results_week"),i,".csv", row.names = F)
#     } else {
#       write.csv(temp.results, file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/1M_contest_full_results_week",i,".csv"), row.names = F)
#     }
# 
#     print(i)
# }

# read cleaned results files
for (i in c(wks.20, wks.27)) {
  name <- paste0("contest_1M_results_wk", i)
  if (slate.days=="thu-mon") {
    assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_thu-mon/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
  } else if (slate.days=="sun-mon") {
    assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/includes_sun-mon/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
  } else {
    assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/1M_contest_full_results_week", i, ".csv"), stringsAsFactors = F)) 
  }
  print(i)
}

temp.cashing <- read.csv(file = "resultsAnalysis/data_warehouse/weekly_payout_structure/contest_1M_cashing.csv", stringsAsFactors = F)
i = 2

cashing.pct.mat <- as.data.frame(matrix(NA, nrow = length(c(wks.20, wks.27)), ncol = 2, dimnames = list(NULL, c("Week","Pct.Cashing"))))
for (i in 1:week.latest) {
  cashing.pct.mat[i,1] <- i
  if (i %in% c(wks.20, wks.27)) {
    # Compute % of submitted lineups that cashed
    temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
    temp.user.results <- temp.results[temp.results$User.Name==user.name,]
    cashing.pct.mat[i,2] <- sum(temp.user.results$Points > temp.cashing$Min[temp.cashing$Week==i])/nrow(temp.user.results)
  }
}
plot(1:week.latest, cashing.pct.mat$Pct.Cashing, xlab = "Week", ylab = "% Cashing", main = paste0("Pct of Submitted Lineups that Cash: ", user.name), type = "b")




