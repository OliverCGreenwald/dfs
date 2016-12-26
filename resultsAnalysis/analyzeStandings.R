#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we examine the standings (contest results) of the millionaire maker, specifically studying top users
# such as SaahilSud and ChipotleAddict.
# Section I. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS THAT CASHED
# Section II. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS > SOME PERCENTILE
# Section III. EXAMINE NUMBER OF UNIQUE PLAYERS
# Section IV. EXAMINE POSITION EXPOSURES


####### SET PARAMETERS #######
slate.days <- "" # "thu-mon" or "sun-mon" or ""
wks.20 <- c(2:9) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
wks.27 <- c(11:15) # c(11:15) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1 # don't need to change this ever

user.name <- "ChipotleAddict" # SaahilSud, ChipotleAddict

pctls.vec <- c(0.75, 0.85, 0.95, 0.99) # percentile thresholds to plot (enter any 4 between 0.0-1.0) (for Section II)

num.games.sunday <- c(13,14,14,13,12,13,13,11,11,12,12,12,13,14,13,12) # up to wk 16 (for Section II) (note that milly maker is sunday only)


####### IMPORT LIBRARIES #########
library('stringr')
library('calibrate')


####### READ IN CLEANED RESULTS FILES #########
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


####### Section I. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS THAT CASHED #########
temp.cashing <- read.csv(file = "resultsAnalysis/data_warehouse/weekly_payout_structure/contest_1M_cashing.csv", stringsAsFactors = F)
cashing.pct.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 2, dimnames = list(NULL, c("Week","Pct.Cashing"))))
for (i in 1:week.latest) {
  cashing.pct.mat[i,1] <- i
  if (i %in% c(wks.20, wks.27)) {
    # Compute % of submitted lineups that cashed
    temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
    temp.user.results <- temp.results[temp.results$User.Name==user.name,]
    cashing.pct.mat[i,2] <- sum(temp.user.results$Points > temp.cashing$Min[temp.cashing$Week==i])/nrow(temp.user.results)
  }
}
plot(1:week.latest, cashing.pct.mat$Pct.Cashing, xlab = "Week", ylab = "% Cashing", main = paste0("Pct Lineups that Cash (MillyMaker): ", user.name), type = "b")


####### Section II. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS > SOME PERCENTILE #########
pctls.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 5, dimnames = list(NULL, c("Week", paste0(pctls.vec[1]*100,"pctle.Fpts"), paste0(pctls.vec[2]*100,"pctle.Fpts"), paste0(pctls.vec[3]*100,"pctle.Fpts"), paste0(pctls.vec[4]*100,"pctle.Fpts")))))
for (i in 1:week.latest) {
  pctls.mat[i,1] <- i
  if (i %in% c(wks.20, wks.27)) {
    # Compute % of submitted lineups that exceed fpts threshold for each percentile
    for (p in 1:length(pctls.vec)) {
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      pctls.mat[i,p+1] <- sum(temp.user.results$Points > quantile(temp.results$Points, pctls.vec[p]))/nrow(temp.user.results) 
    }
  }
}

par(mfrow=c(2,2))
for (i in 1:4) {
  plot(1:week.latest, pctls.mat[,i+1], xlab = "Week", ylab = "% Cashing", main = paste0("Pct Lineups > ",pctls.vec[i]*100,"th Pctle (MillyMaker): ",user.name), type = "b")
}

par(mfrow=c(1,1))
num.games.sunday.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 2, dimnames = list(NULL, c("Week","num.games.sunday"))))
num.games.sunday.mat[,1] <- 1:(week.latest)
num.games.sunday.mat[,2] <- num.games.sunday
plot(num.games.sunday.mat, type = 'b')


####### Section III. EXAMINE NUMBER OF UNIQUE PLAYERS #########


####### Section IV. EXAMINE POSITION EXPOSURES #########



