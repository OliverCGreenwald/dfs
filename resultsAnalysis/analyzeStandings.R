#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we examine the standings (contest results) of the millionaire maker, specifically studying top users
# such as SaahilSud and ChipotleAddict. This file is segmented into sections.
#
# SECTION LOAD. LOAD FILES
# SECTION I. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS THAT CASHED
# SECTION II. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS > SOME PERCENTILE
# SECTION III. EXAMINE NUMBER OF UNIQUE PLAYERS (OVERALL AND BY POSITION)
# SECTION IV. COMPARE NUMBER OF UNIQUE PLAYERS TO OUR FORMULATIONS
# SECTION V. EXAMINE POSITION EXPOSURES


####### SET SECTION TO RUN #######
section.run <- 5 # 1-5 or LOAD


####### SET PARAMETERS #######
slate.days <- "" # "thu-mon" or "sun-mon" or ""
wks.20 <- c(2:9) # c(2:9) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
wks.27 <- c(11:15) # c(11:15) if using sunday only (if thu-mon or sun-mon, need to enter weeks)
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1 # don't need to change this ever

user.name <- "youdacao" # SaahilSud, youdacao, scout326, ehafner, ChipotleAddict, Theclone, awesemo, AssaniFisher, aejones, CONDIA

pctls.vec <- c(0.75, 0.85, 0.95, 0.99) # percentile thresholds to plot (enter any 4 between 0.0-1.0) (for Section II)

num.games.sunday <- c(13,14,14,13,12,13,13,11,11,12,12,12,13,14,13,12) # up to wk 16 (for Section II) (note that milly maker is sunday only)


####### IMPORT LIBRARIES #########
library('stringr')


####### READ IN CLEANED RESULTS FILES #########
if (section.run=="LOAD") {
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
}


####### SECTION I. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS THAT CASHED #########
if (section.run==1) {
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
}


####### SECTION II. COMPUTE AND PLOT PCT OF SUBMITTED LINEUPS > SOME PERCENTILE #########
if (section.run==2) {
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
  plot(num.games.sunday.mat, type = 'b', main = "Number of Sunday Games")
}


####### SECTION III. EXAMINE NUMBER OF UNIQUE PLAYERS (OVERALL AND BY POSITION) #########
if (section.run==3) {
  num.unique.players.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 8, dimnames = list(NULL, c("Week","Num.Unique.All","Num.Unique.QB","Num.Unique.RB1-2","Num.Unique.WR1-3","Num.Unique.TE","Num.Unique.FLEX","Num.Unique.DST"))))
  for (i in 1:week.latest) {
    num.unique.players.mat[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      # Compute % of submitted lineups that cashed
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      num.unique.players.mat[i,2] <- length(unique(unlist(temp.user.results[,6:14])))
      num.unique.players.mat[i,3] <- length(unique(unlist(temp.user.results[,'QB'])))
      num.unique.players.mat[i,4] <- length(unique(unlist(temp.user.results[,c('RB1','RB2')])))
      num.unique.players.mat[i,5] <- length(unique(unlist(temp.user.results[,c('WR1','WR2','WR3')])))
      num.unique.players.mat[i,6] <- length(unique(unlist(temp.user.results[,c('TE')])))
      num.unique.players.mat[i,7] <- length(unique(unlist(temp.user.results[,c('FLEX')])))
      num.unique.players.mat[i,8] <- length(unique(unlist(temp.user.results[,c('DST')])))
    }
  }
  
  par(mfrow=c(1,1))
  plot(1:week.latest, num.unique.players.mat[,2], xlab = "Week", ylab = "Num Unique Players", main = paste0(colnames(num.unique.players.mat)[2], " (MillyMaker): ", user.name), type = "b") 
  print(paste0(colnames(num.unique.players.mat)[2], " correlation: ", cor(1:week.latest, num.unique.players.mat[,2], use = "complete.obs")))
  
  par(mfrow=c(3,2))
  for (i in 2:7) {
    plot(1:week.latest, num.unique.players.mat[,i+1], xlab = "Week", ylab = "Num Unique Players", main = paste0(colnames(num.unique.players.mat)[i+1], " (MillyMaker): ", user.name), type = "b") 
    print(paste0(colnames(num.unique.players.mat)[i+1], " correlation: ", cor(1:week.latest, num.unique.players.mat[,i+1], use = "complete.obs")))
  }
}

####### SECTION IV. COMPARE NUMBER OF UNIQUE PLAYERS TO OUR LINEUPS #########
if (section.run==4) {
  
}

####### SECTION V. EXAMINE POSITION EXPOSURES #########
if (section.run==5) {
  qb.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","QB.Exposure.1","QB.Exposure.2","QB.Exposure.3","QB.Exposure.4","QB.Exposure.5"))))
  rb.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","RB.Exposure.1","RB.Exposure.2","RB.Exposure.3","RB.Exposure.4","RB.Exposure.5"))))
  wr.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","WR.Exposure.1","WR.Exposure.2","WR.Exposure.3","WR.Exposure.4","WR.Exposure.5"))))
  te.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","TE.Exposure.1","TE.Exposure.2","TE.Exposure.3","TE.Exposure.4","TE.Exposure.5"))))
  flex.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","FLEX.Exposure.1","FLEX.Exposure.2","FLEX.Exposure.3","FLEX.Exposure.4","FLEX.Exposure.5"))))
  dst.exposure.mat <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 6, dimnames = list(NULL, c("Week","DST.Exposure.1","DST.Exposure.2","DST.Exposure.3","DST.Exposure.4","DST.Exposure.5"))))
  for (i in 1:week.latest) {
    qb.exposure.mat[i,1] <- i
    if (i %in% c(wks.20, wks.27)) {
      # Compute % of submitted lineups that cashed
      temp.results <- eval(parse(text=paste0("contest_1M_results_wk", i)))
      temp.user.results <- temp.results[temp.results$User.Name==user.name,]
      temp.lineups <- temp.user.results[,6:14]
      
      occurences <- sort(table(unlist(temp.lineups[,c("QB")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      qb.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("RB1","RB2")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      rb.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("WR1","WR2","WR3")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      wr.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("TE")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      te.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("FLEX")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      flex.exposure.mat[i,2:6] <- exposure[1:5]
      
      occurences <- sort(table(unlist(temp.lineups[,c("DST")])), decreasing=T)
      exposure <- occurences / nrow(temp.lineups)
      dst.exposure.mat[i,2:6] <- exposure[1:5]
    }
  }
  
  par(mfrow=c(3,2))
  # plot qb exposure
  plot(1:week.latest, qb.exposure.mat[,2], ylim = c(min(qb.exposure.mat[,6], na.rm = T),max(qb.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 QB Exposure (MillyMaker)"), type = "b")
  points(qb.exposure.mat[,3], type = "b", col = 'red')
  points(qb.exposure.mat[,4], type = "b", col = 'blue')
  points(qb.exposure.mat[,5], type = "b", col = 'green')
  points(qb.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot rb exposure
  plot(1:week.latest, rb.exposure.mat[,2], ylim = c(min(rb.exposure.mat[,6], na.rm = T),max(rb.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 RB Exposure (MillyMaker)"), type = "b")
  points(rb.exposure.mat[,3], type = "b", col = 'red')
  points(rb.exposure.mat[,4], type = "b", col = 'blue')
  points(rb.exposure.mat[,5], type = "b", col = 'green')
  points(rb.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot wr exposure
  plot(1:week.latest, wr.exposure.mat[,2], ylim = c(min(wr.exposure.mat[,6], na.rm = T),max(wr.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 WR Exposure (MillyMaker)"), type = "b")
  points(wr.exposure.mat[,3], type = "b", col = 'red')
  points(wr.exposure.mat[,4], type = "b", col = 'blue')
  points(wr.exposure.mat[,5], type = "b", col = 'green')
  points(wr.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot te exposure
  plot(1:week.latest, te.exposure.mat[,2], ylim = c(min(te.exposure.mat[,6], na.rm = T),max(te.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 TE Exposure (MillyMaker)"), type = "b")
  points(te.exposure.mat[,3], type = "b", col = 'red')
  points(te.exposure.mat[,4], type = "b", col = 'blue')
  points(te.exposure.mat[,5], type = "b", col = 'green')
  points(te.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot flex exposure
  plot(1:week.latest, flex.exposure.mat[,2], ylim = c(min(flex.exposure.mat[,6], na.rm = T),max(flex.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 FLEX Exposure (MillyMaker)"), type = "b")
  points(flex.exposure.mat[,3], type = "b", col = 'red')
  points(flex.exposure.mat[,4], type = "b", col = 'blue')
  points(flex.exposure.mat[,5], type = "b", col = 'green')
  points(flex.exposure.mat[,6], type = "b", col = 'yellow')
  
  # plot DST exposure
  plot(1:week.latest, dst.exposure.mat[,2], ylim = c(min(dst.exposure.mat[,6], na.rm = T),max(dst.exposure.mat[,2], na.rm = T)), xlab = "Week", ylab = "Exposure", main = paste0(user.name, "'s Top 5 DST Exposure (MillyMaker)"), type = "b")
  points(dst.exposure.mat[,3], type = "b", col = 'red')
  points(dst.exposure.mat[,4], type = "b", col = 'blue')
  points(dst.exposure.mat[,5], type = "b", col = 'green')
  points(dst.exposure.mat[,6], type = "b", col = 'yellow')
}

