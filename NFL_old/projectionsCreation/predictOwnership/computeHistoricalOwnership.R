#setwd("~/Projects/DFS")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we compute ownership rates for all players in past weeks. $3 sunday play action contest.
#
# Dependencies: cleanContestResults.R


####### IMPORT LIBRARIES #########
library('stringr')


####### SET SECTION TO RUN #######
# section.run <- "1" # 1 or "LOAD" # run LOAD first, then any section


####### SET PARAMETERS #######
contest.name <- "playaction_contest" # "playaction_contest" or "millymaker_contest"

wks.3 <- c(2:9,11:15,17) # $3 entry contest [50k to 1st]
wks.4 <- c(10,16)


####### READ IN CLEANED RESULTS FILES #########
# if (section.run=="LOAD") {
  for (i in c(wks.3, wks.4)) {
    name <- paste0("contest_playaction_results_wk", i)
    assign(name, read.csv(file = paste0("resultsAnalysis/data_warehouse/contest_results/cleaned/playaction_contest_full_results_week", i, ".csv"), stringsAsFactors = F))
    print(i)
  }
# }


####### FUNCTIONS ######
cleanJrSr <- function(vec) {
  vec <- sub(' Sr.', '', vec)
  vec <- sub(' Jr.', '', vec)
  return(vec)
}


####### COMPUTE OWNERSHIP LEVEL FOR EACH PLAYER #########
# if (section.run=="1") {
  for (i in c(wks.3, wks.4)) {
    # load and clean contest results for week
    temp.results <- eval(parse(text=paste0("contest_playaction_results_wk", i)))
    temp.results$QB <- cleanJrSr(temp.results$QB)
    temp.results$RB1 <- cleanJrSr(temp.results$RB1)
    temp.results$RB2 <- cleanJrSr(temp.results$RB2)
    temp.results$WR1 <- cleanJrSr(temp.results$WR1)
    temp.results$WR2 <- cleanJrSr(temp.results$WR2)
    temp.results$WR3 <- cleanJrSr(temp.results$WR3)
    temp.results$TE <- cleanJrSr(temp.results$TE)
    temp.results$FLEX <- cleanJrSr(temp.results$FLEX)
    
    # remove empty lineups
    temp.results <- temp.results[-c(which(temp.results$Lineup=="")),]
    
    # load projections for week and init df for storing owernship levels (need this bc don't know position of FLEX in contest results)
    # offense
    temp.projections.off <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, ".csv"), stringsAsFactors = F)
    temp.ownerships.off <- temp.projections.off[, c("Player.Name", "Pos", "Salary", "Team", "Opp", "Proj.FP", "Actual.FP")]
    temp.ownerships.off$Ownership.Pctg <- NA
    
    # defense
    temp.projections.def <- read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week', i, ".csv"), stringsAsFactors = F)
    temp.ownerships.def <- temp.projections.def[, c("Player.Name", "Pos", "Salary", "Team", "Opp", "Proj.FP", "Actual.FP")]
    temp.ownerships.def$Ownership.Pctg <- NA
    
    # total number of entries
    num.entries <- nrow(temp.results)
    
    # compute ownership for offense (TODO: figure out how to avoid miscounting when players in FLEX share same name but different position)
    for (j in 1:nrow(temp.ownerships.off)) {
      if (temp.ownerships.off$Pos[j]=="QB") {
        temp.count <- sum(temp.results$QB==temp.ownerships.off$Player.Name[j])
        temp.ownerships.off$Ownership.Pctg[j] <- temp.count/num.entries*100
      } else if (temp.ownerships.off$Pos[j]=="RB") {
        temp.count <- sum(temp.results$RB1==temp.ownerships.off$Player.Name[j]) + sum(temp.results$RB2==temp.ownerships.off$Player.Name[j]) + sum(temp.results$FLEX==temp.ownerships.off$Player.Name[j])
        temp.ownerships.off$Ownership.Pctg[j] <- temp.count/num.entries*100
      } else if (temp.ownerships.off$Pos[j]=="WR") {
        temp.count <- sum(temp.results$WR1==temp.ownerships.off$Player.Name[j]) + sum(temp.results$WR2==temp.ownerships.off$Player.Name[j]) + sum(temp.results$WR3==temp.ownerships.off$Player.Name[j]) + sum(temp.results$FLEX==temp.ownerships.off$Player.Name[j])
        temp.ownerships.off$Ownership.Pctg[j] <- temp.count/num.entries*100
      } else if (temp.ownerships.off$Pos[j]=="TE") {
        temp.count <- sum(temp.results$TE==temp.ownerships.off$Player.Name[j]) + sum(temp.results$FLEX==temp.ownerships.off$Player.Name[j])
        temp.ownerships.off$Ownership.Pctg[j] <- temp.count/num.entries*100
      }
    }
    
    # cleaning for defense
    temp.def.names <- str_split_fixed(temp.ownerships.def$Player.Name, " ", 3) # split at " "
    for (k in 1:nrow(temp.def.names)) {
      if (temp.def.names[k,3] == "") {
        temp.ownerships.def$Player.Name[k] <- temp.def.names[k,2]
      } else {
        temp.ownerships.def$Player.Name[k] <- temp.def.names[k,3]
      }
    }
    
    # compute ownership for defense
    for (j in 1:nrow(temp.ownerships.def)) {
      temp.count <- sum(temp.results$DST==temp.ownerships.def$Player.Name[j])
      temp.ownerships.def$Ownership.Pctg[j] <- temp.count/num.entries*100
    }
    
    # remove players with 0% exposure bc many are mon or thu game players (results are for sunday-only contest. we don't have complete thu-mon contest results.)
    # temp.ownerships.off <- temp.ownerships.off[-c(which(temp.ownerships.off$Ownership.Pctg==0)),]
    # temp.ownerships.def <- temp.ownerships.def[-c(which(temp.ownerships.def$Ownership.Pctg==0)),]
    
    # write to file
    write.csv(temp.ownerships.off, file = paste0("projectionsCreation/predictOwnership/data_warehouse/historical_ownership/",contest.name,"/ownership_offense_week",i,".csv"), row.names = F)
    write.csv(temp.ownerships.def, file = paste0("projectionsCreation/predictOwnership/data_warehouse/historical_ownership/",contest.name,"/ownership_defense_week",i,".csv"), row.names = F)
    
    print(i)
  }
# }










