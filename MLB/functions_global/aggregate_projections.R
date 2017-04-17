if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Aggregates projections (hitters and pitchers) as well as actual fpts into a dataframe.
# for a given date and contest. Includes Rotogrinders, DFN, BaseballMonster, FantasyPros,
# and RotoWire projections.
#
# TODO:
# - match by player and position and team
# - BaseballMonster: MISSING Complete Game Shut Out & No Hitter DK fpts PITCHER computation
# - FantasyPros: need to compute DK fpts for both hitters and pitchers


create_julia_inputs <- function(contest.date, contest.name) {
  ####### Import Functions #######
  source("MLB/functions_global/clean_player_names.R")
  
  ####### Load and Clean DK Salaries #######
  # load
  temp.dksalaries <- read.csv(file = paste0("MLB/data_warehouse/", contest.date,"/", contest.name, "/DKSalaries.csv"), stringsAsFactors = F, header = T)
  
  # clean
  temp.dksalaries$Name <- clean_player_names(temp.dksalaries$Name)
  
  # split into hitters and pitchers
  temp.dksalaries.hitters <- temp.dksalaries[!(temp.dksalaries$Position %in% c("SP", "RP")), ]
  temp.dksalaries.pitchers <- temp.dksalaries[temp.dksalaries$Position %in% c("SP", "RP"), ]
  remove(temp.dksalaries) # don't need original df anymore
  
  
  ####### Load projection Sources, Clean, and Add to DKSalaries DF (Hitters) #######
  # projection file paths
  path.rotogrinders <- paste0("MLB/data_warehouse/projections/rotogrinders/hitters-", contest.date, ".csv")
  path.dfn <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/hitters_", contest.date, ".csv")
  path.baseballmonster <- paste0("MLB/data_warehouse/projections/baseballmonster/Export_Hitters_", contest.date, ".csv")
  path.fantasypros.hitters <- paste0("MLB/data_warehouse/projections/fantasypros/FantasyPros_2017_Projections_H-", contest.date, ".csv")
  path.rotowire.hitters <- paste0("MLB/data_warehouse/projections/rotowire/value-report-", contest.date, ".xls")
  path.rotowire2.hitters <- paste0("MLB/data_warehouse/projections/rotowire/daily_projections-", contest.date, ".xls")
  
  # rotogrinders
  if (file.exists(path.rotogrinders)) {
    temp.rotogrinders.hitters <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
    temp.rotogrinders.hitters$Name <- clean_player_names(temp.rotogrinders.hitters$Name)
    temp.dksalaries.hitters$Projection <- temp.rotogrinders.hitters$Projections[match(temp.dksalaries.hitters$Name, temp.rotogrinders.hitters$Name)]
  } else {
    temp.dksalaries.hitters$Projection <- NA
  }
  
  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn.hitters <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    temp.dfn.hitters$Player.Name <- clean_player_names(temp.dfn.hitters$Player.Name)
    temp.dksalaries.hitters$Projection_dfn <- temp.dfn.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.dfn.hitters$Player.Name)]
  } else {
    temp.dksalaries.hitters$Projection_dfn <- NA
  }
  
  # baseballmonster (fpts manually computed)
  if (file.exists(path.baseballmonster)) {
    temp.baseballmonster.hitters <- read.csv(file = path.baseballmonster, stringsAsFactors = F, header = T)
    temp.baseballmonster.hitters$Proj.FP <- 3*temp.baseballmonster.hitters$singles + 5*temp.baseballmonster.hitters$doubles + 8*temp.baseballmonster.hitters$triples + 10*temp.baseballmonster.hitters$home_runs + 2*temp.baseballmonster.hitters$rbi + 2*temp.baseballmonster.hitters$runs + 2*temp.baseballmonster.hitters$walks + 2*temp.baseballmonster.hitters$hbp + 5*temp.baseballmonster.hitters$sb # compute Fpts
    temp.baseballmonster.hitters$Name <- clean_player_names(paste0(temp.baseballmonster.hitters$first_name, " ", temp.baseballmonster.hitters$last_name))
    temp.dksalaries.hitters$Projection_baseballmonster  <- temp.baseballmonster.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.baseballmonster.hitters$Name)]
  } else {
    temp.dksalaries.hitters$Projection_baseballmonster <- NA
  }
  
  # fantasypros
  if (file.exists(path.fantasypros.hitters)) {
    temp.fantasypros.hitters <- read.csv(file = path.fantasypros.hitters, stringsAsFactors = F, header = T)
    temp.fantasypros.hitters$Player <- clean_player_names(temp.fantasypros.hitters$Player)
    # temp.dksalaries.hitters$Projection_fantasypros
  } else {
    # nothing for now
  }
  
  # rotowire
  if (file.exists(path.rotowire.hitters)) {
    temp.rotowire.hitters <- read.csv(file = path.rotowire.hitters, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire.hitters$Full.Name <- clean_player_names(temp.rotowire.hitters$Full.Name)
    temp.dksalaries.hitters$Projection_rotowire <- temp.rotowire.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.rotowire.hitters$Full.Name)]
  } else {
    temp.dksalaries.hitters$Projection_rotowire <- NA
  }
  
  # rotowire2
  if (file.exists(path.rotowire2.hitters)) {
    temp.rotowire2.hitters <- read.csv(file = path.rotowire2.hitters, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire2.hitters$Full.Name <- clean_player_names(temp.rotowire2.hitters$Full.Name)
    # temp.dksalaries.hitters$Projection_rotowire2
  } else {
    # nothing for now
  }
  
  # temp.rotogrinders.hitters <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
  # temp.dfn.hitters <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
  # temp.baseballmonster.hitters <- read.csv(file = path.baseballmonster, stringsAsFactors = F, header = T)
  # temp.fantasypros.hitters <- read.csv(file = path.fantasypros.hitters, stringsAsFactors = F, header = T)
  # temp.rotowire.hitters <- read.csv(file = path.rotowire.hitters, sep = "\t", stringsAsFactors = F, header = T)
  # temp.rotowire2.hitters <- read.csv(file = path.rotowire2.hitters, sep = "\t", stringsAsFactors = F, header = T)
  # 
  # # clean
  # temp.rotogrinders.hitters$Name <- clean_player_names(temp.rotogrinders.hitters$Name)
  # temp.dfn.hitters$Player.Name <- clean_player_names(temp.dfn.hitters$Player.Name)
  # temp.baseballmonster.hitters$Name <- clean_player_names(paste0(temp.baseballmonster.hitters$first_name, " ", temp.baseballmonster.hitters$last_name))
  # temp.fantasypros.hitters$Player <- clean_player_names(temp.fantasypros.hitters$Player)
  # temp.rotowire.hitters$Full.Name <- clean_player_names(temp.rotowire.hitters$Full.Name)
  # temp.rotowire2.hitters$Full.Name <- clean_player_names(temp.rotowire2.hitters$Full.Name)
  # 
  # # add projections to DKSalaries df
  # temp.dksalaries.hitters$Projection <- temp.rotogrinders.hitters$Projections[match(temp.dksalaries.hitters$Name, temp.rotogrinders.hitters$Name)]
  # temp.dksalaries.hitters$Projection_dfn <- temp.dfn.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.dfn.hitters$Player.Name)]
  # temp.dksalaries.hitters$Projection_rotowire <- temp.rotowire.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.rotowire.hitters$Full.Name)]
  
  
  ####### Load projection Sources, Clean, and Add to DKSalaries DF (Pitchers) #######
  # projection file paths
  path.rotogrinders <- paste0("MLB/data_warehouse/projections/rotogrinders/pitchers-", contest.date, ".csv")
  path.dfn <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/pitchers_", contest.date, ".csv")
  path.baseballmonster <- paste0("MLB/data_warehouse/projections/baseballmonster/Export_Pitchers_", contest.date, ".csv")
  path.fantasypros.pitchers <- paste0("MLB/data_warehouse/projections/fantasypros/FantasyPros_2017_Projections_P-", contest.date, ".csv")
  path.rotowire.pitchers <- paste0("MLB/data_warehouse/projections/rotowire/value-report-", contest.date, ".xls")
  path.rotowire2.pitchers <- paste0("MLB/data_warehouse/projections/rotowire/daily_projections-", contest.date, ".xls")
  
  # rotogrinders
  if (file.exists(path.rotogrinders)) {
    temp.rotogrinders.pitchers <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
    temp.rotogrinders.pitchers$Name <- clean_player_names(temp.rotogrinders.pitchers$Name)
    temp.dksalaries.pitchers$Projection <- temp.rotogrinders.pitchers$Projections[match(temp.dksalaries.pitchers$Name, temp.rotogrinders.pitchers$Name)]
  } else {
    temp.dksalaries.pitchers$Projection <- NA
  }
  
  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn.pitchers <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    temp.dfn.pitchers$Player.Name <- clean_player_names(temp.dfn.pitchers$Player.Name)
    temp.dksalaries.pitchers$Projection_dfn <- temp.dfn.pitchers$Proj.FP[match(temp.dksalaries.pitchers$Name, temp.dfn.pitchers$Player.Name)]
  } else {
    temp.dksalaries.pitchers$Projection_dfn <- NA
  }
  
  # baseballmonster
  if (file.exists(path.baseballmonster)) {
    temp.baseballmonster.pitchers <- read.csv(file = path.baseballmonster, stringsAsFactors = F, header = T)
    hits_against <- temp.baseballmonster.pitchers$singles + temp.baseballmonster.pitchers$doubles + temp.baseballmonster.pitchers$triples + temp.baseballmonster.pitchers$home_runs # for projections
    temp.baseballmonster.pitchers$Proj.FP <- 2.25*temp.baseballmonster.pitchers$innings + 2*temp.baseballmonster.pitchers$strikeouts + 4*temp.baseballmonster.pitchers$wins + (-2)*temp.baseballmonster.pitchers$earned_runs + (-0.6)*hits_against + (-0.6)*temp.baseballmonster.pitchers$walks + (-0.6)*temp.baseballmonster.pitchers$hbp + 2.5*temp.baseballmonster.pitchers$cg # MISSING: Complete Game Shut Out, No Hitter (Notes: "hbp" is "Hit Batsman")
    temp.baseballmonster.pitchers$Name <- clean_player_names(paste0(temp.baseballmonster.pitchers$first_name, " ", temp.baseballmonster.pitchers$last_name))
    temp.dksalaries.pitchers$Projection_baseballmonster  <- temp.baseballmonster.pitchers$Proj.FP[match(temp.dksalaries.pitchers$Name, temp.baseballmonster.pitchers$Name)]
  } else {
    temp.dksalaries.pitchers$Projection_baseballmonster <- NA
  }
  
  # fantasypros
  if (file.exists(path.fantasypros.pitchers)) {
    temp.fantasypros.pitchers <- read.csv(file = path.fantasypros.pitchers, stringsAsFactors = F, header = T)
    temp.fantasypros.pitchers$Player <- clean_player_names(temp.fantasypros.pitchers$Player)
    # temp.dksalaries.pitchers$Projection_fantasypros
  } else {
    # nothing for now
  }
  
  # rotowire
  if (file.exists(path.rotowire.pitchers)) {
    temp.rotowire.pitchers <- read.csv(file = path.rotowire.pitchers, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire.pitchers$Full.Name <- clean_player_names(temp.rotowire.pitchers$Full.Name)
    temp.dksalaries.pitchers$Projection_rotowire <- temp.rotowire.pitchers$Proj.FP[match(temp.dksalaries.pitchers$Name, temp.rotowire.pitchers$Full.Name)]
  } else {
    temp.dksalaries.pitchers$Projection_rotowire <- NA
  }
  
  # rotowire2
  if (file.exists(path.rotowire2.pitchers)) {
    temp.rotowire2.pitchers <- read.csv(file = path.rotowire2.pitchers, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire2.pitchers$Full.Name <- clean_player_names(temp.rotowire2.pitchers$Full.Name)
    # temp.dksalaries.pitchers$Projection_rotowire2
  } else {
    # nothing for now
  }
  
  
  ####### Add Actual Fpts Column #######
  # Hitters
  temp.dfn.hitters.actual <- read.csv(paste0("MLB/data_warehouse/projections/dailyfantasynerd/actual/hitters_", contest.date, ".csv"), stringsAsFactors = F, header = T)
  temp.dfn.hitters.actual$Player.Name <- clean_player_names(temp.dfn.hitters.actual$Player.Name)
  temp.dksalaries.hitters$Actual_fpts <- temp.dfn.hitters.actual$Actual.FP[match(temp.dksalaries.hitters$Name, temp.dfn.hitters.actual$Player.Name)]
  
  # Pitchers
  temp.dfn.pitchers.actual <- read.csv(paste0("MLB/data_warehouse/projections/dailyfantasynerd/actual/pitchers_", contest.date, ".csv"), stringsAsFactors = F, header = T)
  temp.dfn.pitchers.actual$Player.Name <- clean_player_names(temp.dfn.pitchers.actual$Player.Name)
  temp.dksalaries.pitchers$Actual_fpts <- temp.dfn.pitchers.actual$Actual.FP[match(temp.dksalaries.pitchers$Name, temp.dfn.pitchers.actual$Player.Name)]
  
  
  ####### Write to File #######
  
  
  # return
  julia.inputs <- list(temp.dksalaries.hitters, temp.dksalaries.pitchers)
  return(julia.inputs)
}


# trash
# contest.date <- "2017-04-10"
# contest.name <- "$5.00entry_MLB$5KKnuckleball"
# 
# temp.fantasypros.hitters <- read.csv(file = paste0("MLB/data_warehouse/projections/fantasypros/FantasyPros_2017_Projections_H-", contest.date, ".csv"), stringsAsFactors = F, header = T)
# temp.fantasypros.hitters$Proj.FP <- 3*temp.fantasypros.hitters$
# 
# 
