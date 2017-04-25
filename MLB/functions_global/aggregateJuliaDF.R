if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Aggregates the following data into a dataframe:
# - projections (sources: Rotogrinders, DFN, BaseballMonster, FantasyPros (deprecated), and RotoWire)
# - projected and confirmed batting order (source: DFN)
# - actual fpts (source: Fantasy Cruncher)
#
# TODO:
# - match by player and position and team (not just player name)
# - BaseballMonster: MISSING Complete Game Shut Out & No Hitter DK fpts PITCHER computation
# - remove FantasyPros (deprecated)


aggregateJuliaDF <- function(contest.date, contest.name) {
  ####### Import Libraries #######
  require(stringr)
  
  ####### Import Functions #######
  source("MLB/functions_global/cleanPlayerNames.R")
  
  ####### Load and Clean DK Salaries #######
  # load
  temp.dksalaries <- read.csv(file = paste0("MLB/data_warehouse/", contest.date,"/", contest.name, "/DKSalaries.csv"), stringsAsFactors = F, header = T)
  
  # clean
  temp.dksalaries$Name <- cleanPlayerNames(temp.dksalaries$Name)
  
  # add opponent column
  temp.dksalaries$Temp_Team1 <- str_split_fixed(str_split_fixed(temp.dksalaries$GameInfo, " ", 2)[,1], "@", 2)[,1]
  temp.dksalaries$Temp_Team2 <- str_split_fixed(str_split_fixed(temp.dksalaries$GameInfo, " ", 2)[,1], "@", 2)[,2]
  for (i in 1:nrow(temp.dksalaries)) {
    if (temp.dksalaries$teamAbbrev[i]==temp.dksalaries$Temp_Team1[i]) {
      temp.dksalaries$Opponent[i] <- temp.dksalaries$Temp_Team2[i]
    } else {
      temp.dksalaries$Opponent[i] <- temp.dksalaries$Temp_Team1[i]
    }
  }
  temp.dksalaries$Temp_Team1 <- NULL
  temp.dksalaries$Temp_Team2 <- NULL
  
  # split into hitters and pitchers
  temp.dksalaries.hitters <- temp.dksalaries[!(temp.dksalaries$Position %in% c("SP", "RP", "P")), ]
  temp.dksalaries.pitchers <- temp.dksalaries[temp.dksalaries$Position %in% c("SP", "RP", "P"), ]
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
    temp.rotogrinders.hitters$Name <- cleanPlayerNames(temp.rotogrinders.hitters$Name)
    temp.dksalaries.hitters$Projection <- temp.rotogrinders.hitters$Projections[match(temp.dksalaries.hitters$Name, temp.rotogrinders.hitters$Name)]
  } else {
    temp.dksalaries.hitters$Projection <- NA
  }
  
  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn.hitters <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    temp.dfn.hitters$Player.Name <- cleanPlayerNames(temp.dfn.hitters$Player.Name)
    temp.dksalaries.hitters$Projection_dfn <- temp.dfn.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.dfn.hitters$Player.Name)]
  } else {
    temp.dksalaries.hitters$Projection_dfn <- NA
  }
  
  # baseballmonster (fpts manually computed)
  if (file.exists(path.baseballmonster)) {
    temp.baseballmonster.hitters <- read.csv(file = path.baseballmonster, stringsAsFactors = F, header = T)
    temp.baseballmonster.hitters$Proj.FP <- 3*temp.baseballmonster.hitters$singles + 5*temp.baseballmonster.hitters$doubles + 8*temp.baseballmonster.hitters$triples + 10*temp.baseballmonster.hitters$home_runs + 2*temp.baseballmonster.hitters$rbi + 2*temp.baseballmonster.hitters$runs + 2*temp.baseballmonster.hitters$walks + 2*temp.baseballmonster.hitters$hbp + 5*temp.baseballmonster.hitters$sb # compute Fpts
    temp.baseballmonster.hitters$Name <- cleanPlayerNames(paste0(temp.baseballmonster.hitters$first_name, " ", temp.baseballmonster.hitters$last_name))
    temp.dksalaries.hitters$Projection_baseballmonster  <- temp.baseballmonster.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.baseballmonster.hitters$Name)]
  } else {
    temp.dksalaries.hitters$Projection_baseballmonster <- NA
  }
  
  # fantasypros
  if (file.exists(path.fantasypros.hitters)) {
    temp.fantasypros.hitters <- read.csv(file = path.fantasypros.hitters, stringsAsFactors = F, header = T)
    temp.fantasypros.hitters$Player <- cleanPlayerNames(temp.fantasypros.hitters$Player)
    # temp.dksalaries.hitters$Projection_fantasypros
  } else {
    # nothing for now
  }
  
  # rotowire
  if (file.exists(path.rotowire.hitters)) {
    temp.rotowire.hitters <- read.csv(file = path.rotowire.hitters, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire.hitters$Full.Name <- cleanPlayerNames(temp.rotowire.hitters$Full.Name)
    temp.dksalaries.hitters$Projection_rotowire <- temp.rotowire.hitters$Proj.FP[match(temp.dksalaries.hitters$Name, temp.rotowire.hitters$Full.Name)]
  } else {
    temp.dksalaries.hitters$Projection_rotowire <- NA
  }
  
  # rotowire2
  if (file.exists(path.rotowire2.hitters)) {
    temp.rotowire2.hitters <- read.csv(file = path.rotowire2.hitters, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire2.hitters$Full.Name <- cleanPlayerNames(temp.rotowire2.hitters$Full.Name)
    # temp.dksalaries.hitters$Projection_rotowire2
  } else {
    # nothing for now
  }
  
  
  ####### Add Batting Order using DFN Data (Hitters) #######
  # add DFN batting order data (projected)
  if (file.exists(path.dfn)) {
    temp.dfn.hitters <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    temp.dfn.hitters$Player.Name <- cleanPlayerNames(temp.dfn.hitters$Player.Name)
    temp.dksalaries.hitters$Batting_Order_Projected <- temp.dfn.hitters$Batting.Order..Projected.[match(temp.dksalaries.hitters$Name, temp.dfn.hitters$Player.Name)]
  } else {
    temp.dksalaries.hitters$Batting_Order_Projected <- NA
  }
  
  # add DFN batting order data (confirmed)
  path.dfn.confirmed <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/updates/hitters_", contest.date, ".csv")
  if (file.exists(path.dfn)) {
    temp.dfn.hitters <- read.csv(file = path.dfn.confirmed, stringsAsFactors = F, header = T)
    temp.dfn.hitters$Player.Name <- cleanPlayerNames(temp.dfn.hitters$Player.Name)
    temp.dksalaries.hitters$Batting_Order_Confirmed <- temp.dfn.hitters$Batting.Order..Confirmed.[match(temp.dksalaries.hitters$Name, temp.dfn.hitters$Player.Name)]
  } else {
    temp.dksalaries.hitters$Batting_Order_Confirmed <- NA
  }
  
  
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
    temp.rotogrinders.pitchers$Name <- cleanPlayerNames(temp.rotogrinders.pitchers$Name)
    temp.dksalaries.pitchers$Projection <- temp.rotogrinders.pitchers$Projections[match(temp.dksalaries.pitchers$Name, temp.rotogrinders.pitchers$Name)]
  } else {
    temp.dksalaries.pitchers$Projection <- NA
  }
  
  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn.pitchers <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    temp.dfn.pitchers$Player.Name <- cleanPlayerNames(temp.dfn.pitchers$Player.Name)
    temp.dksalaries.pitchers$Projection_dfn <- temp.dfn.pitchers$Proj.FP[match(temp.dksalaries.pitchers$Name, temp.dfn.pitchers$Player.Name)]
  } else {
    temp.dksalaries.pitchers$Projection_dfn <- NA
  }
  
  # baseballmonster
  if (file.exists(path.baseballmonster)) {
    temp.baseballmonster.pitchers <- read.csv(file = path.baseballmonster, stringsAsFactors = F, header = T)
    hits_against <- temp.baseballmonster.pitchers$singles + temp.baseballmonster.pitchers$doubles + temp.baseballmonster.pitchers$triples + temp.baseballmonster.pitchers$home_runs # for projections
    temp.baseballmonster.pitchers$Proj.FP <- 2.25*temp.baseballmonster.pitchers$innings + 2*temp.baseballmonster.pitchers$strikeouts + 4*temp.baseballmonster.pitchers$wins + (-2)*temp.baseballmonster.pitchers$earned_runs + (-0.6)*hits_against + (-0.6)*temp.baseballmonster.pitchers$walks + (-0.6)*temp.baseballmonster.pitchers$hbp + 2.5*temp.baseballmonster.pitchers$cg # MISSING: Complete Game Shut Out, No Hitter (Notes: "hbp" is "Hit Batsman")
    temp.baseballmonster.pitchers$Name <- cleanPlayerNames(paste0(temp.baseballmonster.pitchers$first_name, " ", temp.baseballmonster.pitchers$last_name))
    temp.dksalaries.pitchers$Projection_baseballmonster  <- temp.baseballmonster.pitchers$Proj.FP[match(temp.dksalaries.pitchers$Name, temp.baseballmonster.pitchers$Name)]
  } else {
    temp.dksalaries.pitchers$Projection_baseballmonster <- NA
  }
  
  # fantasypros
  if (file.exists(path.fantasypros.pitchers)) {
    temp.fantasypros.pitchers <- read.csv(file = path.fantasypros.pitchers, stringsAsFactors = F, header = T)
    temp.fantasypros.pitchers$Player <- cleanPlayerNames(temp.fantasypros.pitchers$Player)
    # temp.dksalaries.pitchers$Projection_fantasypros
  } else {
    # nothing for now
  }
  
  # rotowire
  if (file.exists(path.rotowire.pitchers)) {
    temp.rotowire.pitchers <- read.csv(file = path.rotowire.pitchers, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire.pitchers$Full.Name <- cleanPlayerNames(temp.rotowire.pitchers$Full.Name)
    temp.dksalaries.pitchers$Projection_rotowire <- temp.rotowire.pitchers$Proj.FP[match(temp.dksalaries.pitchers$Name, temp.rotowire.pitchers$Full.Name)]
  } else {
    temp.dksalaries.pitchers$Projection_rotowire <- NA
  }
  
  # rotowire2
  if (file.exists(path.rotowire2.pitchers)) {
    temp.rotowire2.pitchers <- read.csv(file = path.rotowire2.pitchers, sep = "\t", stringsAsFactors = F, header = T)
    temp.rotowire2.pitchers$Full.Name <- cleanPlayerNames(temp.rotowire2.pitchers$Full.Name)
    # temp.dksalaries.pitchers$Projection_rotowire2
  } else {
    # nothing for now
  }
  
  
  ####### Add Actual Fpts Column #######
  # Hitters (dfn)
  # path.dfn.hitters.actual <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/updates/hitters_", contest.date, ".csv")
  # if (file.exists(path.dfn.hitters.actual)) {
  #   temp.dfn.hitters.actual <- read.csv(path.dfn.hitters.actual, stringsAsFactors = F, header = T)
  #   temp.dfn.hitters.actual$Player.Name <- cleanPlayerNames(temp.dfn.hitters.actual$Player.Name)
  #   temp.dksalaries.hitters$Actual_fpts <- temp.dfn.hitters.actual$Actual.FP[match(temp.dksalaries.hitters$Name, temp.dfn.hitters.actual$Player.Name)]
  # } else {
  #   temp.dksalaries.hitters$Actual_fpts <- NA
  # }
  
  # Pitchers (dfn)
  # path.dfn.pitchers.actual <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/updates/pitchers_", contest.date, ".csv")
  # if (file.exists(path.dfn.pitchers.actual)) {
  #   temp.dfn.pitchers.actual <- read.csv(path.dfn.pitchers.actual, stringsAsFactors = F, header = T)
  #   temp.dfn.pitchers.actual$Player.Name <- cleanPlayerNames(temp.dfn.pitchers.actual$Player.Name)
  #   temp.dksalaries.pitchers$Actual_fpts <- temp.dfn.pitchers.actual$Actual.FP[match(temp.dksalaries.pitchers$Name, temp.dfn.pitchers.actual$Player.Name)]
  # } else {
  #   temp.dksalaries.pitchers$Actual_fpts <- NA
  # }
  
  # Hitters and Pitchers (fantasy cruncher)
  path.actual.fpts <- paste0("MLB/data_warehouse/", contest.date,"/player_results.csv")
  if (file.exists(path.actual.fpts)) {
    temp.actual.fpts <- read.csv(path.actual.fpts, stringsAsFactors = F, header = T)
    temp.actual.fpts$Player <- cleanPlayerNames(temp.actual.fpts$Player)
    temp.dksalaries.hitters$Actual_fpts <- temp.actual.fpts$Actual.Score[match(temp.dksalaries.hitters$Name, temp.actual.fpts$Player)]
    temp.dksalaries.pitchers$Actual_fpts <- temp.actual.fpts$Actual.Score[match(temp.dksalaries.pitchers$Name, temp.actual.fpts$Player)]
  } else {
    temp.dksalaries.hitters$Actual_fpts <- NA
  }
  
  # return
  julia.inputs <- list(temp.dksalaries.hitters, temp.dksalaries.pitchers)
  return(julia.inputs)
}


# debugging
# contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
# i <- 46
# 
# contest.date <- contest_info$Contest_Date[i]
# contest.name <- paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i]))
# 
# projections.dat <- aggregate_projections(contest.date = contest_info$Contest_Date[i], contest.name = paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])))
# aggregated_data_hitters[[i]] <- projections.dat[[1]]
# aggregated_data_pitchers[[i]] <- projections.dat[[2]]

# contest.date <- "2017-04-21"
# contest.name <- "$33.00entry_MLB$300KFastball"
