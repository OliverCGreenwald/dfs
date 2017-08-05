if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Aggregates the following data into a dataframe:
# - projections (sources: Rotogrinders, DFN)
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
  temp.dksalaries <- read.csv(file = paste0("NFL/data_warehouse/", contest.date,"/", contest.name, "/DKSalaries.csv"), stringsAsFactors = F, header = T)
  
  # clean
  temp.dksalaries$Name <- cleanPlayerNames(temp.dksalaries$Name)
  
  # add opponent column
  temp.dksalaries$Temp_Team1 <- str_split_fixed(str_split_fixed(temp.dksalaries$GameInfo, " ", 2)[,1], "@", 2)[,1]
  temp.dksalaries$Temp_Team2 <- str_split_fixed(str_split_fixed(temp.dksalaries$GameInfo, " ", 2)[,1], "@", 2)[,2]
  for (i in 1:nrow(temp.dksalaries)) {
    if (temp.dksalaries$TeamAbbrev[i]==temp.dksalaries$Temp_Team1[i]) {
      temp.dksalaries$Opponent[i] <- temp.dksalaries$Temp_Team2[i]
    } else {
      temp.dksalaries$Opponent[i] <- temp.dksalaries$Temp_Team1[i]
    }
  }
  temp.dksalaries$Temp_Team1 <- NULL
  temp.dksalaries$Temp_Team2 <- NULL
  
  # change team names to uppercase to normalize with other naming conventions
  temp.dksalaries$GameInfo <- toupper(temp.dksalaries$GameInfo)
  temp.dksalaries$TeamAbbrev <- toupper(temp.dksalaries$TeamAbbrev)
  temp.dksalaries$Opponent <- toupper(temp.dksalaries$Opponent)
  
  # split into hitters and pitchers
  temp.dksalaries.offense <- temp.dksalaries[!(temp.dksalaries$Position %in% c("DST")), ]
  temp.dksalaries.defense <- temp.dksalaries[temp.dksalaries$Position %in% c("DST"), ]
  remove(temp.dksalaries) # don't need original df anymore
  
  
  ####### Load projection Sources, Clean, and Add to DKSalaries DF (Offense) #######
  # projection file paths
  path.rotogrinders <- paste0("NFL/data_warehouse/projections/rotogrinders/roto_offense_", contest.date, ".csv")
  path.dfn <- paste0("NFL/data_warehouse/projections/dailyfantasynerd/dfn_offense_", contest.date, ".csv")
  
  # rotogrinders
  if (file.exists(path.rotogrinders)) {
    temp.rotogrinders.offense <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
    if (is.null(temp.rotogrinders.offense$player)) {
      print(paste0("Rotogrinders (offense) headers incorrect. ", contest.date))
    }
    temp.rotogrinders.offense$player <- cleanPlayerNames(temp.rotogrinders.offense$player)
    temp.dksalaries.offense$Projection <- temp.rotogrinders.offense$fpts[match(paste0(temp.dksalaries.offense$Name, temp.dksalaries.offense$Position), paste0(temp.rotogrinders.offense$player, temp.rotogrinders.offense$pos))]
  } else {
    temp.dksalaries.hitters$Projection <- NA
    warning(paste0("Rotogrinders projections not found. ", contest.date))
  }
  
  # dfn
  if (file.exists(path.dfn)) {
    temp.dfn.offense <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
    if (is.null(temp.dfn.offense$Player.Name)) {
      print(paste0("DFN (hitters) headers incorrect. ", contest.date))
    }
    temp.dfn.offense$Player.Name <- cleanPlayerNames(temp.dfn.offense$Player.Name)
    temp.dksalaries.offense$Projection_dfn <- temp.dfn.offense$Proj.FP[match(paste0(temp.dksalaries.offense$Name, temp.dksalaries.offense$Position), paste0(temp.dfn.offense$Player.Name, temp.dfn.offense$Pos))]
  } else {
    temp.dksalaries.hitters$Projection_dfn <- NA
    warning(paste0("DFN projections not found. ", contest.date))
  }

  
  # ####### Load projection Sources, Clean, and Add to DKSalaries DF (Defense) #######
  # # projection file paths
  # path.rotogrinders <- paste0("MLB/data_warehouse/projections/rotogrinders/pitchers-", contest.date, ".csv")
  # path.dfn <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/pitchers_", contest.date, ".csv")
  # 
  # # rotogrinders
  # if (file.exists(path.rotogrinders)) {
  #   temp.rotogrinders.defense <- read.csv(file = path.rotogrinders, stringsAsFactors = F, header = T)
  #   if (is.null(temp.rotogrinders.defense$Name)) {
  #     print(paste0("Rotogrinders (pitchers) headers incorrect. ", contest.date))
  #   }
  #   temp.rotogrinders.defense$Name <- cleanPlayerNames(temp.rotogrinders.defense$Name)
  #   temp.dksalaries.pitchers$Projection <- temp.rotogrinders.defense$Projections[match(temp.dksalaries.pitchers$Name, temp.rotogrinders.defense$Name)]
  # } else {
  #   temp.dksalaries.pitchers$Projection <- NA
  #   warning(paste0("Rotogrinders projections not found. ", contest.date))
  # }
  # 
  # # dfn
  # if (file.exists(path.dfn)) {
  #   temp.dfn.defense <- read.csv(file = path.dfn, stringsAsFactors = F, header = T)
  #   if (is.null(temp.dfn.defense$Player.Name)) {
  #     print(paste0("DFN (pitchers) headers incorrect. ", contest.date))
  #   }
  #   temp.dfn.defense$Player.Name <- cleanPlayerNames(temp.dfn.defense$Player.Name)
  #   temp.dksalaries.pitchers$Projection_dfn <- temp.dfn.defense$Proj.FP[match(temp.dksalaries.pitchers$Name, temp.dfn.defense$Player.Name)]
  # } else {
  #   temp.dksalaries.pitchers$Projection_dfn <- NA
  #   warning(paste0("DFN projections not found. ", contest.date))
  # }
  # 
  # ####### Add Actual Fpts Column #######
  # # Hitters (dfn)
  # # path.dfn.hitters.actual <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/updates/hitters_", contest.date, ".csv")
  # # if (file.exists(path.dfn.hitters.actual)) {
  # #   temp.dfn.offense.actual <- read.csv(path.dfn.hitters.actual, stringsAsFactors = F, header = T)
  # #   temp.dfn.offense.actual$Player.Name <- cleanPlayerNames(temp.dfn.offense.actual$Player.Name)
  # #   temp.dksalaries.hitters$Actual_fpts <- temp.dfn.offense.actual$Actual.FP[match(temp.dksalaries.hitters$Name, temp.dfn.offense.actual$Player.Name)]
  # # } else {
  # #   temp.dksalaries.hitters$Actual_fpts <- NA
  # # }
  # 
  # # Pitchers (dfn)
  # # path.dfn.pitchers.actual <- paste0("MLB/data_warehouse/projections/dailyfantasynerd/updates/pitchers_", contest.date, ".csv")
  # # if (file.exists(path.dfn.pitchers.actual)) {
  # #   temp.dfn.defense.actual <- read.csv(path.dfn.pitchers.actual, stringsAsFactors = F, header = T)
  # #   temp.dfn.defense.actual$Player.Name <- cleanPlayerNames(temp.dfn.defense.actual$Player.Name)
  # #   temp.dksalaries.pitchers$Actual_fpts <- temp.dfn.defense.actual$Actual.FP[match(temp.dksalaries.pitchers$Name, temp.dfn.defense.actual$Player.Name)]
  # # } else {
  # #   temp.dksalaries.pitchers$Actual_fpts <- NA
  # # }
  # 
  # # Hitters and Pitchers (fantasy cruncher)
  # # path.actual.fpts <- paste0("MLB/data_warehouse/", contest.date,"/player_results.csv")
  # # if (file.exists(path.actual.fpts)) {
  # #   temp.actual.fpts <- read.csv(path.actual.fpts, stringsAsFactors = F, header = T)
  # #   temp.actual.fpts$Player <- cleanPlayerNames(temp.actual.fpts$Player)
  # #   temp.dksalaries.hitters$Actual_fpts <- temp.actual.fpts$Actual.Score[match(temp.dksalaries.hitters$Name, temp.actual.fpts$Player)]
  # #   temp.dksalaries.pitchers$Actual_fpts <- temp.actual.fpts$Actual.Score[match(temp.dksalaries.pitchers$Name, temp.actual.fpts$Player)]
  # #   if (sum(is.na(temp.dksalaries.pitchers$Actual_fpts))==length(temp.dksalaries.pitchers$Actual_fpts)) {
  # #     warning(paste0("All Actual_fpts elements are NA.", contest.date))
  # #   }
  # # } else {
  # #   temp.dksalaries.hitters$Actual_fpts <- NA
  # #   warning(paste0("Fantasy Cruncher player_results.csv not found.", contest.date))
  # # }
  
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
