#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we scrape completions, targets, completion %, TDs, and target % for every player
# listed on nflsavant.com. Rolling stats are computed for completions, targets, and TDs. We also assign
# three separate integer rankings to each player (based on rolling completions, rolling targets, and rolling
# TDs) within each team. This ranking will be minimized in the objective function of our integer program
# solved in julia. Writes weekly rolling stats to the stats folder.

####### IMPORT LIBRARIES #########
library('rvest')
library('stringr')

####### SET YEAR, WEEK, POSITION #########
yr <- '2016'
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
# week.latest <- 11
pos <- ''

####### SET TEAM NAMES FOLLOWING NFL SAVANT NAMING CONVENTION #########
team.names <- c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN','DET','GB','HOU','IND','JAX','KC',
                'MIA','MIN','NE','NO','NYG','NYJ','OAK','PHI','PIT','SD','SEA','SF','LA','TB','TEN','WAS')

player.names <- c() # initialize (will store all player names)
#player.helper <- c() # helper variable for matching position and team to player

####### SCRAPE HTML TABLE FOR ALL TEAMS #########
for (i in 1:week.latest) {
  stats.df <- data.frame(matrix(data = NA, nrow = 0, ncol = 9)) # initialize df to store targets data
  #colnames(stats.df) <- c('Rank', 'Name', 'Team', 'Pos', 'Completions', 'Targets', 'Comp.Pct', 'TDs', 'Target.Pct')
  
  # iterate through all teams
  for (j in 1:length(team.names)) {
    url <- paste0("http://nflsavant.com/targets.php?ddlTeam=", team.names[j], "&ddlYear=", yr, "&week=", i, "&rz=all&ddlPosition=", pos)
    temp.df <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="tblTargetsTotal"]') %>%
      html_table()
    temp.df <- temp.df[[1]]
    stats.df <- rbind(stats.df, temp.df)
  }
  
  # add week number
  stats.df$Week.Num <- i
  
  # assign variable name (with week number) to stats.df
  name <- paste("stats.df.wk", i, sep = "")
  assign(name, stats.df)
  
  #player.names <- c(player.names, stats.df$Name)
  #player.helper <- c(player.helper, paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))
  player.names <- c(player.names, paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.)) # prevents double counting / missing players with same name
  print(paste0('Week ', i, ' done'))
}

# Set unique names
player.names <- unique(player.names)
#player.helper <- unique(player.helper)

# Completions df
completions.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
completions.weekly[,1] <- player.names
for (i in 2:(week.latest+1)) {
  stats.df <- eval(parse(text=paste("stats.df.wk", i-1, sep = "")))
  completions.weekly[,i] <- stats.df$Completions[match(completions.weekly[,1], paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))]
  completions.weekly[is.na(completions.weekly[,i]),i] <- 0
}

# Targets df
targets.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
targets.weekly[,1] <- player.names
for (i in 2:(week.latest+1)) {
  stats.df <- eval(parse(text=paste("stats.df.wk", i-1, sep = "")))
  targets.weekly[,i] <- stats.df$Targets[match(targets.weekly[,1], paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))]
  targets.weekly[is.na(targets.weekly[,i]),i] <- 0
}

# TDs df
TDs.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
TDs.weekly[,1] <- player.names
for (i in 2:(week.latest+1)) {
  stats.df <- eval(parse(text=paste("stats.df.wk", i-1, sep = "")))
  TDs.weekly[,i] <- stats.df$TDs[match(TDs.weekly[,1], paste0(stats.df$Name,'@', stats.df$Team,'@', stats.df$Pos.))]
  TDs.weekly[is.na(TDs.weekly[,i]),i] <- 0
}

# Compute rolling stats for Completions, Targets, TDs, and Target %
for (i in 1:week.latest) {
  # Initialize df
  temp.df <- as.data.frame(matrix(data = NA, nrow = length(player.names), ncol = 4))
  colnames(temp.df) <- c('Name','Completions.Rolling','Targets.Rolling','TDs.Rolling')
  temp.df$Name <- player.names
  
  # Compute Rolling Completions
  if (i==1) {
    temp.df$Completions.Rolling <- completions.weekly[,i+1]
  } else {
    temp.df$Completions.Rolling <- rowSums(completions.weekly[,2:(i+1)])
  }
  
  # Compute Rolling Targets
  if (i==1) {
    temp.df$Targets.Rolling <- targets.weekly[,i+1]
  } else {
    temp.df$Targets.Rolling <- rowSums(targets.weekly[,2:(i+1)])
  }
  
  # Compute Rolling TDs
  if (i==1) {
    temp.df$TDs.Rolling <- TDs.weekly[,i+1]
  } else {
    temp.df$TDs.Rolling <- rowSums(TDs.weekly[,2:(i+1)])
  }
  
  # split Name column into Name, Team, Position
  name.team.pos <- str_split_fixed(temp.df$Name, "@", 3) # split at @ symbol
  temp.df$Name <- name.team.pos[,1]
  temp.df$Team <- name.team.pos[,2]
  temp.df$Pos <- name.team.pos[,3]
  
  # Compute Rolling Target %
  temp.agg <-  data.frame(temp.df$Team, temp.df$Targets.Rolling)
  temp.agg <- aggregate(temp.agg[,-c(1)], by = list(temp.agg$temp.df.Team), FUN = sum)
  temp.df$Targets.Rolling.Sum <- temp.agg$x[match(temp.df$Team, temp.agg$Group.1)]
  temp.df$Target.Ptcg.Rolling <- temp.df$Targets.Rolling/temp.df$Targets.Rolling.Sum
  temp.df$Targets.Rolling.Sum <- NULL
  
  # assign variable name (with week number) to stats.df
  assign(paste0("rolling.stats.wk",i), temp.df)
}

# Compute Rolling Target %
# i <- 1
# temp.wk <- eval(parse(text=paste0("rolling.stats.wk",i)))
# temp.agg <-  data.frame(temp.wk$Team, temp.wk$Targets.Rolling)
# temp.agg <- aggregate(temp.agg[,-c(1)], by = list(temp.agg$temp.wk.Team), FUN = sum)
# temp.wk$Targets.Rolling.Sum <- temp.agg$x[match(temp.wk$Team, temp.agg$Group.1)]
# temp.wk$Target.Ptcg.Rolling <- temp.wk$Targets.Rolling/temp.wk$Targets.Rolling.Sum
# temp.wk$Targets.Rolling.Sum <- NULL
# assign(paste0("rolling.stats.wk",i), temp.wk)

####### ASSIGN INTEGER RANKING TO EACH PLAYER WITHIN TEAMS #########
# TODO: factor in Redzone Targets, Completion %, etc
for (i in 1:week.latest) {
  temp.wk <- eval(parse(text=paste0("rolling.stats.wk",i)))
  
  # add three ranking columns based on completions, targets, TDs (for ties, assign min rank)
  temp.wk <- transform(temp.wk, Rank.Completions = ave(temp.wk$Completions.Rolling, temp.wk$Team, FUN = function(x) rank(-x, ties.method = "min")))
  temp.wk <- transform(temp.wk, Rank.Targets = ave(temp.wk$Targets.Rolling, temp.wk$Team, FUN = function(x) rank(-x, ties.method = "min")))
  temp.wk <- transform(temp.wk, Rank.TDs = ave(temp.wk$TDs.Rolling, temp.wk$Team, FUN = function(x) rank(-x, ties.method = "min")))
  
  assign(paste0("rolling.stats.wk",i), temp.wk)
}

####### WRITE TO FILE #########
# for (i in 1:week.latest) {
#   write.csv(eval(parse(text=paste0("rolling.stats.wk",i))), file = paste0('optimizationCode/data_warehouse/stats/rolling.stats.wk',i,'.csv'), row.names = F)
# }
write.csv(eval(parse(text=paste0("rolling.stats.wk",week.latest))), file = paste0('optimizationCode/data_warehouse/stats/rolling.stats.wk',i,'.csv'), row.names = F)

####### APPEND TO 2016_cleaned_input FILES #########
for (i in 2:week.latest) {
  # i <- 11
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), stringsAsFactors = F)
  
  # name cleaning shit
  temp$Temp.Name <- paste0(temp$LastName, ', ', temp$FirstName)
  temp$Temp.Name <- sub("'", "", temp$Temp.Name)
  
  # add target rank
  temp.rolling.wk <- eval(parse(text=paste0("rolling.stats.wk",i-1))) # i-1 b/c we use previous weeks rolling stats
  # temp.rolling.wk <- read.csv(file = "optimizationCode/data_warehouse/stats/rolling.stats.wk10.csv", stringsAsFactors = F)
  temp$RankTargets <- temp.rolling.wk$Rank.Targets[match(temp$Temp.Name, temp.rolling.wk$Name)]
  temp$RankTargets[is.na(temp$RankTargets)==T] <- 0
  
  # add rolling target %
  temp$RollingTargetPctg <- temp.rolling.wk$Target.Ptcg.Rolling[match(temp$Temp.Name, temp.rolling.wk$Name)]
  temp$RollingTargetPctg[is.na(temp$RollingTargetPctg)==T] <- 0
  
  temp$Temp.Name <- NULL
  write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), row.names = F)
}


