#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we scrape targets (as well as completions, completion %, TDs, and target %) for every player
# listed on nflsavant.com. Rolling stats are computed for all sttats besides target %.

# TODO: add position and team

####### IMPORT LIBRARIES #########
library('rvest')

####### SET YEAR, WEEK, POSITION #########
yr <- '2016'
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
pos <- ''

####### SET TEAM NAMES FOLLOWING NFL SAVANT NAMING CONVENTION #########
team.names <- c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN','DET','GB','HOU','IND','JAX','KC',
                'MIA','MIN','NE','NO','NYG','NYJ','OAK','PHI','PIT','SD','SEA','SF','LA','TB','TEN','WAS')

player.names <- c() # initialize (will store all player names)
#player.helper <- c() # helper variable for matching position and team to player

####### SCRAPE HTML TABLE FOR ALL TEAMS #########
for (i in 1:week.latest) {
  targets.df <- data.frame(matrix(data = NA, nrow = 0, ncol = 9)) # initialize df to store targets data
  #colnames(targets.df) <- c('Rank', 'Name', 'Team', 'Pos', 'Completions', 'Targets', 'Comp.Pct', 'TDs', 'Target.Pct')
  
  # iterate through all teams
  for (j in 1:length(team.names)) {
    url <- paste0("http://nflsavant.com/targets.php?ddlTeam=", team.names[j], "&ddlYear=", yr, "&week=", i, "&rz=all&ddlPosition=", pos)
    temp.df <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="tblTargetsTotal"]') %>%
      html_table()
    temp.df <- temp.df[[1]]
    targets.df <- rbind(targets.df, temp.df)
  }
  
  # add week number
  targets.df$Week.Num <- i
  
  # assign variable name (with week number) to targets.df
  name <- paste("targets.df.wk", i, sep = "")
  assign(name, targets.df)
  
  #player.names <- c(player.names, targets.df$Name)
  #player.helper <- c(player.helper, paste0(targets.df$Name,'@', targets.df$Team,'@', targets.df$Pos.))
  player.names <- c(player.names, paste0(targets.df$Name,'@', targets.df$Team,'@', targets.df$Pos.)) # prevents double counting / missing players with same name
  print(i)
}

# Set unique names
player.names <- unique(player.names)
#player.helper <- unique(player.helper)

# Completions df
completions.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
completions.weekly[,1] <- player.names
for (i in 2:(week.latest+1)) {
  targets.df <- eval(parse(text=paste("targets.df.wk", i-1, sep = "")))
  completions.weekly[,i] <- targets.df$Completions[match(completions.weekly[,1], paste0(targets.df$Name,'@', targets.df$Team,'@', targets.df$Pos.))]
  completions.weekly[is.na(completions.weekly[,i]),i] <- 0
}

# Targets df
targets.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
targets.weekly[,1] <- player.names
for (i in 2:(week.latest+1)) {
  targets.df <- eval(parse(text=paste("targets.df.wk", i-1, sep = "")))
  targets.weekly[,i] <- targets.df$Targets[match(targets.weekly[,1], paste0(targets.df$Name,'@', targets.df$Team,'@', targets.df$Pos.))]
  targets.weekly[is.na(targets.weekly[,i]),i] <- 0
}

# TDs df
TDs.weekly <- as.data.frame(matrix(data = 0, nrow = length(player.names), ncol = week.latest+1))
TDs.weekly[,1] <- player.names
for (i in 2:(week.latest+1)) {
  targets.df <- eval(parse(text=paste("targets.df.wk", i-1, sep = "")))
  TDs.weekly[,i] <- targets.df$TDs[match(TDs.weekly[,1], paste0(targets.df$Name,'@', targets.df$Team,'@', targets.df$Pos.))]
  TDs.weekly[is.na(TDs.weekly[,i]),i] <- 0
}

# Compute rolling stats
for (i in 1:week.latest) {
  # Initialize df
  temp.df <- as.data.frame(matrix(data = NA, nrow = length(player.names), ncol = 4))
  colnames(temp.df) <- c('Name','Completions.Rolling','Targets.Rolling','TDs.Rolling')
  temp.df$Name <- player.names
  
  # Compute rolling completions
  if (i==1) {
    temp.df$Completions.Rolling <- completions.weekly[,i+1]
  } else {
    temp.df$Completions.Rolling <- rowSums(completions.weekly[,2:(i+1)])
  }
  
  # Compute rolling targets
  if (i==1) {
    temp.df$Targets.Rolling <- targets.weekly[,i+1]
  } else {
    temp.df$Targets.Rolling <- rowSums(targets.weekly[,2:(i+1)])
  }
  
  # Compute rolling TDs
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
  
  # assign variable name (with week number) to targets.df
  name <- paste("rolling.stats.wk", i, sep = "")
  assign(name, temp.df)
}

####### SET TEAM-LEVEL RANKING FOR EACH PLAYER #########

####### WRITE TO FILE #########
for (i in 1:week.latest) {
  write.csv(eval(parse(text=paste0("rolling.stats.wk",i))), file = paste0('optimizationCode/data_warehouse/stats/rolling.stats.wk',i,'.csv'), row.names = F)
}
