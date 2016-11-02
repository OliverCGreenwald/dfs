#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we scrape targets (as well as completions, completion %, TDs, and target %) for every player
# listed on nflsavant.com.

####### IMPORT LIBRARIES #########
library('rvest')

####### SET YEAR, WEEK, POSITION #########
yr <- '2016'
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
pos <- ''

####### SET TEAM NAMES FOLLOWING NFL SAVANT NAMING CONVENTION #########
team.names <- c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN','DET','GB','HOU','IND','JAX','KC',
                'MIA','MIN','NE','NO','NYG','NYJ','OAK','PHI','PIT','SD','SEA','SF','LA','TB','TEN','WAS')

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
  
  # aggregate (we want rolling stats, so add on previous week's data)
  if (i != 1) {
    # targets.df.prev <- eval(parse(text=paste("targets.df.wk", i-1, sep = ""))) # previous week's df
    # targets.df$Completions[match(targets.df$Name, targets.df.prev$Name)] <- targets.df$Completions + targets.df.prev$Completions # add on previous week's completions
    # targets.df$Targets[match(targets.df$Name, targets.df.prev$Name)] <- targets.df$Targets + targets.df.prev$Targets # add on previous week's targets
    # targets.df$TDs[match(targets.df$Name, targets.df.prev$Name)] <- targets.df$TDs + targets.df.prev$TDs # add on previous week's TDs
  }
  
  # assign variable name (with week number) to targets.df
  name <- paste("targets.df.wk", i, sep = "")
  assign(name, targets.df)
  
  print(i)
}

####### SET TEAM-LEVEL RANKING FOR EACH PLAYER #########
# i <- 2
# targets.df.prev <- targets.df.wk1
# targets.df <- targets.df.wk2
# ind.match <- which(targets.df$Name %in% targets.df.prev$Name)
# match(targets.df$Name, targets.df.prev$Name)
# targets.df$Completions[ind.match] <- targets.df$Completions[ind.match] + targets.df.prev$Completions[targets.df.prev$Name==targets.df$Name] # add on previous week's completions
# 
# targets.df$Completions <- targets.df$Completions + targets.df.prev$Completions[match]
# projections.data$Actual_fpts <- realized.data$Actual.Score[match(projections.data$Name, realized.data$Player)]

####### WRITE TO FILE #########
for (i in 1:week.latest) {
  
}






