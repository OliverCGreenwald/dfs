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
  
  for (j in 1:length(team.names)) {
    url <- paste0("http://nflsavant.com/targets.php?ddlTeam=", team.names[j], "&ddlYear=", yr, "&week=", i, "&rz=all&ddlPosition=", pos)
    temp.df <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="tblTargetsTotal"]') %>%
      html_table()
    temp.df <- temp.df[[1]]
    targets.df <- rbind(targets.df, temp.df)
  }
  
  targets.df$Week.Num <- i
  name <- paste("targets.df.wk", i, sep = "")
  assign(name, targets.df)
  
  # aggregate (we want rolling stats)
  if (i == 1) break
  
  #fpts.realized.week4$DfnPreds <- fpts.dfn.offense.week4$Proj.FP[match(fpts.realized.week4$Player, fpts.dfn.offense.week4$Player.Name)]
}

####### SET TEAM-LEVEL RANKING FOR EACH PLAYER #########
for (j in week.latest:1) {
  print(j)
  if (j == 6) break
}








