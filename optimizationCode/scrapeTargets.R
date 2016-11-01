#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we scrape targets (as well as completions, completion %, TDs, and target %) for every player
# listed on nflsavant.com.

####### IMPORT LIBRARIES #########
library('rvest')

####### SET YEAR, WEEK, POSITION #########
yr <- '2016'
wk <- ''
pos <- ''

####### SET TEAM NAMES FOLLOWING NFL SAVANT NAMING CONVENTION #########
team.names <- c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN','DET','GB','HOU','IND','JAX','KC','MIA','MIN','NE','NO','NYG','NYJ','OAK','PHI','PIT','SD','SEA','SF','LA','TB','TEN','WAS')

####### SCRAPE HTML TABLE FOR ALL TEAMS #########
targets.df <- data.frame(matrix(data = NA, nrow = 0, ncol = 9))
colnames(targets.df) <- c('Rank', 'Name', 'Team', 'Pos', 'Completions', 'Targets', 'Comp.Pct', 'TDs', 'Target.Pct')
for (i in 1:length(team.names)) {
  url <- paste0("http://nflsavant.com/targets.php?ddlTeam=", team.names[i], "&ddlYear=", yr, "&week=", wk, "&rz=all&ddlPosition=", pos)
  temp.df <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="tblTargetsTotal"]') %>%
    html_table()
  temp.df <- temp.df[[1]]
  targets.df <- rbind(targets.df, temp.df)
}

head(targets.df)



