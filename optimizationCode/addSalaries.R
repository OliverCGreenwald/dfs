#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #######
# In this file we create a DKSalaries_week[x] file for all players (instead of just sunday)
# using DFN files. Needed for testing Thursday-Monday contests such as NFL $150K KICKOFF SPECIAL
# and NFL $70K GRIDIRON.


####### SET WEEK #########
# week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
week.latest <- 15
i <- week.latest


####### LOAD DFN FILES #########
assign(paste0("dfn_offense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
assign(paste0("dfn_defense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_defense_week', i, '.csv'), stringsAsFactors = F))

####### AGGREGATE SALARIES FROM DFN INTO CORRECT FORMAT FOR CLEAN_PLAYER_DATA #########
temp.all <- NULL
name.sub <- 'dfn_defense_week'
for (name.sub in c('dfn_offense_week', 'dfn_defense_week')) {
  temp <- eval(parse(text=paste0(name.sub, i)))
  temp.dk <- temp[,c('Pos','Player.Name','Salary','Team','Opp')]
  colnames(temp.dk)[1] <- 'Position'
  colnames(temp.dk)[2] <- 'Name'
  colnames(temp.dk)[4] <- 'TeamAbbrev'
  
  if (name.sub == 'dfn_defense_week') {
    temp.dk$Position <- 'DST'
    
    temp.dst.teams <- str_split_fixed(temp.dk$Name, " ", 3)
    temp.dst.append <- NULL
    for (k in 1:nrow(temp.dk)) {
      if (temp.dst.teams[k,3] == "") {
        temp.dst.append[k] <- temp.dst.teams[k,2]
      } else {
        temp.dst.append[k] <- temp.dst.teams[k,3]
      }
    }
    temp.dk$Name <- paste0(temp.dst.append, ' ')
  }
  
  temp.dk$Name...ID <- temp.dk$Name
  temp.dk$ID <- '0000000'
  temp.dk$Name...ID <- paste0(temp.dk$Name, ' (', temp.dk$ID, ')')
  
  team.names <- read.csv(file = 'optimizationCode/data_warehouse/rotogrinders/team_names.csv', stringsAsFactors = F)
  team.names$dfn_name <- c('SEA','KC','ARI','NYJ','PHI','HOU','MIN','CIN','BUF','NE','ATL','GB','IND','NYG','BAL','TEN',
                           'TB','CHI','DAL','DET','SD','OAK','JAX','CLE','NO','MIA','SF','CAR','DEN','PIT','LA','WAS')
  team.names$dfn_name.format <- paste0('@',team.names$dfn_name)
  
  temp.dk$TeamAbbrev <- team.names$dk_name[match(temp.dk$TeamAbbrev, team.names$dfn_name)]
  temp.dk$Opp.temp <- team.names$dk_name[match(temp.dk$Opp, team.names$dfn_name)]
  temp.dk$Opp.temp2 <- paste0('@', team.names$dk_name[match(temp.dk$Opp, team.names$dfn_name.format)])
  temp.dk$Opp.temp[is.na(temp.dk$Opp.temp)] <- temp.dk$Opp.temp2[is.na(temp.dk$Opp.temp)]
  temp.dk$Opp <- temp.dk$Opp.temp
  temp.dk$Opp.temp <- NULL
  temp.dk$Opp.temp <- NULL
  
  for (j in 1:nrow(temp.dk)) {
    if(substring(temp.dk$Opp[j],1,1) == "@") {
      temp.dk$GameInfo[j] <- paste(temp.dk$TeamAbbrev[j], temp.dk$Opp[j], sep="")
    } else {
      temp.dk$GameInfo[j] <- paste(temp.dk$Opp[j],temp.dk$TeamAbbrev[j], sep="@")
    }
  }
  
  temp.dk$GameInfo <- paste0(temp.dk$GameInfo, ' 00:00PM ET')
  temp.dk$Opp <- NULL
  
  temp.dk <- temp.dk[,c('Position','Name...ID','Name','ID','Salary','GameInfo','TeamAbbrev')]
  temp.all <- rbind(temp.all, temp.dk)
}

write.csv(temp.all, file = paste0('optimizationCode/data_warehouse/draftkings/includes_thu-mon/DKSalaries_week', i, '.csv'), row.names = F)




