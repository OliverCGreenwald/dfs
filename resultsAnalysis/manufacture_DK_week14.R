setwd("~/Projects/DFS/")
library(stringr)


dk <- read.csv(file = 'optimizationCode/data_warehouse/draftkings/DKSalaries_week13.csv', stringsAsFactors = F)

dfn_offense <- read.csv(file = 'optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week14.csv', stringsAsFactors = F)
dfn_defense <- read.csv(file = 'optimizationCode/data_warehouse/dailyfantasynerd/dfn_defense_week14.csv', stringsAsFactors = F)


temp_offense <- dfn_offense[,c(4,1,5,6)]
temp_dk <- dk[,c(3,4,2)]

for (i in 1:nrow(dfn_offense)) {
  if(substring(dfn_offense$Opp[i],1,1) == "@") {
    temp_offense$GameInfo[i] <- paste(dfn_offense$Team[i], dfn_offense$Opp[i], sep="")
  } else {
    temp_offense$GameInfo[i] <- paste(dfn_offense$Opp[i],dfn_offense$Team[i],  sep="@")
  }
}

temp_offense$Player.Name[8] <- "Odell Beckham Jr."
temp_offense$Player.Name[163] <- "Marvin Jones Jr."
temp_offense$Player.Name[196] <- "Ted Ginn Jr."
temp_offense$Player.Name[72] <- "Steve Smith Sr."



colnames(temp_offense)[2] <- "Name"

temp_offense <- merge(temp_offense, temp_dk, by = "Name")


temp_offense <- temp_offense[,c(2,7,1,6, 3,5,4)]
colnames(temp_offense)[1] <- "Position"
colnames(temp_offense)[7] <- "TeamAbbrev"

#temp_offense$TeamAbbrev <- paste(substr(temp_offense$TeamAbbrev,1,1), tolower(substr(temp_offense$TeamAbbrev,2,nchar(temp_offense$TeamAbbrev))), sep = "")

#Clean Defense 

temp_defense <- dfn_defense[,c(3,1,4,5,6)]
colnames(temp_defense) <- c("Position", "Name","Salary","Team", "Opp")
temp_defense$Position <- "DST"

temp_defense$Name <- paste(word(temp_defense$Name,-1), " ", sep="") 
temp_defense <- merge(temp_defense, temp_dk, by = "Name")

for (i in 1:nrow(temp_defense)) {
  if(substring(temp_defense$Opp[i],1,1) == "@") {
    temp_defense$GameInfo[i] <- paste(temp_defense$Team[i], temp_defense$Opp[i], sep="")
  } else {
    temp_defense$GameInfo[i] <- paste(temp_defense$Opp[i],temp_defense$Team[i],  sep="@")
  }
}

temp_defense <- temp_defense[,c(2,7,1,6,3,8,4)]
colnames(temp_defense)[7] <- "TeamAbbrev"

cleaned <- rbind(temp_offense, temp_defense)

cleaned <- cleaned[-c(120,119, 343,344),]
cleaned <- cleaned[cleaned$GameInfo != "OAK@KC",]
cleaned <- cleaned[cleaned$GameInfo != "BAL@NE",]
cleaned <- cleaned[order(-cleaned$Salary),] 

write.csv(cleaned, file = 'optimizationCode/data_warehouse/draftkings/DKSalaries_week14.csv', row.names  = F, quote = FALSE)
