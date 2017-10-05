#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### Description #########
# In this file we run a regression on Rotogrinders and Daily Fantasy Nerd projections to output combined predictions.
# Split into defense and offense.

####### Offense #########
####### Week 4 #########
fpts.realized.week4 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week4.csv', stringsAsFactors = F)
# fpts.realized.week4$Actual.Score[is.na(fpts.realized.week4$Actual.Score)] <- 0 # fix NA's
fpts.realized.week4$Player <- sub(' Sr.', '', fpts.realized.week4$Player) # remove Sr.
fpts.realized.week4$Player <- sub(' Jr.', '', fpts.realized.week4$Player) # remove Jr.

#--- offense ---#
# prepare matrix for regression
fpts.roto.offense.week4 <- read.csv(file = 'optimizationCode/data_warehouse/rotogrinders/roto_offense_week4.csv', stringsAsFactors = F)
fpts.dfn.offense.week4 <- read.csv(file = 'optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week4.csv', stringsAsFactors = F)
fpts.dfn.offense.week4$Player.Name <- sub(' Sr.', '', fpts.dfn.offense.week4$Player.Name) # remove Sr.
fpts.dfn.offense.week4$Player.Name <- sub(' Jr.', '', fpts.dfn.offense.week4$Player.Name) # remove Jr.

fpts.realized.week4$RotoPreds <- fpts.roto.offense.week4$fpts[match(fpts.realized.week4$Player, fpts.roto.offense.week4$player)]
fpts.realized.week4$DfnPreds <- fpts.dfn.offense.week4$Proj.FP[match(fpts.realized.week4$Player, fpts.dfn.offense.week4$Player.Name)]

df.week4 <- as.data.frame(cbind(fpts.realized.week4$Actual.Score, fpts.realized.week4$RotoPreds, fpts.realized.week4$DfnPreds), stringsAsFactors = F)
colnames(df.week4) <- c('Realized_fpts', 'Rotogrinders_ftps', 'Dfn_ftps')

# regression
model.week4 <- lm(df.week4$Realized_fpts ~ df.week4$Rotogrinders_ftps + df.week4$Dfn_ftps, na.action = na.omit)
summary(model.week4)

fpts.50th.percentile <- quantile(df.week4$Realized_fpts, na.rm = T)[3]
df.week4.cut <- df.week4[df.week4$Realized_fpts >= fpts.50th.percentile,]
model.week4.cut <- lm(df.week4.cut$Realized_fpts ~ df.week4.cut$Rotogrinders_ftps + df.week4.cut$Dfn_ftps, na.action = na.omit)
summary(model.week4.cut)

####### Week 3 #########
fpts.realized.week3 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week3.csv', stringsAsFactors = F)
# fpts.realized.week3$Actual.Score[is.na(fpts.realized.week3$Actual.Score)] <- 0 # fix NA's
fpts.realized.week3$Player <- sub(' Sr.', '', fpts.realized.week3$Player) # remove Sr.
fpts.realized.week3$Player <- sub(' Jr.', '', fpts.realized.week3$Player) # remove Jr.

#--- offense ---#
# prepare matrix for regression
fpts.roto.offense.week3 <- read.csv(file = 'optimizationCode/data_warehouse/rotogrinders/roto_offense_week3.csv', stringsAsFactors = F)
fpts.dfn.offense.week3 <- read.csv(file = 'optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week3.csv', stringsAsFactors = F)
fpts.dfn.offense.week3$Player.Name <- sub(' Sr.', '', fpts.dfn.offense.week3$Player.Name) # remove Sr.
fpts.dfn.offense.week3$Player.Name <- sub(' Jr.', '', fpts.dfn.offense.week3$Player.Name) # remove Jr.

fpts.realized.week3$RotoPreds <- fpts.roto.offense.week3$fpts[match(fpts.realized.week3$Player, fpts.roto.offense.week3$player)]
fpts.realized.week3$DfnPreds <- fpts.dfn.offense.week3$Proj.FP[match(fpts.realized.week3$Player, fpts.dfn.offense.week3$Player.Name)]

df.week3 <- as.data.frame(cbind(fpts.realized.week3$Actual.Score, fpts.realized.week3$RotoPreds, fpts.realized.week3$DfnPreds), stringsAsFactors = F)
colnames(df.week3) <- c('Realized_fpts', 'Rotogrinders_ftps', 'Dfn_ftps')

# regression
model.week3 <- lm(df.week3$Realized_fpts ~ df.week3$Rotogrinders_ftps + df.week3$Dfn_ftps, na.action = na.omit)
summary(model.week3)

fpts.50th.percentile <- quantile(df.week3$Realized_fpts, na.rm = T)[3]
df.week3.cut <- df.week3[df.week3$Realized_fpts >= fpts.50th.percentile,]
model.week3.cut <- lm(df.week3.cut$Realized_fpts ~ df.week3.cut$Rotogrinders_ftps + df.week3.cut$Dfn_ftps, na.action = na.omit)
summary(model.week3.cut)

####### Week 2 #########
fpts.realized.week2 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week2.csv', stringsAsFactors = F)
# fpts.realized.week2$Actual.Score[is.na(fpts.realized.week2$Actual.Score)] <- 0 # fix NA's
fpts.realized.week2$Player <- sub(' Sr.', '', fpts.realized.week2$Player) # remove Sr.
fpts.realized.week2$Player <- sub(' Jr.', '', fpts.realized.week2$Player) # remove Jr.

#--- offense ---#
# prepare matrix for regression
fpts.roto.offense.week2 <- read.csv(file = 'optimizationCode/data_warehouse/rotogrinders/roto_offense_week2.csv', stringsAsFactors = F)
fpts.dfn.offense.week2 <- read.csv(file = 'optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week2.csv', stringsAsFactors = F)
fpts.dfn.offense.week2$Player.Name <- sub(' Sr.', '', fpts.dfn.offense.week2$Player.Name) # remove Sr.
fpts.dfn.offense.week2$Player.Name <- sub(' Jr.', '', fpts.dfn.offense.week2$Player.Name) # remove Jr.

fpts.realized.week2$RotoPreds <- fpts.roto.offense.week2$fpts[match(fpts.realized.week2$Player, fpts.roto.offense.week2$player)]
fpts.realized.week2$DfnPreds <- fpts.dfn.offense.week2$Proj.FP[match(fpts.realized.week2$Player, fpts.dfn.offense.week2$Player.Name)]

df.week2 <- as.data.frame(cbind(fpts.realized.week2$Actual.Score, fpts.realized.week2$RotoPreds, fpts.realized.week2$DfnPreds), stringsAsFactors = F)
colnames(df.week2) <- c('Realized_fpts', 'Rotogrinders_ftps', 'Dfn_ftps')

# regression
model.week2 <- lm(df.week2$Realized_fpts ~ df.week2$Rotogrinders_ftps + df.week2$Dfn_ftps, na.action = na.omit)
summary(model.week2)

fpts.50th.percentile <- quantile(df.week2$Realized_fpts, na.rm = T)[3]
df.week2.cut <- df.week2[df.week2$Realized_fpts >= fpts.50th.percentile,]
model.week2.cut <- lm(df.week2.cut$Realized_fpts ~ df.week2.cut$Rotogrinders_ftps + df.week2.cut$Dfn_ftps, na.action = na.omit)
summary(model.week2.cut)

####### Week 1 #########
fpts.realized.week1 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week1.csv', stringsAsFactors = F)
# fpts.realized.week1$Actual.Score[is.na(fpts.realized.week1$Actual.Score)] <- 0 # fix NA's
fpts.realized.week1$Player <- sub(' Sr.', '', fpts.realized.week1$Player) # remove Sr.
fpts.realized.week1$Player <- sub(' Jr.', '', fpts.realized.week1$Player) # remove Jr.

#--- offense ---#
# prepare matrix for regression
fpts.roto.offense.week1 <- read.csv(file = 'optimizationCode/data_warehouse/rotogrinders/roto_offense_week1.csv', stringsAsFactors = F)
fpts.dfn.offense.week1 <- read.csv(file = 'optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week1.csv', stringsAsFactors = F)
fpts.dfn.offense.week1$Player.Name <- sub(' Sr.', '', fpts.dfn.offense.week1$Player.Name) # remove Sr.
fpts.dfn.offense.week1$Player.Name <- sub(' Jr.', '', fpts.dfn.offense.week1$Player.Name) # remove Jr.

fpts.realized.week1$RotoPreds <- fpts.roto.offense.week1$fpts[match(fpts.realized.week1$Player, fpts.roto.offense.week1$player)]
fpts.realized.week1$DfnPreds <- fpts.dfn.offense.week1$Proj.FP[match(fpts.realized.week1$Player, fpts.dfn.offense.week1$Player.Name)]

df.week1 <- as.data.frame(cbind(fpts.realized.week1$Actual.Score, fpts.realized.week1$RotoPreds, fpts.realized.week1$DfnPreds), stringsAsFactors = F)
colnames(df.week1) <- c('Realized_fpts', 'Rotogrinders_ftps', 'Dfn_ftps')

# regression
model.week1 <- lm(df.week1$Realized_fpts ~ df.week1$Rotogrinders_ftps + df.week1$Dfn_ftps, na.action = na.omit)
summary(model.week1)

fpts.50th.percentile <- quantile(df.week1$Realized_fpts, na.rm = T)[3]
df.week1.cut <- df.week1[df.week1$Realized_fpts >= fpts.50th.percentile,]
model.week1.cut <- lm(df.week1.cut$Realized_fpts ~ df.week1.cut$Rotogrinders_ftps + df.week1.cut$Dfn_ftps, na.action = na.omit)
summary(model.week1.cut)

####### Combined Weeks 1-4 #########
df <- rbind(df.week1, df.week2, df.week3, df.week4)
model <- lm(df$Realized_fpts ~ df$Rotogrinders_ftps + df$Dfn_ftps, na.action = na.omit)
summary(model)

fpts.50th.percentile <- quantile(df$Realized_fpts, na.rm = T)[3]
df.cut <- df[df$Realized_fpts >= fpts.50th.percentile,]
model.cut <- lm(df.cut$Realized_fpts ~ df.cut$Rotogrinders_ftps + df.cut$Dfn_ftps, na.action = na.omit)
summary(model.cut)

####### Defense #########
#fpts.roto.defense.week1 <- read.csv(file = 'optimizationCode/data_warehouse/rotogrinders/roto_defense_week4.csv', stringsAsFactors = F)

