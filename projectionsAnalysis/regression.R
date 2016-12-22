#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we run a regression on Rotogrinders and Daily Fantasy Nerd projections to output combined predictions.
# Filtered out players projected to get 0 fpts. Offense only.
# We also add floor, ceiling, and actual fpts to the 2016_cleaned_input (offense and defense) and 2016_cleaned_input/all_data (just offense) folders.

####### LOAD DFN FILES #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:(week.latest+1)) {
  #--- offense ---#
  name <- paste("dfn_offense_week", i, sep = "")
  if (i == (week.latest+1)) {
    assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_desfense_week', i, '.csv'), stringsAsFactors = F))
  } else {
    assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
  }
  
  # add week column
  temp.df <- eval(parse(text=name))
  temp.df$Week.Num <- i
  assign(name, temp.df)
  
  #--- defense (only used for appending Actual to 2016_cleaned_input) ---#
  name <- paste("dfn_defense_week", i, sep = "")
  if (i == (week.latest+1)) {
    assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
  } else {
    assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week', i, '.csv'), stringsAsFactors = F))
  }
}


####### ADD FLOOR, CEIL, ACTUAL (OFF AND DEF) TO 2016_CLEANED_INPUT/ALL_DATA FILES (only run after current week's data is prepared) #########
# for (i in 2:week.latest) {
  i <- week.latest + 1
  
  #--- 2016_cleaned_input/all_data ---#
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), stringsAsFactors = F)
  temp.dfn <- eval(parse(text=paste0("dfn_offense_week", i)))
  
  # add Projection_dfn_floor, Projection_dfn_ceiling, Actual
  temp$Projection_dfn_floor <- temp.dfn$Floor.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]
  temp$Projection_dfn_ceiling <- temp.dfn$Ceil.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]
  temp$Actual <- temp.dfn$Actual.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]
  
  # write to file
  write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/all_data/wk', i, '/offensive_players.csv'), row.names = F)
  
  #--- 2016_cleaned_input (offense) ---#
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), stringsAsFactors = F)
  temp.dfn <- eval(parse(text=paste0("dfn_offense_week", i)))
  
  # add Actual
  temp$Actual <- temp.dfn$Actual.FP[match(paste0(temp$Name,temp$Position), paste0(temp.dfn$Player.Name,temp.dfn$Pos))]
  temp$Actual[is.na(temp$Actual)==T] <- 0 # replace NA's with 0's b/c can't run julia with NA's
  
  # write to file
  write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), row.names = F)
  
  #--- 2016_cleaned_input (defense) ---#
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/defenses.csv'), stringsAsFactors = F)
  temp.dfn <- eval(parse(text=paste0("dfn_defense_week", i)))
  
  # name cleaning
  temp$Team <- toupper(temp$Team)
  
  # add Actual
  temp$Actual <- temp.dfn$Actual.FP[match(temp$Team, temp.dfn$Team)]
  # temp$Actual[is.na(temp$Actual)==T] <- 0 # replace NA's with 0's b/c can't run julia with NA's (catch NAs)
  
  # write to file
  write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/defenses.csv'), row.names = F)
# }


####### AGGREGATE DATA #########
all.data <- as.data.frame(matrix(NA, nrow = 0, ncol = 7)) # initialization purposes (for storing all weeks in one df)
colnames(all.data) <- c('Player.Name', 'Floor.FP', 'Ceil.FP', 'Proj.FP', 'Actual.FP', 'Week.Num', 'Roto.Pred') # initialization purposes

for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  
  temp <- eval(parse(text=name)) # temp to counts week's dfn data
  temp <- temp[temp$Proj.FP > 0,] # filter out players with projection 0
  
  temp.roto <- read.csv(file = paste0('optimizationCode/data_warehouse/rotogrinders/roto_offense_week',i,'.csv'), stringsAsFactors = F)
  temp$Player.Name <- sub(' Sr.', '', temp$Player.Name)
  temp$Player.Name <- sub(' Jr.', '', temp$Player.Name)
  temp$Roto.Pred <- temp.roto$fpts[match(temp$Player.Name, temp.roto$player)]
  
  all.data <- rbind(all.data, temp[,c('Player.Name', 'Floor.FP', 'Ceil.FP', 'Proj.FP', 'Actual.FP', 'Week.Num', 'Roto.Pred')]) # append
}


####### RUN REGRESSIONS (ROLLING) FOR EACH WEEK #########
threshold.pct <- 0.70 #0.70
# threshold.pcts <- seq(from = 0.5, to = 0.9, by = 0.05)
# threshold.tuning.mat <- as.data.frame(matrix(NA, nrow = length(threshold.pcts), ncol = week.latest*2))
# for (i in 1:ncol(threshold.tuning.mat)) {
#   colnames(threshold.tuning.mat)[i] <- paste0('')
# }

coeff.all <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 5)) # for storing coefficents and p-values
coeff.upper <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 5)) # for storing coefficents and p-values
coeff.lower <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 5)) # for storing coefficents and p-values
thresholds <- as.data.frame(matrix(NA, nrow = week.latest, ncol = 1)) # for storing thresholds

colnames(coeff.all) <- c('Week','Proj.FP', 'Roto.Pred', 'Proj.FP.pval', 'Roto.Pred.pval')
colnames(coeff.upper) <- c('Week','Proj.FP', 'Roto.Pred', 'Proj.FP.pval', 'Roto.Pred.pval')
colnames(coeff.lower) <- c('Week','Proj.FP', 'Roto.Pred', 'Proj.FP.pval', 'Roto.Pred.pval')
colnames(thresholds) <- 'Threshold.Fpts'

for (i in 1:week.latest) {
  temp <- all.data[all.data$Week.Num <= i, ] # & all.data$Week.Num >= i-4 # if more than 5 weeks, use previous 5 weeks worth of data (rather than all prev weeks)
  model <- lm(Actual.FP ~ Proj.FP + Roto.Pred, data = temp, na.action = na.omit)
  # print(paste0("Week ", i, " (all)"))
  # print(summary(model))
  # cat("\n")
  coeff.all[i,1] <- i
  coeff.all[i,2] <- summary(model)$coefficients[2]
  coeff.all[i,3] <- summary(model)$coefficients[3]
  coeff.all[i,4] <- summary(model)$coefficients[11]
  coeff.all[i,5] <- summary(model)$coefficients[12]
  
  threshold <- quantile(temp$Actual.FP, na.rm = T, probs = threshold.pct) # we set this to threshold.pct percentile
  thresholds[i,1] <- threshold
  
  if (i > 1) {
    temp.upper <- temp[(temp$Proj.FP + temp$Roto.Pred)/2 >= thresholds[i-1,1], ] # only keep players where average > last week's threshold
    model <- lm(Actual.FP ~ Proj.FP + Roto.Pred, data = temp.upper, na.action = na.omit)
    print(paste0("Week ", i, " (above ", threshold.pct*100, "th pct: ", threshold, ")"))
    print(summary(model))
    cat("\n")
    coeff.upper[i,1] <- i
    coeff.upper[i,2] <- summary(model)$coefficients[2]
    coeff.upper[i,3] <- summary(model)$coefficients[3]
    coeff.upper[i,4] <- summary(model)$coefficients[11]
    coeff.upper[i,5] <- summary(model)$coefficients[12]
    
    temp.lower <- temp[(temp$Proj.FP + temp$Roto.Pred)/2 < thresholds[i-1,1], ] # only keep players where actual >= threshold
    model <- lm(Actual.FP ~ Proj.FP + Roto.Pred, data = temp.lower, na.action = na.omit)
    print(paste0("Week ", i, " (below ", threshold.pct*100, "th pct: ", threshold, ")"))
    print(summary(model))
    cat("\n")
    coeff.lower[i,1] <- i
    coeff.lower[i,2] <- summary(model)$coefficients[2]
    coeff.lower[i,3] <- summary(model)$coefficients[3]
    coeff.lower[i,4] <- summary(model)$coefficients[11]
    coeff.lower[i,5] <- summary(model)$coefficients[12]
  }
}

####### ADD REGRESSED PREDICTIONS TO 2016_CLEANED_INPUT FILES #########
# for (i in 2:week.latest) { # change to week.latest+1 once current week's data has been scraped
  i <- week.latest + 1
  
  temp <- read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), stringsAsFactors = F)
  
  # Option 1: Use single regression model (if using this, comment out Option 2)
  temp$Projection_reg <- coeff.all[i-1,'Proj.FP']*temp$Projection_dfn + coeff.all[i-1,'Roto.Pred']*temp$Projection
  
  # Option 2: Use two regression models (split at threshold) (if using this, comment out Option 1)
  for (j in 1:nrow(temp)) {
    if ((temp$Projection_dfn[j] + temp$Projection[j])/2 > thresholds[i-1,1]) {
      temp$Projection_reg_split[j] <- coeff.upper[i-1,'Proj.FP']*temp$Projection_dfn[j] + coeff.upper[i-1,'Roto.Pred']*temp$Projection[j]
    } else {
      temp$Projection_reg_split[j] <- coeff.upper[i-1,'Proj.FP']*temp$Projection_dfn[j] + coeff.upper[i-1,'Roto.Pred']*temp$Projection[j]
    }
  }
  
  write.csv(temp, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), row.names = F)
# }


