#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #######
# In this file we examine whether there is a correlation between DST salary and DST fpts (projected
# and actual). We suspect that there isn't much of a correlation.
# Confirmed our suspicion. There's negative correlation when we subset DST salary >= $3100!


####### SET PARAMETERS #######
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1


####### LOAD DFN DEFENSE FILES #######
def.all <- NULL
for (i in 1:week.latest) {
  assign(paste0("dfn_defense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_defense_week', i, ".csv"), stringsAsFactors = F))
  temp <- eval(parse(text=paste0("dfn_defense_week", i)))
  temp$Week.Num <- i
  def.all <- rbind(def.all, temp)
}

####### ANALYZE CORRELATION #######
# Plot
plot(def.all$Salary, def.all$Proj.FP)
plot(def.all$Salary, def.all$Actual.FP)

# Correlation
cor(def.all$Salary, def.all$Actual.FP)

# Correlation on subsetted data by salary
for (i in seq(from = 3300, to = 2700, by = -100)) {
  print(paste0("Salary ", i, ": ", cor(def.all$Salary[def.all$Salary >= i], def.all$Actual.FP[def.all$Salary >= i])))
  plot(def.all$Salary[def.all$Salary >= i], def.all$Actual.FP[def.all$Salary >= i])  
}

# cor(def.all$Salary[def.all$Salary >= 3300], def.all$Actual.FP[def.all$Salary >= 3300])
# plot(def.all$Salary[def.all$Salary >= 3300], def.all$Actual.FP[def.all$Salary >= 3300])
# 
# cor(def.all$Salary[def.all$Salary >= 3200], def.all$Actual.FP[def.all$Salary >= 3200])
# plot(def.all$Salary[def.all$Salary >= 3200], def.all$Actual.FP[def.all$Salary >= 3200])
# 
# cor(def.all$Salary[def.all$Salary >= 3100], def.all$Actual.FP[def.all$Salary >= 3100])
# plot(def.all$Salary[def.all$Salary >= 3100], def.all$Actual.FP[def.all$Salary >= 3100])
# 
# cor(def.all$Salary[def.all$Salary >= 3000], def.all$Actual.FP[def.all$Salary >= 3000])
# plot(def.all$Salary[def.all$Salary >= 3000], def.all$Actual.FP[def.all$Salary >= 3000])
# 
# cor(def.all$Salary[def.all$Salary >= 2900], def.all$Actual.FP[def.all$Salary >= 2900])
# plot(def.all$Salary[def.all$Salary >= 2900], def.all$Actual.FP[def.all$Salary >= 2900])
# 
# cor(def.all$Salary[def.all$Salary >= 2800], def.all$Actual.FP[def.all$Salary >= 2800])
# plot(def.all$Salary[def.all$Salary >= 2800], def.all$Actual.FP[def.all$Salary >= 2800])
# 
# cor(def.all$Salary[def.all$Salary >= 2700], def.all$Actual.FP[def.all$Salary >= 2700])
# plot(def.all$Salary[def.all$Salary >= 2700], def.all$Actual.FP[def.all$Salary >= 2700])
# 
# cor(def.all$Salary[def.all$Salary >= 2600], def.all$Actual.FP[def.all$Salary >= 2600])
# plot(def.all$Salary[def.all$Salary >= 2600], def.all$Actual.FP[def.all$Salary >= 2600])

cor(def.all$Salary[def.all$Salary >= 2900 & def.all$Salary <= 3300], def.all$Actual.FP[def.all$Salary >= 2900 & def.all$Salary <= 3300])
plot(def.all$Salary[def.all$Salary >= 2900 & def.all$Salary <= 3300], def.all$Actual.FP[def.all$Salary >= 2900 & def.all$Salary <= 3300])

# Check projection vs actual
plot(def.all$Proj.FP, def.all$Actual.FP)
cor(def.all$Proj.FP, def.all$Actual.FP)

####### COMPARE TO CORRELATIONS FOR OTHER POSITIONS #######
# Load DFN offense files
off.all <- NULL
for (i in 1:week.latest) {
  assign(paste0("dfn_offense_week", i), read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, ".csv"), stringsAsFactors = F))
  temp <- eval(parse(text=paste0("dfn_offense_week", i)))
  temp$Week.Num <- i
  temp <- temp[, c('Player.Name','Salary','Pos','Proj.FP','Actual.FP')]
  off.all <- rbind(off.all, temp)
}

# Correlation QB
cor(off.all$Salary[off.all$Pos=="QB"], off.all$Actual.FP[off.all$Pos=="QB"])
plot(off.all$Salary[off.all$Pos=="QB"], off.all$Actual.FP[off.all$Pos=="QB"])

cor(off.all$Proj.FP[off.all$Pos=="QB"], off.all$Actual.FP[off.all$Pos=="QB"])
plot(off.all$Proj.FP[off.all$Pos=="QB"], off.all$Actual.FP[off.all$Pos=="QB"])

# Correlation RB
cor(off.all$Salary[off.all$Pos=="RB"], off.all$Actual.FP[off.all$Pos=="RB"])
plot(off.all$Salary[off.all$Pos=="RB"], off.all$Actual.FP[off.all$Pos=="RB"])

cor(off.all$Proj.FP[off.all$Pos=="RB"], off.all$Actual.FP[off.all$Pos=="RB"])
plot(off.all$Proj.FP[off.all$Pos=="RB"], off.all$Actual.FP[off.all$Pos=="RB"])

# Correlation WR
cor(off.all$Salary[off.all$Pos=="WR"], off.all$Actual.FP[off.all$Pos=="WR"])
plot(off.all$Salary[off.all$Pos=="WR"], off.all$Actual.FP[off.all$Pos=="WR"])

cor(off.all$Proj.FP[off.all$Pos=="WR"], off.all$Actual.FP[off.all$Pos=="WR"])
plot(off.all$Proj.FP[off.all$Pos=="WR"], off.all$Actual.FP[off.all$Pos=="WR"])

# Correlation TE
cor(off.all$Salary[off.all$Pos=="TE"], off.all$Actual.FP[off.all$Pos=="TE"])
plot(off.all$Salary[off.all$Pos=="TE"], off.all$Actual.FP[off.all$Pos=="TE"])

cor(off.all$Proj.FP[off.all$Pos=="TE"], off.all$Actual.FP[off.all$Pos=="TE"])
plot(off.all$Proj.FP[off.all$Pos=="TE"], off.all$Actual.FP[off.all$Pos=="TE"])

