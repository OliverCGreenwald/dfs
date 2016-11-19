#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we test ceiling and floor projections from DFN. Filtered out players projected to get 0 fpts. Offense only.
# Note: Need to add latest week's files to dailyfantasynerd/updates folder (redownload from DFN to get actual fpts).
# Be aware the updated data might have some differences with the original data b/c projections may have changed.

####### LOAD DFN FILES #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
  
  # add week column
  temp.df <- eval(parse(text=name))
  temp.df$Week.Num <- i
  assign(name, temp.df)
}

####### COUNT NUMBER OF PROJECTIONS IN FLOOR-CEILING RANGE #########
# Count number of projections in floor-ceiling range
all.data <- as.data.frame(matrix(NA, nrow = 0, ncol = 8)) # initialization purposes (for storing all weeks in one df)
colnames(all.data) <- c('Player.Name', 'Pos', 'Floor.FP', 'Ceil.FP', 'Proj.FP', 'Actual.FP','dummy', 'Week.Num') # initialization purposes

for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  
  temp <- eval(parse(text=name)) # temp to counts week's dfn data
  temp <- temp[temp$Proj.FP > 0,] # filter out players with projection 0
  temp$dummy <- 0
  temp[temp$Actual.FP < temp$Floor.FP, 'dummy'] <- -1 # -1: below floor
  temp[temp$Actual.FP >= temp$Floor.FP & temp$Actual.FP <= temp$Ceil.FP, 'dummy'] <- 0 # 0: in range
  temp[temp$Actual.FP > temp$Ceil.FP, 'dummy'] <- 1 # 1: above ceiling
  
  all.data <- rbind(all.data, temp[,c('Player.Name', 'Pos', 'Floor.FP', 'Ceil.FP', 'Proj.FP', 'Actual.FP','dummy', 'Week.Num')]) # append
  
  print(paste0('Week ', i, ' below floor %: ', 100*sum(temp$dummy==-1)/nrow(temp)))
  print(paste0('Week ', i, ' in-range %: ', 100*sum(temp$dummy==0)/nrow(temp)))
  print(paste0('Week ', i, ' above ceiling %: ', 100*sum(temp$dummy==1)/nrow(temp)))
  cat("\n")
}

####### ANALYZE IN-RANGE, BELOW-FLOOR, AND ABOVE-CEILING DISTRIBUTIONS #########
# Sort and plot (to determine if higher/lower projection players underperform/overperform or vice versa)
all.data <- all.data[order(all.data$Proj.FP, decreasing = F),] # sort
hist(all.data$dummy) # quick look at frequencies of below floor, in range, and above ceiling
plot(all.data$dummy) #, xlim = c(0,50)) # looks pretty uniform, hard to tell if there's a pattern

# Create histogram (bins of ascending projections, i.e. if 10 bins, then first tick mark is 0th-10th percentile bin)
num.bins <- floor(nrow(all.data)/50)
ind <- seq(from = nrow(all.data), to = 1, by = -floor((nrow(all.data)-1)/num.bins)) # indicies for bins
ind <- ind[-length(ind)]
ind <- c(ind,1)
ind <- sort(ind)

counts.in <- rep(0,length(ind)-1) # store counts in-range
counts.below <- rep(0,length(ind)-1) # store counts below floor
counts.above <- rep(0,length(ind)-1) # store counts above ceiling
for (i in 1:(length(ind)-1)) {
  counts.in[i] <- sum(all.data[ind[i]:ind[i+1], 'dummy']==0)
  counts.below[i] <- sum(all.data[ind[i]:ind[i+1], 'dummy']==-1)
  counts.above[i] <- sum(all.data[ind[i]:ind[i+1], 'dummy']==1)
}
plot(ind[-1], counts.in, xlab = 'Index (Ranges)', ylab = 'Count', main = 'Number of -1s,0s,1s vs Ascending Projection', xaxt = 'n', ylim = c(min(counts.in,counts.below,counts.above), max(counts.in,counts.below,counts.above)))
points(ind[-1], counts.below, col = "red")
points(ind[-1], counts.above, col = "green")
axis(1, at = ind[-1], las=2) # e.g. "219" is "1-219" range and "430" is "219-430" range, where these are indicies in ASCENDING order of projections


####### ANALYZE IN-RANGE, BELOW-FLOOR, AND ABOVE-CEILING DISTRIBUTIONS BY P0SITION AND WEEK #########
# Set Position (comment out other temp.data lines in this section)
# temp.data <- all.data[all.data$Pos=='WR',] # use floor(nrow(temp.data)/50) num.bins (clear green hump in middle)
temp.data <- all.data[all.data$Pos=='RB',] # use floor(nrow(temp.data)/35) num.bins (clear green hump in middle)
# temp.data <- all.data[all.data$Pos=='TE',] # use floor(nrow(temp.data)/25) num.bins
# temp.data <- all.data[all.data$Pos=='QB',] # use floor(nrow(temp.data)/15) num.bins
# temp.data <- all.data[all.data$Pos %in% c('WR','RB'),] # use floor(nrow(temp.data)/115) num.bins
# temp.data <- all.data[all.data$Pos %in% c('WR','TE'),] # use floor(nrow(temp.data)/75) num.bins
# temp.data <- all.data[all.data$Pos %in% c('WR','RB','TE'),] # use floor(nrow(temp.data)/125) num.bins

# Set Week(s) (comment out other temp.data lines in this section)
temp.data <- temp.data[temp.data$Week.Num %in% c((week.latest-6):week.latest),]

# Sort and plot (to determine if higher/lower projection players underperform/overperform or vice versa)
temp.data <- temp.data[order(temp.data$Proj.FP, decreasing = F),] # sort
hist(temp.data$dummy) # quick look at frequencies of below floor, in range, and above ceiling
plot(temp.data$dummy) #, xlim = c(0,50)) # looks pretty uniform, hard to tell if there's a pattern

# Create histogram (bins of ascending projections, i.e. if 10 bins, then first tick mark is 0th-10th percentile bin)
num.bins <- floor(nrow(temp.data)/38)
ind <- seq(from = nrow(temp.data), to = 1, by = -floor((nrow(temp.data)-1)/num.bins)) # indicies for bins
ind <- ind[-length(ind)]
ind <- c(ind,1)
ind <- sort(ind)

counts.in <- rep(0,length(ind)-1) # store counts in-range
counts.below <- rep(0,length(ind)-1) # store counts below floor
counts.above <- rep(0,length(ind)-1) # store counts above ceiling
for (i in 1:(length(ind)-1)) {
  counts.in[i] <- sum(temp.data[ind[i]:ind[i+1], 'dummy']==0)
  counts.below[i] <- sum(temp.data[ind[i]:ind[i+1], 'dummy']==-1)
  counts.above[i] <- sum(temp.data[ind[i]:ind[i+1], 'dummy']==1)
}
plot(ind[-1], counts.in, xlab = 'Index (Ranges)', ylab = 'Count', main = 'Number of -1s,0s,1s vs Ascending Projection', xaxt = 'n', ylim = c(min(counts.in,counts.below,counts.above), max(counts.in,counts.below,counts.above)))
points(ind[-1], counts.below, col = "red")
points(ind[-1], counts.above, col = "green")
axis(1, at = ind[-1], las=2)


####### ADJUST RB AND WR PROJECTIONS AND APPEND TO 2016_CLEANED_INPUT FILES #########
for (i in 1:week.latest) {
  name <- paste("offensive_players_wk", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), stringsAsFactors = F))
  temp.df1 <- eval(parse(text=name))
  
  name <- paste("dfn_offense_week", i, sep = "")
  temp.df2 <- eval(parse(text=name))
  temp.df2$Player.Name <- sub(' Sr.', '', temp.df2$Player.Name)
  temp.df2$Player.Name <- sub(' Jr.', '', temp.df2$Player.Name)
  
  # Adding new columns
  temp.df1$Projection_ceil <- temp.df2$Ceil.FP[match(temp.df1$Name, temp.df2$Player.Name)]
  temp.df1$Projection_rceil <- temp.df1$Projection_dfn # temp
  
  # RBs.
  # After week 10, using temp.data <- temp.data[temp.data$Week.Num %in% c((week.latest-6):week.latest),] and
  # num.bins <- floor(nrow(temp.data)/30), we notice that for bins 317-348, 348-379, 379-410 have a lot more move-ceiling than below-floor
  # This range is in the 317/534 = 59.36 percentile to the 410/534 = 76.78 percentile.
  # This corresponds to temp.data[317,'Proj.FP']=9.3 and temp.data[410,'Proj.FP']=12.3.
  # So, we will randomly set 50% of these RBs to their ceiling.
  # NOTE: there's some lookahead bias here
  temp.lo <- 9.3
  temp.hi <- 14.0
  temp.num <- nrow(temp.df1[temp.df1$Position=='RB' & temp.df1$Projection_dfn<=temp.hi & temp.df1$Projection_dfn>=temp.lo,])
  temp.inds <- sample(x = 1:temp.num, size = floor(temp.num/2))
  for (j in temp.inds) {
    temp.df1[temp.df1$Position=='RB' & temp.df1$Projection_dfn<=temp.hi & temp.df1$Projection_dfn>=temp.lo,]$Projection_rceil[j] <- temp.df1[temp.df1$Position=='RB' & temp.df1$Projection_dfn<=temp.hi & temp.df1$Projection_dfn>=temp.lo,]$Projection_ceil[j]
  }
  
  print(temp.num)
  print(sum(is.na(temp.df1$Projection_rceil))) # there some NAs. TODO: fix this shit
  
  # Write to file
  write.csv(temp.df1, file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i,'/offensive_players.csv'), row.names = F)
}

# asdf <- all.data[all.data$Pos=='RB',]
# for (i in 1:week.latest) {
#   asdf1 <- asdf[asdf$Week.Num==i,]
#   hist(asdf1$Proj.FP)  
# }