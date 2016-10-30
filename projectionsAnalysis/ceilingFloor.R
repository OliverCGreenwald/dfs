#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we test ceiling and floor projections from DFN.

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
all.data <- as.data.frame(matrix(NA, nrow = 0, ncol = 7)) # initialization purposes (for storing all weeks in one df)
colnames(all.data) <- c('Player.Name', 'Floor.FP', 'Ceil.FP', 'Proj.FP', 'Actual.FP','dummy', 'Week.Num') # initialization purposes

for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  
  temp <- eval(parse(text=name)) # temp to counts week's dfn data
  temp <- temp[temp$Proj.FP > 0,]
  temp$dummy <- 0
  temp[temp$Actual.FP < temp$Floor.FP, 'dummy'] <- -1 # -1: below floor
  temp[temp$Actual.FP >= temp$Floor.FP & temp$Actual.FP <= temp$Ceil.FP, 'dummy'] <- 0 # 0: in range
  temp[temp$Actual.FP > temp$Ceil.FP, 'dummy'] <- 1 # 1: above ceiling
  
  all.data <- rbind(all.data, temp[,c('Player.Name', 'Floor.FP', 'Ceil.FP', 'Proj.FP', 'Actual.FP','dummy', 'Week.Num')]) # append
  
  print(paste0('Week ', i, ' below floor %: ', 100*sum(temp$dummy==-1)/nrow(temp)))
  print(paste0('Week ', i, ' in-range %: ', 100*sum(temp$dummy==0)/nrow(temp)))
  print(paste0('Week ', i, ' above ceiling %: ', 100*sum(temp$dummy==1)/nrow(temp)))
}

# Sort and plot (to determine if higher/lower projection players underperform/overperform or vice versa)
all.data <- all.data[order(all.data$Proj.FP, decreasing = F),] # sort
hist(all.data$dummy) # quick look at frequencies of below floor, in range, and above ceiling
plot(all.data$dummy) #, xlim = c(0,50)) # looks pretty uniform, hard to tell if there's a pattern

# Create histogram (bins of ascending projections, i.e. if 10 bins, then first tick mark is 0th-10th percentile bin)
num.bins <- 25
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
points(ind[-1], counts.above, col = "blue")
axis(1, at = ind[-1], las=2) # e.g. "219" is "1-219" range and "430" is "219-430" range, where these are indicies in ascending order of projections

