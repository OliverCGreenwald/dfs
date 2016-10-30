#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we test ceiling and floor projections from DFN.

####### LOAD DFN FILES #########
week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
}

####### COUNT NUMBER OF PROJECTIONS IN FLOOR-CEILING RANGE #########
# Count number of projections in floor-ceiling range
all.data <- dfn_offense_week1 # initialization purposes
all.data <- all.data[0,] # initialization purposes
for (i in 1:week.latest) {
  name <- paste("dfn_offense_week", i, sep = "")
  temp <- eval(parse(text=name))
  temp <- temp[temp$Proj.FP > 0,]
  temp$dummy <- 0
  temp[temp$Actual.FP >= temp$Floor.FP & temp$Actual.FP <= temp$Ceil.FP, 'dummy'] <- 1
  all.data <- rbind(all.data, temp)
  print(paste0('Week ', i, ' in-range %: ', 100*sum(temp$dummy)/nrow(temp)))
}

# Sort and plot
asdf <- temp[order(temp$Proj.FP, decreasing = F),]
plot(asdf$dummy, xlim = c(0,50))
hist(asdf$dummy)

jkl <- seq(from = nrow(asdf), to = 1, by = -floor((nrow(asdf)-1)/10))
jkl <- jkl[-length(jkl)]
jkl <- c(jkl,1)
jkl <- sort(jkl)
jkl
store <- rep(0,length(jkl)-1)
for (i in 1:(length(jkl)-1)) {
  store[i] <- sum(asdf[jkl[i]:jkl[i+1], 'dummy'])
}
plot(jkl[-1], store, xlab = 'Index', ylab = 'Count', main = 'Number of 1s vs Ascending Projection', xaxt = 'n')
axis(1, at = jkl[-1], las=2) # "31" is "1-31" range and "60" is "31-60" range
