######### Instructions #########

# After running, view Results using the following commands: 
# View(PnL[,c(1,2,6,7)])
# View(PnL$Lineups[[NUMBER]])
# View(aggregated_PnL)


#########   Variables   #########

# the Row number that corresponds to the desired contest in 'contest_info' 
lineup_name <- "*covar_chg75p_exp*" # "*covar_arima_p3d1q2*"
######### Code Begins #########
#install.packages("data.table")
library(data.table)
library(dplyr)
library(ggplot2)

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}

### Save DFS Directory Path

original_wd <- getwd()

# Load in Helper Files
setwd('MLB/resultsAnalysis/helperFunctions')
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)

# Return to base directory
setwd(original_wd)

### Read in Contest File (Not necessary but useful to look at to find correct row)
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)
contest_baseline <- read.csv('MLB/optimizationCode/baseline_contests.csv', stringsAsFactors = F)
contest_row_index <- contest_baseline$contest_row_index

PnL <- data.frame(Name=character(),
                 PnL=numeric(), 
                 Lineups=list(), 
                 contest=character(),
                 Date=as.Date(character()),
                 stringsAsFactors=FALSE) 

for(contest_row in contest_row_index) {
  temp <- singleContest_manyLineups_PnL_comparison(contest_row, lineup_name)
  temp$contest <- contest_info$Contest_Name[contest_row]
  temp$Date <- as.Date(contest_info$Contest_Date)[contest_row]
  PnL <- rbind(PnL,temp)
}

aggregated_PnL <- aggregate(PnL$PnL, by= list(PnL$Name), sum)

####### GRAPHS CUMSUM #####

final_data <- as.data.table(PnL[,c(1,2,6,7)])

final_data$Date <- as.Date(final_data$Date, "%m/%d/%Y")

# use data table to aggregate on months 
# First lets add a field plot date with Year and Month YYYYMM 201401
final_data[, PlotDate := as.numeric(format(Date, "%Y%m%d"))] 

# key by this plot date
setkeyv(final_data, "PlotDate")

# second we aggregate with by , and label columns
plotdata <- final_data[, .(PnL  = cumsum(PnL)), by = list(PlotDate, Name)]
plotdata = plotdata[order(plotdata$PlotDate), ]
plotdata <- mutate(group_by(plotdata,Name), cumsum=cumsum(PnL))

ggplot(data=plotdata,
       aes(x=as.Date(as.character(PlotDate),'%Y%m%d'), y=cumsum, colour=Name)) +
  geom_line() + theme(legend.position="none")



# view a particular formulation
inds_form <- which(PnL$Name=="formulations.formulation5_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_P0.8_exposure_B10.5_exposure_B20.4_exposure_B30.6_exposure_C0.5_exposure_SS0.5_exposure_OF0.6_covar_chg75p_exp(spike).csv")
inds_form <- which(PnL$Name=="formulations.formulation3_covar_stacksize_5_overlap_5_lineups_150_lambda_0.002_exposure_0.6_covar_chg75p_exp(spike).csv")
plot(as.Date(PnL$Date[inds_form]), PnL$PnL[inds_form], type = "b")


