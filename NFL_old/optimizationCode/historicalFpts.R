#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we create a dataframe of all weekly historical fpts for each player, written to optimizationCode/historical_fpts folder.
# We also add historical fpts to the 2016_cleaned_input/all_data folder.


####### WRITE TO FILE? #######
write.bool <- F # TRUE if write to file, FALSE if don't write (MAKE SURE CODE ALL PARAMS ARE SET CORRECTLY BEFORE WRITING)


####### RUN SECTION #########
section.run <- "1" # 1 (historical fpts df) or 2 (freq ind) # need to run 1 before 2


####### IMPORT LIBRARIES #########
library('stringr')


if (section.run==1) {
  ####### LOAD DFN FILES (UPDATES FOLDER B/C WE NEED HISTORICAL ACTUAL FPTS) #########
  week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
  # player.names.qbs <- c() # record QBs for adding to player.names
  player.names <- c()
  for (i in 1:week.latest) {
    name <- paste("dfn_offense_week", i, sep = "")
    assign(name, read.csv(file = paste0('optimizationCode/data_warehouse/dailyfantasynerd/updates/dfn_offense_week', i, '.csv'), stringsAsFactors = F))
    
    # add week column
    temp.df <- eval(parse(text=name))
    temp.df$Week.Num <- i
    
    # clean names
    # temp.df$Player.Name <- sub("'", "", temp.df$Player.Name)
    
    # add Unique.ID column for matching purposes
    temp.df$Unique.ID <- paste0(temp.df$Player.Name, '@', temp.df$Team, '@', temp.df$Pos)
    
    # concatenate QBs who played this week to player.names.qbs
    # player.names.qbs <- c(player.names.qbs, temp.df[temp.df$Pos=='QB','Player.Name'])
    
    #
    player.names <- c(player.names, temp.df$Unique.ID)
    
    # assign
    assign(name, temp.df)
  }
  
  
  ####### CREATE DATAFRAME OF ALL HISTORICAL ACTUAL FPTS #######
  player.names <- unique(player.names)
  
  # intialize df and add player names
  historical.fpts.data <- as.data.frame(matrix(data = NA, nrow = length(player.names), ncol = week.latest+1))
  historical.fpts.data[,1] <- unique(player.names)
  
  # add column names to df
  colnames(historical.fpts.data)[1] <- "Unique.ID"
  for (i in 2:(week.latest+1)) {
    colnames(historical.fpts.data)[i] <- paste0("Week", i-1)
  }
  
  # split Unique.ID column (using @ symbol) for later use
  historical.fpts.data$FullName <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,1]
  historical.fpts.data$Team <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,2]
  historical.fpts.data$Pos <- str_split_fixed(historical.fpts.data$Unique.ID, "@", 3)[,3]
  
  # match
  for (i in 2:(week.latest+1)) {
    dfn.df <- eval(parse(text=paste("dfn_offense_week", i-1, sep = "")))
    historical.fpts.data[,i] <- dfn.df$Actual.FP[match(historical.fpts.data$Unique.ID, dfn.df$Unique.ID)]
  }
  
  # injuries
  historical.fpts.data[historical.fpts.data$Unique.ID=="LeSean McCoy@BUF@RB",'Week8'] <- NA
  historical.fpts.data[historical.fpts.data$Unique.ID=="Steve Smith@BAL@WR",'Week6'] <- NA
  historical.fpts.data[historical.fpts.data$Unique.ID=="Steve Smith@BAL@WR",'Week7'] <- NA
  historical.fpts.data[historical.fpts.data$Unique.ID=="Eric Ebron@DET@TE",'Week5'] <- NA
  historical.fpts.data[historical.fpts.data$Unique.ID=="Eric Ebron@DET@TE",'Week6'] <- NA
  historical.fpts.data[historical.fpts.data$Unique.ID=="Eric Ebron@DET@TE",'Week7'] <- NA
  
  # actually, let's just set all 0's to NA's b/c we don't have injury data
  is.na(historical.fpts.data[,2:(week.latest+1)]) <- !historical.fpts.data[,2:(week.latest+1)]
  
  # replace NA's with 0's for any game we know player actually got 0 fpts
  historical.fpts.data[historical.fpts.data$Unique.ID=="Brandin Cooks@NO@WR",'Week12'] <- 0
  historical.fpts.data[historical.fpts.data$Unique.ID=="Adam Thielen@MIN@WR",'Week15'] <- 0
  historical.fpts.data[historical.fpts.data$Unique.ID=="Jermaine Kearse@SEA@WR",'Week15'] <- 0
  
  # add mean column for analysis
  historical.fpts.data$mean <- NA
  for (j in 1:nrow(historical.fpts.data)) {
    historical.fpts.data$mean[j] <- mean(as.numeric(historical.fpts.data[j,2:(week.latest+1)]), na.rm = TRUE)  
  }
  
  
  ####### WRITE DATAFRAME TO FILE #######
  if (write.bool==T) {
    write.csv(historical.fpts.data, file = "optimizationCode/data_warehouse/historical_fpts/historical.fpts.csv", row.names = F) 
  } 
}