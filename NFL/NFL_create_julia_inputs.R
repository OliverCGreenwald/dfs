if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Create julia inputs for each date and contest:
# - Section I: aggregated projections, actual fpts for offense and defense for each contest

NFL_create_julia_inputs <- function(date_start, date_end) {
  ####### Import Functions #######
  source("NFL/functions_global/aggregateJuliaDF.R")
  
  ####### Section I (player data df) #######
  print("Creating Player Input Data Dataframe...")
  
  dates <- seq(from = as.Date(date_start), to = as.Date(date_end), by = "day") # one date
  for (d in 1:length(dates)) {
    # load full contest info file
    contest_info <- read.csv(file = 'NFL/data_warehouse/contests.csv', stringsAsFactors = F)
    
    # subset by date
    date <- dates[d]
    contest_info <- contest_info[contest_info$Contest_Date==as.Date(date),]
    
    # aggregate
    aggregated_data_offense <- list()
    aggregated_data_defense <- list()
    for (i in 1:nrow(contest_info)) {
      # check if folder for contest exists
      temp.dksalaries.path <- paste0(file = paste0("NFL/data_warehouse/", contest_info$Contest_Date[i],"/", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/DKSalaries.csv"))
      if (file.exists(temp.dksalaries.path)) {
        projections.dat <- aggregateJuliaDF(contest.date = contest_info$Contest_Date[i], contest.name = paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])))
        aggregated_data_offense[[i]] <- projections.dat[[1]]
        aggregated_data_defense[[i]] <- projections.dat[[2]]
        
        # remove NAs in pitchers df
        # aggregated_data_pitchers[[i]] <- aggregated_data_pitchers[[i]][!is.na(aggregated_data_pitchers[[i]]$Projection_dfn),]
        
        # DK changed team name convention to all caps starting 4/29, so we convert everything to all caps
        # TODO (done in aggregateJuliaDF.R file for now)
        
        ####### Write to CSV file #######
        write.csv(aggregated_data_offense[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/offense.csv"), row.names = F)
        write.csv(aggregated_data_defense[[i]], file = paste0("MLB/data_warehouse/", contest_info$Contest_Date[i],"/" , paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])), "/defense.csv"), row.names = F)
      } else {
        print(paste0("contest folder missing: ", paste0("NFL/data_warehouse/", contest_info$Contest_Date[i],"/", paste0(contest_info$Entry_Fee[i],"entry_",gsub(" ", "", contest_info$Contest_Name[i])))))
      }
    }
    
    print(paste0(dates[d], " completed"))
  }
}


NFL_create_julia_inputs(date_start = "2016-10-09", date_end = "2016-10-09")


