

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


### Save DFS Directory Path

original_wd <- getwd()

### Load all Scraping Functions

setwd('scrapingContestData')
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)
setwd("~/Projects/DFS/")


### Read in Contest File
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)


# Scrape DK Site for todays contests 
contest_info <- download_DK_daily_contests_MLB(contest_info)

# Find Earliest index of "yesterday's" contests
first_contest_update <- min(which(as.Date(contest_info$Contest_Date) == Sys.Date() - 1))

### Update Files

for(index in first_contest_update:length(contest_info$Contest_Date)) {
  print(contest_info$Contest_Name[index])
  # Case: Contest Occured Yesterday 
  if (as.Date(contest_info$Contest_Date[index]) == (Sys.Date() - 1)) {
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)

    #Load in Player Results
    download_player_results('MLB', as.Date(contest_info$Contest_Date[index]))
    
    #Download DK Results File 
    download_DK_contest_file_MLB(contest_info$Contest_ID[index], 
                                 as.Date(contest_info$Contest_Date[index]),
                                 contest_name)
    
  } else if (as.Date(contest_info$Contest_Date[index]) == (Sys.Date())) {
  # CASE: If contest Occurs Today
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)
    
    # Load in DK Salary Files
    download_DK_player_salary_file(contest_info$Contest_ID[index], 
                                   as.Date(contest_info$Contest_Date[index]))
    
    # Download DK Payout Structure
    download_DK_payout_structure_MLB(contest_info$Contest_ID[index], 
                                     as.Date(contest_info$Contest_Date[index]),
                                     contest_name)
    
    
    
  }
  
  
}
# Download Rotogrinders Projections
download_rotogrinders_projections_MLB(as.Date(contest_info$Contest_Date[index]))

# Download DFN Projections
# NEED TO FINISH

# Download BaseballMonster Projections
download_BBmonster_projections()

# Download FantasyPros Projections
# NEED TO FINISH

# Download Rotowire Projections
# NEED TO FINISH



quit(save='no')