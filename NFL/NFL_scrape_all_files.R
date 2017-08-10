# TODO:
# - download contest results section

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

setwd(original_wd)

### Read in Contest File
contest_info <- read.csv(file = 'NFL/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)


# Scrape DK Site for todays contests 
contest_info <- download_DK_daily_contests_NFL(contest_info = contest_info, sunday_date = "2017-09-10") # hard coded date


### re-read in Contest File (not sure if needed but a safety precaution)
contest_info <- read.csv(file = 'NFL/data_warehouse/contests.csv', stringsAsFactors = F)
contest_info$Contest_Date <- as.Date(contest_info$Contest_Date)

# Find Earliest index of last contest
# first_contest_update <- min(which(as.Date(contest_info$Contest_Date) == Sys.Date()))

# Index of 

for(index in first_contest_update:length(contest_info$Contest_Date)) {
  print(paste0(index, ' of ', length(contest_info$Contest_Date), ' | Currently: ', contest_info$Contest_Name[index]))
  # Case: Contest Occured Yesterday 
  if (as.Date(contest_info$Contest_Date[index]) < (Sys.Date())) {
    # contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)
    # 
    # #Load in Player Results
    # download_player_results('MLB', as.Date(contest_info$Contest_Date[index]))
    # 
    # #Download DK Results File 
    # download_DK_contest_file_MLB(contest_info$Contest_ID[index], 
    #                              as.Date(contest_info$Contest_Date[index]),
    #                              contest_name)
    
  } else if (as.Date(contest_info$Contest_Date[index]) == (Sys.Date())) {
    # CASE: If contest Occurs Today
    contest_name <- gsub(" ", "", contest_info$Contest_Name[index], fixed = TRUE)
    
    # Load in DK Salary Files
    download_DK_player_salary_file_NFL(contest_info$Contest_ID[index], 
                                   as.Date(contest_info$Contest_Date[index]))
    
    # Download DK Payout Structure
    download_DK_payout_structure_NFL(contest_info$Contest_ID[index], 
                                     as.Date(contest_info$Contest_Date[index]),
                                     contest_name)
  }
}

### Clean DK Salary File
# TODO: probably add this to download_DK_player_salary_file.R
# source("NFL/functions_global/cleanDKSalaries.R")
# cleanDKSalaries(data = NULL)


### Download Rotogrinders Projections
# print('Downloading Rotogrinders Projections')
# download_rotogrinders_projections_MLB(Sys.Date())


