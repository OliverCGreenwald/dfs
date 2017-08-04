

if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


#######
# Insert NFL DK Scraping Code Here
######


### Clean DK Salary File
# TODO: probably add this to download_DK_player_salary_file.R
source("NFL/functions_global/cleanDKSalaries.R")
cleanDKSalaries(data = NULL)


### Download Rotogrinders Projections
print('Downloading Rotogrinders Projections')
download_rotogrinders_projections_MLB(Sys.Date())


