#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

download_DK_contest_file_MLB <- function(contest_number) {
  original_wd <- getwd()
  browseURL(paste0('https://www.draftkings.com/contest/exportfullstandingscsv/', contest_number))
  setwd('~/Downloads')
  #unzip(paste0("contest-standings-", contest_number, ".zip"))
  
  contest <- read.csv(paste0("contest-standings-", contest_number, ".csv"), stringsAsFactors = F)
  setwd(original_wd)
  
  write.csv(contest, file = paste0('MLB/data_warehouse/',contest_number, '.csv'), row.names = F)
}



