#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

download_DK_contest_file_MLB <- function(contest_number, date, contest_name) {
  ### Local Function
  
  strEndsWith <- function(haystack, needle)
  {
    hl <- nchar(haystack)
    nl <- nchar(needle)
    if(nl>hl)
    {
      return(F)
    } else
    {
      return(substr(haystack, hl-nl+1, hl) == needle)
    }
  }
  
  
  original_wd <- getwd()
  browseURL(paste0('https://www.draftkings.com/contest/exportfullstandingscsv/', contest_number))
  setwd('~/Downloads')
  #unzip(paste0("contest-standings-", contest_number, ".zip"))
  while(!file.exists(paste0("contest-standings-", contest_number, ".csv"))){
    Sys.sleep(1)
  }
  contest <- read.csv(paste0("contest-standings-", contest_number, ".csv"), stringsAsFactors = F)
  file.remove(paste0("contest-standings-", contest_number, ".csv"))
  setwd(original_wd)
  
  setwd(paste0('MLB/data_warehouse/', date))
  file.sources = list.files()
  cleaned_files <- toupper(file.sources)
  cleaned_files <- gsub(" ", "", cleaned_files, fixed = TRUE)
  
  folder <- grep(toupper(get('contest_name')), cleaned_files, value=TRUE)
  
  correct_index = 0  
  for(index in 1:length(cleaned_files)) {
    if(strEndsWith(cleaned_files[index], toupper(get('contest_name')))) {
      correct_index = index
    }
  }
  
  setwd(file.sources[correct_index])
  
  
  write.csv(contest, file = 'contest-standings.csv', row.names = F)
  
  setwd(original_wd)
}

