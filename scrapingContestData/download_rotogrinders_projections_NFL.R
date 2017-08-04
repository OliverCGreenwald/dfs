#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

download_rotogrinders_projections_NFL <- function(date) {
  original_wd <- getwd()
  
  # offense
  qb.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-qb.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(qb.data) <- qb.data[nrow(qb.data),]
  qb.data <- qb.data[-nrow(qb.data),]
  
  flex.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-flex.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(flex.data) <- flex.data[nrow(flex.data),]
  flex.data <- flex.data[-nrow(flex.data),]
  
  off.data <- rbind(qb.data, flex.data)
  
  # defense
  def.data <- read.csv(file = "https://rotogrinders.com/projected-stats/nfl-defense.csv?site=draftkings", stringsAsFactors = F, header = F)
  colnames(def.data) <- def.data[nrow(def.data),]
  def.data <- def.data[-nrow(def.data),]
  
  # write to file
  setwd(paste0('NFL/data_warehouse/projections/rotogrinders'))
  write.csv(off.data, file = paste0('rg_offense_', date, '.csv'), row.names = F)
  write.csv(def.data, file = paste0('rg_defense_', date, '.csv'), row.names = F)
  
  setwd(original_wd)
}

