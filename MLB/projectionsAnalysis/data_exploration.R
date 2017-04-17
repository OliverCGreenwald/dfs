if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Analyze projections. Using players from the $5 entry Knuckleball contest each day.

# TODO:
# - iterate over all contests


####### Import Functions #######
source("MLB/functions_global/aggregate_projections.R")


####### Create Aggregated Projected and Actual Fpts Dataframe For Each Day #######
# load contest info file
contest_info <- read.csv(file = 'MLB/data_warehouse/contests.csv', stringsAsFactors = F)

# find one 150 entry contest per day
contest_info_filtered <- NULL
dates <- seq(from = as.Date("2017-04-03"), to = Sys.Date() - 1, by = "day")
for (i in 1:length(dates)) {
  # subset by the date and max_entry=150
  contest_info_temp <- contest_info[contest_info$Contest_Date==dates[i] & contest_info$Max_Entry==150, ]
  
  # use knuckleball contest if exists, else use first 150 entry contest in df
  temp.found <- F
  for (j in 1:nrow(contest_info_temp)) {
    if (grepl("KNUCKLEBALL", contest_info_temp$Contest_Name[j])==T) {
      contest_info_filtered <- rbind(contest_info_filtered, contest_info_temp[j,])
      temp.found <- T
    }
  }
  if (temp.found==F) {
    contest_info_filtered <- rbind(contest_info_filtered, contest_info_temp[1,])
  }
}
remove(contest_info_temp, temp.found)

# aggregate projectioned and actual fpts for each day's contest
aggregated_data_hitters <- list()
aggregated_data_pitchers <- list()
for (i in 1:nrow(contest_info_filtered)) {
  projections.dat <- aggregate_projections(contest_info_filtered$Contest_Date[i], paste0(contest_info_filtered$Entry_Fee[i],"entry_",gsub(" ", "", contest_info_filtered$Contest_Name[i])))
  aggregated_data_hitters[[i]] <- projections.dat[[1]]
  aggregated_data_pitchers[[i]] <- projections.dat[[2]]
}
remove(projections.dat)


####### Check Correlations of Projections and Actual (filter by projection > 0 and !is.na()) #######
# hitters
rotogrinders_hitters_alldays <- NULL
dfn_hitters_alldays <- NULL
baseballmonster_hitters_alldays <- NULL
rotowire_hitters_alldays <- NULL
for (i in 1:nrow(contest_info_filtered)) {
  rotogrinders_hitters_alldays <- rbind(rotogrinders_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection > 0 & !is.na(aggregated_data_hitters[[i]]$Projection) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection", "Actual_fpts")])
  dfn_hitters_alldays <- rbind(dfn_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection_dfn > 0 & !is.na(aggregated_data_hitters[[i]]$Projection_dfn) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection_dfn", "Actual_fpts")])
  baseballmonster_hitters_alldays <- rbind(baseballmonster_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection_baseballmonster > 0 & !is.na(aggregated_data_hitters[[i]]$Projection_baseballmonster) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection_baseballmonster", "Actual_fpts")])
  rotowire_hitters_alldays <- rbind(rotowire_hitters_alldays, aggregated_data_hitters[[i]][aggregated_data_hitters[[i]]$Projection_rotowire > 0 & !is.na(aggregated_data_hitters[[i]]$Projection_rotowire) & !is.na(aggregated_data_hitters[[i]]$Actual_fpts), c("Projection_rotowire", "Actual_fpts")])
}

# pitchers
rotogrinders_pitchers_alldays <- NULL
dfn_pitchers_alldays <- NULL
baseballmonster_pitchers_alldays <- NULL
rotowire_pitchers_alldays <- NULL
for (i in 1:nrow(contest_info_filtered)) {
  rotogrinders_pitchers_alldays <- rbind(rotogrinders_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection", "Actual_fpts")])
  dfn_pitchers_alldays <- rbind(dfn_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection_dfn > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection_dfn) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection_dfn", "Actual_fpts")])
  baseballmonster_pitchers_alldays <- rbind(baseballmonster_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection_baseballmonster > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection_baseballmonster) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection_baseballmonster", "Actual_fpts")])
  rotowire_pitchers_alldays <- rbind(rotowire_pitchers_alldays, aggregated_data_pitchers[[i]][aggregated_data_pitchers[[i]]$Projection_rotowire > 0 & !is.na(aggregated_data_pitchers[[i]]$Projection_rotowire) & !is.na(aggregated_data_pitchers[[i]]$Actual_fpts), c("Projection_rotowire", "Actual_fpts")])
}

# correlations for all players (after filtering out projection > 0 and !is.na())
cor(rotogrinders_hitters_alldays$Projection, rotogrinders_hitters_alldays$Actual_fpts)
cor(dfn_hitters_alldays$Projection_dfn, dfn_hitters_alldays$Actual_fpts)
cor(baseballmonster_hitters_alldays$Projection_baseballmonster, baseballmonster_hitters_alldays$Actual_fpts)
cor(rotowire_hitters_alldays$Projection_rotowire, rotowire_hitters_alldays$Actual_fpts)

cor(rotogrinders_pitchers_alldays$Projection, rotogrinders_pitchers_alldays$Actual_fpts)
cor(dfn_pitchers_alldays$Projection_dfn, dfn_pitchers_alldays$Actual_fpts)
cor(baseballmonster_pitchers_alldays$Projection_baseballmonster, baseballmonster_pitchers_alldays$Actual_fpts)
cor(rotowire_pitchers_alldays$Projection_rotowire, rotowire_pitchers_alldays$Actual_fpts)

# correlations for all players with projection > X (after filtering out projection > 0 and !is.na())
X <- 7.5
cor(rotogrinders_hitters_alldays$Projection[rotogrinders_hitters_alldays$Projection > X], rotogrinders_hitters_alldays$Actual_fpts[rotogrinders_hitters_alldays$Projection > X])
cor(dfn_hitters_alldays$Projection_dfn[dfn_hitters_alldays$Projection_dfn > X], dfn_hitters_alldays$Actual_fpts[dfn_hitters_alldays$Projection_dfn > X])
cor(baseballmonster_hitters_alldays$Projection_baseballmonster[baseballmonster_hitters_alldays$Projection_baseballmonster > X], baseballmonster_hitters_alldays$Actual_fpts[baseballmonster_hitters_alldays$Projection_baseballmonster > X])
cor(rotowire_hitters_alldays$Projection_rotowire[rotowire_hitters_alldays$Projection_rotowire > X], rotowire_hitters_alldays$Actual_fpts[rotowire_hitters_alldays$Projection_rotowire > X])

X <- 15
cor(rotogrinders_pitchers_alldays$Projection[rotogrinders_pitchers_alldays$Projection > X], rotogrinders_pitchers_alldays$Actual_fpts[rotogrinders_pitchers_alldays$Projection > X])
cor(dfn_pitchers_alldays$Projection_dfn[dfn_pitchers_alldays$Projection_dfn > X], dfn_pitchers_alldays$Actual_fpts[dfn_pitchers_alldays$Projection_dfn > X])
cor(baseballmonster_pitchers_alldays$Projection_baseballmonster[baseballmonster_pitchers_alldays$Projection_baseballmonster > X], baseballmonster_pitchers_alldays$Actual_fpts[baseballmonster_pitchers_alldays$Projection_baseballmonster > X])
cor(rotowire_pitchers_alldays$Projection_rotowire[rotowire_pitchers_alldays$Projection_rotowire > X], rotowire_pitchers_alldays$Actual_fpts[rotowire_pitchers_alldays$Projection_rotowire > X])

# correlations for all players with Actual_fpts > X (after filtering out projection > 0 and !is.na())
X <- 7.5
cor(rotogrinders_hitters_alldays$Projection[rotogrinders_hitters_alldays$Actual_fpts > X], rotogrinders_hitters_alldays$Actual_fpts[rotogrinders_hitters_alldays$Actual_fpts > X])
cor(dfn_hitters_alldays$Projection_dfn[dfn_hitters_alldays$Actual_fpts > X], dfn_hitters_alldays$Actual_fpts[dfn_hitters_alldays$Actual_fpts > X])
cor(baseballmonster_hitters_alldays$Projection_baseballmonster[baseballmonster_hitters_alldays$Actual_fpts > X], baseballmonster_hitters_alldays$Actual_fpts[baseballmonster_hitters_alldays$Actual_fpts > X])
cor(rotowire_hitters_alldays$Projection_rotowire[rotowire_hitters_alldays$Actual_fpts > X], rotowire_hitters_alldays$Actual_fpts[rotowire_hitters_alldays$Actual_fpts > X])

X <- 15
cor(rotogrinders_pitchers_alldays$Projection[rotogrinders_pitchers_alldays$Actual_fpts > X], rotogrinders_pitchers_alldays$Actual_fpts[rotogrinders_pitchers_alldays$Actual_fpts > X])
cor(dfn_pitchers_alldays$Projection_dfn[dfn_pitchers_alldays$Actual_fpts > X], dfn_pitchers_alldays$Actual_fpts[dfn_pitchers_alldays$Actual_fpts > X])
cor(baseballmonster_pitchers_alldays$Projection_baseballmonster[baseballmonster_pitchers_alldays$Actual_fpts > X], baseballmonster_pitchers_alldays$Actual_fpts[baseballmonster_pitchers_alldays$Actual_fpts > X])
cor(rotowire_pitchers_alldays$Projection_rotowire[rotowire_pitchers_alldays$Actual_fpts > X], rotowire_pitchers_alldays$Actual_fpts[rotowire_pitchers_alldays$Actual_fpts > X])

# Takeaways:
# - rotogrinders seems to be better at predicting pitchers that do relatively well
quantile(rotogrinders_pitchers_alldays$Actual_fpts)
# - rotowire seems to be better at predicting hitters based on projection > X (but not observed for Actual_fpts > X)




