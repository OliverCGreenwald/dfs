#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we compute the variance of the "projection error", abs(Projected - Actual), for each player
# since the beginning of the season. We then perturb projections using a Gaussian random variable with
# mean 0 and variance "projection error".


###### COMPUTE PROJECTION ERROR (ROTOGRINDERS AND DFN) FOR EACH PLAYER FOR EACH WEEK #########

####### Week 1 #########
fpts.realized.week1 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week1.csv', stringsAsFactors = F)

fpts.offense.week1 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk1/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week1$Actual_fpts <- fpts.realized.week1$Actual.Score[match(fpts.offense.week1$Name, fpts.realized.week1$Player)]
fpts.offense.week1$Error_fpts_dfn <- abs(fpts.offense.week1$Projection_dfn - fpts.offense.week1$Actual_fpts)

####### Week 2 #########
fpts.realized.week2 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week2.csv', stringsAsFactors = F)

fpts.offense.week2 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk2/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week2$Actual_fpts <- fpts.realized.week2$Actual.Score[match(fpts.offense.week2$Name, fpts.realized.week2$Player)]
fpts.offense.week2$Error_fpts_dfn <- abs(fpts.offense.week2$Projection_dfn - fpts.offense.week2$Actual_fpts)

# fpts.offense.week2$Name <- sub(' Sr.', '', fpts.offense.week2$Name) # remove Sr.
# fpts.offense.week2$Name <- sub(' Jr.', '', fpts.offense.week2$Name) # remove Jr.

####### Week 3 #########
fpts.realized.week3 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week3.csv', stringsAsFactors = F)

fpts.offense.week3 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk3/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week3$Actual_fpts <- fpts.realized.week3$Actual.Score[match(fpts.offense.week3$Name, fpts.realized.week3$Player)]
fpts.offense.week3$Error_fpts_dfn <- abs(fpts.offense.week3$Projection_dfn - fpts.offense.week3$Actual_fpts)

# fpts.offense.week3$Name <- sub(' Sr.', '', fpts.offense.week3$Name) # remove Sr.
# fpts.offense.week3$Name <- sub(' Jr.', '', fpts.offense.week3$Name) # remove Jr.

####### Week 4 #########
fpts.realized.week4 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week4.csv', stringsAsFactors = F)

fpts.offense.week4 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk4/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week4$Actual_fpts <- fpts.realized.week4$Actual.Score[match(fpts.offense.week4$Name, fpts.realized.week4$Player)]
fpts.offense.week4$Error_fpts_dfn <- abs(fpts.offense.week4$Projection_dfn - fpts.offense.week4$Actual_fpts)

# fpts.offense.week4$Name <- sub(' Sr.', '', fpts.offense.week4$Name) # remove Sr.
# fpts.offense.week4$Name <- sub(' Jr.', '', fpts.offense.week4$Name) # remove Jr.

####### Week 5 #########
fpts.realized.week5 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week5.csv', stringsAsFactors = F)

fpts.offense.week5 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk5/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week5$Actual_fpts <- fpts.realized.week5$Actual.Score[match(fpts.offense.week5$Name, fpts.realized.week5$Player)]
fpts.offense.week5$Error_fpts_dfn <- abs(fpts.offense.week5$Projection_dfn - fpts.offense.week5$Actual_fpts)

# fpts.offense.week5$Name <- sub(' Sr.', '', fpts.offense.week5$Name) # remove Sr.
# fpts.offense.week5$Name <- sub(' Jr.', '', fpts.offense.week5$Name) # remove Jr.


####### Week 6 #########
fpts.realized.week6 <- read.csv(file = 'resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week6.csv', stringsAsFactors = F)

fpts.offense.week6 <- read.csv(file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk6/offensive_players.csv', stringsAsFactors = F)
fpts.offense.week6$Actual_fpts <- fpts.realized.week6$Actual.Score[match(fpts.offense.week6$Name, fpts.realized.week6$Player)]
fpts.offense.week6$Error_fpts_dfn <- abs(fpts.offense.week6$Projection_dfn - fpts.offense.week6$Actual_fpts)

# fpts.offense.week6$Name <- sub(' Sr.', '', fpts.offense.week6$Name) # remove Sr.
# fpts.offense.week6$Name <- sub(' Jr.', '', fpts.offense.week6$Name) # remove Jr.

####### COMBINE ALL WEEKS #########
fpts.offense.allweeks <- as.data.frame(fpts.offense.week2$Name)
colnames(fpts.offense.allweeks) <- c('Name')
fpts.offense.allweeks$wk1_dfn_error <- fpts.offense.week1$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week1$Name)]
fpts.offense.allweeks$wk2_dfn_error <- fpts.offense.week2$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week2$Name)]
fpts.offense.allweeks$wk3_dfn_error <- fpts.offense.week3$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week3$Name)]
fpts.offense.allweeks$wk4_dfn_error <- fpts.offense.week4$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week4$Name)]
fpts.offense.allweeks$wk5_dfn_error <- fpts.offense.week5$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week5$Name)]
#fpts.offense.allweeks$wk6_dfn_error <- fpts.offense.week6$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week6$Name)]

# compute standard deviation
fpts.offense.allweeks <- transform(fpts.offense.allweeks, sd_dfn_error=apply(fpts.offense.allweeks,1, sd, na.rm = TRUE))
fpts.offense.allweeks$sd_dfn_error[is.na(fpts.offense.allweeks$sd_dfn_error)==T] <- 0 # replace NAs with 0 b/c no perturbation


####### PERTURB PROJECTIONS #########
# Note: there's some look ahead bias b/c we won't have the variance for future weeks
salary.lo <- 3000
salary.hi <- 4000

# Week 6
fpts.offense.week6$sd_dfn_error <- fpts.offense.allweeks$sd_dfn_error[match(fpts.offense.week6$Name, fpts.offense.allweeks$Name)]
fpts.offense.week6$sd_dfn_error[is.na(fpts.offense.week6$sd_dfn_error)==T] <- 0 # replace NAs with 0 b/c no perturbation
fpts.offense.week6$Projection_dfn_perturbed <- NA

for (i in 1:nrow(fpts.offense.week6)) {
  perturbation <- rnorm(n = 1, mean = 0, sd = fpts.offense.week6$sd_dfn_error[i])
  if (salary.lo <= fpts.offense.week6$Salary[i] & fpts.offense.week6$Salary[i] <= salary.hi) {
    fpts.offense.week6$Projection_dfn_perturbed[i] <- max(0, fpts.offense.week6$Projection_dfn[i] + perturbation)
  } else {
    fpts.offense.week6$Projection_dfn_perturbed[i] <- fpts.offense.week6$Projection_dfn[i]
  }
}

fpts.offense.week6$Actual_fpts <- NULL
fpts.offense.week6$Error_fpts_dfn <- NULL
fpts.offense.week6$sd_dfn_error <- NULL

write.csv(fpts.offense.week6, file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk6/offensive_players.csv', row.names = F) # input in julia code


# Week 5
fpts.offense.week5$sd_dfn_error <- fpts.offense.allweeks$sd_dfn_error[match(fpts.offense.week5$Name, fpts.offense.allweeks$Name)]
fpts.offense.week5$sd_dfn_error[is.na(fpts.offense.week5$sd_dfn_error)==T] <- 0 # replace NAs with 0 b/c no perturbation
fpts.offense.week5$Projection_dfn_perturbed <- NA

for (i in 1:nrow(fpts.offense.week5)) {
  perturbation <- rnorm(n = 1, mean = 0, sd = fpts.offense.week5$sd_dfn_error[i])
  if (salary.lo <= fpts.offense.week5$Salary[i] & fpts.offense.week5$Salary[i] <= salary.hi) {
    fpts.offense.week5$Projection_dfn_perturbed[i] <- max(0, fpts.offense.week5$Projection_dfn[i] + perturbation)
  } else {
    fpts.offense.week5$Projection_dfn_perturbed[i] <- fpts.offense.week5$Projection_dfn[i]
  }
}

fpts.offense.week5$Actual_fpts <- NULL
fpts.offense.week5$Error_fpts_dfn <- NULL
fpts.offense.week5$sd_dfn_error <- NULL

write.csv(fpts.offense.week5, file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk5/offensive_players.csv', row.names = F) # input in julia code


# Week 4
fpts.offense.week4$sd_dfn_error <- fpts.offense.allweeks$sd_dfn_error[match(fpts.offense.week4$Name, fpts.offense.allweeks$Name)]
fpts.offense.week4$sd_dfn_error[is.na(fpts.offense.week4$sd_dfn_error)==T] <- 0 # replace NAs with 0 b/c no perturbation
fpts.offense.week4$Projection_dfn_perturbed <- NA

for (i in 1:nrow(fpts.offense.week4)) {
  perturbation <- rnorm(n = 1, mean = 0, sd = fpts.offense.week4$sd_dfn_error[i])
  if (salary.lo <= fpts.offense.week4$Salary[i] & fpts.offense.week4$Salary[i] <= salary.hi) {
    fpts.offense.week4$Projection_dfn_perturbed[i] <- max(0, fpts.offense.week4$Projection_dfn[i] + perturbation)
  } else {
    fpts.offense.week4$Projection_dfn_perturbed[i] <- fpts.offense.week4$Projection_dfn[i]
  }
}

fpts.offense.week4$Actual_fpts <- NULL
fpts.offense.week4$Error_fpts_dfn <- NULL
fpts.offense.week4$sd_dfn_error <- NULL

write.csv(fpts.offense.week4, file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk4/offensive_players.csv', row.names = F) # input in julia code


# Week 3
fpts.offense.week3$sd_dfn_error <- fpts.offense.allweeks$sd_dfn_error[match(fpts.offense.week3$Name, fpts.offense.allweeks$Name)]
fpts.offense.week3$sd_dfn_error[is.na(fpts.offense.week3$sd_dfn_error)==T] <- 0 # replace NAs with 0 b/c no perturbation
fpts.offense.week3$Projection_dfn_perturbed <- NA

for (i in 1:nrow(fpts.offense.week3)) {
  perturbation <- rnorm(n = 1, mean = 0, sd = fpts.offense.week3$sd_dfn_error[i])
  if (salary.lo <= fpts.offense.week3$Salary[i] & fpts.offense.week3$Salary[i] <= salary.hi) {
    fpts.offense.week3$Projection_dfn_perturbed[i] <- max(0, fpts.offense.week3$Projection_dfn[i] + perturbation)
  } else {
    fpts.offense.week3$Projection_dfn_perturbed[i] <- fpts.offense.week3$Projection_dfn[i]
  }
}

fpts.offense.week3$Actual_fpts <- NULL
fpts.offense.week3$Error_fpts_dfn <- NULL
fpts.offense.week3$sd_dfn_error <- NULL

write.csv(fpts.offense.week3, file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk3/offensive_players.csv', row.names = F) # input in julia code


# Week 2
fpts.offense.week2$sd_dfn_error <- fpts.offense.allweeks$sd_dfn_error[match(fpts.offense.week2$Name, fpts.offense.allweeks$Name)]
fpts.offense.week2$sd_dfn_error[is.na(fpts.offense.week2$sd_dfn_error)==T] <- 0 # replace NAs with 0 b/c no perturbation
fpts.offense.week2$Projection_dfn_perturbed <- NA

for (i in 1:nrow(fpts.offense.week2)) {
  perturbation <- rnorm(n = 1, mean = 0, sd = fpts.offense.week2$sd_dfn_error[i])
  if (salary.lo <= fpts.offense.week2$Salary[i] & fpts.offense.week2$Salary[i] <= salary.hi) {
    fpts.offense.week2$Projection_dfn_perturbed[i] <- max(0, fpts.offense.week2$Projection_dfn[i] + perturbation)
  } else {
    fpts.offense.week2$Projection_dfn_perturbed[i] <- fpts.offense.week2$Projection_dfn[i]
  }
}

fpts.offense.week2$Actual_fpts <- NULL
fpts.offense.week2$Error_fpts_dfn <- NULL
fpts.offense.week2$sd_dfn_error <- NULL

write.csv(fpts.offense.week2, file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk2/offensive_players.csv', row.names = F) # input in julia code


# Week 1
fpts.offense.week1$sd_dfn_error <- fpts.offense.allweeks$sd_dfn_error[match(fpts.offense.week1$Name, fpts.offense.allweeks$Name)]
fpts.offense.week1$sd_dfn_error[is.na(fpts.offense.week1$sd_dfn_error)==T] <- 0 # replace NAs with 0 b/c no perturbation
fpts.offense.week1$Projection_dfn_perturbed <- NA

for (i in 1:nrow(fpts.offense.week1)) {
  perturbation <- rnorm(n = 1, mean = 0, sd = fpts.offense.week1$sd_dfn_error[i])
  if (salary.lo <= fpts.offense.week1$Salary[i] & fpts.offense.week1$Salary[i] <= salary.hi) {
    fpts.offense.week1$Projection_dfn_perturbed[i] <- max(0, fpts.offense.week1$Projection_dfn[i] + perturbation)
  } else {
    fpts.offense.week1$Projection_dfn_perturbed[i] <- fpts.offense.week1$Projection_dfn[i]
  }
}

fpts.offense.week1$Actual_fpts <- NULL
fpts.offense.week1$Error_fpts_dfn <- NULL
fpts.offense.week1$sd_dfn_error <- NULL

write.csv(fpts.offense.week1, file = 'optimizationCode/data_warehouse/2016_cleaned_input/wk1/offensive_players.csv', row.names = F) # input in julia code


