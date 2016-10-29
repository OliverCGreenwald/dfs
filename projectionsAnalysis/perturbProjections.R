#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

####### DESCRIPTION #########
# In this file we compute the variance of the "projection error", abs(Projected - Actual), for each player
# since the beginning of the season. We then perturb projections using a Gaussian random variable with
# mean 0 and variance "projection error".


###### COMPUTE PROJECTION ERROR (DFN) FOR EACH PLAYER #########

addError <- function(realized.data, projections.data) {
  projections.data$Actual_fpts <- realized.data$Actual.Score[match(projections.data$Name, realized.data$Player)]
  projections.data$Error_fpts_dfn <- abs(projections.data$Projection_dfn - projections.data$Actual_fpts)
  return(projections.data)
}

week.latest <- ceiling((as.numeric(Sys.Date()) - as.numeric(as.Date("2016-09-11")))/7 + 1) - 1
for (i in 1:week.latest) {
  name.realized <- paste("fpts.realized.week", i, sep = "")
  assign(name.realized, read.csv(file = paste0('resultsAnalysis/data_warehouse/player_weekly_performance/draftkings_player_production_week', i, '.csv'), stringsAsFactors = F))
  
  name.projections <- paste("fpts.offense.week", i, sep = "")
  assign(name.projections, read.csv(file = paste0('optimizationCode/data_warehouse/2016_cleaned_input/wk', i, '/offensive_players.csv'), stringsAsFactors = F))
  
  assign(name.projections, addError(eval(parse(text=name.realized)), eval(parse(text=name.projections))))
}


#----- any code below this line needs to be updated and/or cleaned -----#


####### COMBINE ALL WEEKS #########
fpts.offense.allweeks <- as.data.frame(fpts.offense.week2$Name) # week 2 has most players (?)
colnames(fpts.offense.allweeks) <- c('Name')

fpts.offense.allweeks$wk1_dfn_error <- fpts.offense.week1$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week1$Name)]
fpts.offense.allweeks$wk2_dfn_error <- fpts.offense.week2$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week2$Name)]
fpts.offense.allweeks$wk3_dfn_error <- fpts.offense.week3$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week3$Name)]
fpts.offense.allweeks$wk4_dfn_error <- fpts.offense.week4$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week4$Name)]
fpts.offense.allweeks$wk5_dfn_error <- fpts.offense.week5$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week5$Name)]
fpts.offense.allweeks$wk6_dfn_error <- fpts.offense.week6$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week6$Name)]
#fpts.offense.allweeks$wk7_dfn_error <- fpts.offense.week7$Error_fpts_dfn[match(fpts.offense.allweeks$Name, fpts.offense.week7$Name)]

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


