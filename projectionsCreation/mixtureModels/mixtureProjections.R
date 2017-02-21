#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# In this file we cluster n player projections into k tiers. Offense only. Sunday only.
# The larger the TierRank, the better the player. Set kCutoffs higher if want more players
# to be ranked. Unranked players set to tier 0.
# Guidelines for setting kCutoffs and kTiers:
# - should be larger for weeks with more games b/c there are more players available
# - 
# Based on code from https://github.com/hueykwik/FantasyFootball/blob/master/tiers.Rmd.


####### IMPORT LIBRARIES #########
library(mclust)
library(ggplot2)


####### SET PARAMETERS #########
# wk <- 6

# suggested params
# param.mat <- as.data.frame(matrix(data = NA, nrow = 17, ncol = 15, dimnames = list(NULL, c("Week","Num.Games","Num.QB","Num.RB","Num.WR","Num.TE","Num.Total","Tiers.QB","Tiers.RB","Tiers.WR","Tiers.TE","Cutoff.QB","Cutoff.RB","Cutoff.WR","Cutoff.TE"))))
# param.mat$Week <- 1:17
# param.mat$Num.Games <- c(13,14,14,13,12,13,13,11,11,12,12,12,13,14,13,14,16)
# for (i in 1:17) {
#   temp.param <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", i, "/offensive_players.csv"), stringsAsFactors = F)
#   
#   # count for each position
#   param.mat$Num.QB[i] <- sum(temp.param$Position=="QB")
#   param.mat$Num.RB[i] <- sum(temp.param$Position=="RB")
#   param.mat$Num.WR[i] <- sum(temp.param$Position=="WR")
#   param.mat$Num.TE[i] <- sum(temp.param$Position=="TE")
#   param.mat$Num.Total[i] <- nrow(temp.param)
#   
#   # tiers for each position
#   param.mat$Tiers.QB[i] <- 5
#   
#   # cutoff for each position
#   param.mat$Cutoff.QB[i] <- param.mat$Num.Games[i]*2 + 5
# }

# number of tiers (clusters) for each position
kTiers = c(QB = 8, RB = 9, WR = 12, TE = 8)

# number of players to be considered for each position (e.g. 24 means top 24 players at that positino are ranked)
kCutoffs = c(QB = 24, RB = 40, WR = 60, TE = 24)

# iterate through these positions
pos.vec <- c("QB", "RB", "WR", "TE")


####### ITERATE THROUGH ALL WEEKS #########
for (wk in 1:17) {
  print(paste0("Week: ", wk))
  ####### LOAD DATA #########
  temp.dfn <- read.csv(file = paste0("optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week", wk, ".csv"), stringsAsFactors = F)
  
  
  ####### COMPUTE TIERS #########
  # Define function for computing tiers given a set of projections and number of tiers/clusters to be found
  computeTiers = function(projections, k, reverse = TRUE) { # reverse: Useful for clustering on projected points. We want highest projected points to be rank 1, so we reverse the levels.
    clusters = NULL
    while (is.null(clusters)) {
      tryCatch({ 
        clusters = Mclust(projections, G = k)
        n_clusters = length(unique(clusters$classification))
      }, 
      warning = function(w) { warning(w); return(NULL) },
      error = function(e) { warning(e); return(NULL) })
      
      if (!is.null(clusters)) break 
      k = k - 1
    }
    
    n_clusters = length(unique(clusters$classification))
    tiers = factor(clusters$classification)
    if (reverse) {
      levels(tiers) = rev(levels(tiers))
      levels(tiers) = n_clusters:1
    } else {
      levels(tiers) = 1:n_clusters
    }
    print(n_clusters)
    return(tiers)
  }
  
  # Compute tiers for each position in pos.vec
  tier.df.list <- list() # initialize list to store df for each position
  for (i in 1:length(pos.vec)) {
    pos = pos.vec[i]
    
    ecr_df = temp.dfn[temp.dfn$Pos==pos,]
    ecr_df <- ecr_df[order(ecr_df$Proj.FP, decreasing = T),]
    ecr_df = ecr_df[1:kCutoffs[pos],]  # The data is ordered from best rank to worst rank  
    ecr_df$TierRank = computeTiers(ecr_df$Proj.FP, kTiers[pos])
    print(pos.vec[i])
    
    ecr_df$nchar = nchar(as.character(ecr_df$Player.Name))  # For formatting later
    
    # Calculate position rank, negative so lowest rank will be at bottom in the plot below
    ecr_df$position.rank = -seq(nrow(ecr_df))
    
    # Plotting
    font = 3.5
    barsize = 1.5  
    dotsize = 2  
    ceil.fp <- ecr_df$Ceil.FP
    floor.fp <- ecr_df$Floor.FP
    
    # We put Avg.Rank as y because geom_errorbar requires ymin/ymax. We then flip the coordinates.
    p = ggplot(ecr_df, aes(x = position.rank, y = Proj.FP)) # we will flip these
    p = p + geom_errorbar(aes(ymin = floor.fp, ymax = ceil.fp, width=0.2, colour=TierRank), size=barsize*0.8, alpha=0.4)
    p = p + coord_flip() # flip x and y
    p = p + geom_text(aes(label=Player.Name, colour=TierRank, y = Proj.FP), size=font)
    p = p + scale_x_continuous("Ranking by DFN Projection")
    p = p + ylab("DFN Projected Fantasy Points (Floor-Proj-Ceiling)")
    p = p + ggtitle(paste0("Gaussian Mixture Model: Week ", wk, " Tiers (", pos, ")"))
    print(p)
    
    # change factors to char
    ecr_df$TierRank <- as.character(ecr_df$TierRank)
    
    # append to list
    tier.df.list[[pos]] <- ecr_df
  }
  
  
  ####### ADD TIER COLUMN TO CLEANED_INPUT_FILES #########
  # load 2016_cleaned_input file
  temp <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), stringsAsFactors = F)
  
  # add tier rank for each position
  for (i in 1:length(pos.vec)) {
    # load tier df for position
    tier.df.temp <- tier.df.list[[pos.vec[i]]]
    
    # subset cleaned input file by position
    temp.subset <- temp[temp$Position==pos.vec[i],]
    
    # add TierRank
    temp.subset$TierRank <- tier.df.temp$TierRank[match(temp.subset$Name, tier.df.temp$Player.Name)]
    
    # put subsetted data back into original df
    temp[temp$Position==pos.vec[i],'TierRank'] <- as.numeric(temp.subset$TierRank)
  }
  
  # set unranked players (NAs) to 0
  temp$TierRank[is.na(temp$TierRank)] <- 0
  
  # write to file
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), row.names = F) 
}

