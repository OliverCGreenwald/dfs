wk <- 17

temp.cleaned <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), stringsAsFactors = F)
temp.dfn <- read.csv(file = paste0("optimizationCode/data_warehouse/dailyfantasynerd/dfn_offense_week", wk, ".csv"), stringsAsFactors = F)
temp.cleaned$Projection_dfn <- temp.dfn$Proj.FP[match(paste0(temp.cleaned$Name, temp.cleaned$Position), paste0(temp.dfn$Player.Name, temp.dfn$Pos))]

write.csv(temp.cleaned, file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/offensive_players.csv"), row.names = F)



# temp.cleaned <- read.csv(file = paste0("optimizationCode/data_warehouse/2016_cleaned_input/wk", wk, "/model1/offensive_players.csv"), stringsAsFactors = F)
# sum(temp.cleaned$ValueWR)
