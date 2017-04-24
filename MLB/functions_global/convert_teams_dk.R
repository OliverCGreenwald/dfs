if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for replace a vector of team names with the DK convention.


####### Function for Computing Covariance Matrix Given Start and End Date #######
convert_teams_dk <- function(team_vec, name_source) {
  # define DK naming convention
  dk_team_names <- c("Ari","Atl","Bal","Bos","Chc","Cin","Cle","Col","CWS","Det","Hou","KC","LAA","LAD","Mia","Mil","Min","NYM","NYY","Oak","Phi","Pit","SD","Sea","SF","StL","TB","Tex","Tor","Was")
  
  # replace source with DK naming convention
  if (name_source=="DFN") {
    source_team_names <- c("ARI","ATL","BAL","BOS","CHC","CIN","CLE","COL","CWS","DET","HOU","KC","LAA","LAD","MIA","MIL","MIN","NYM","NYY","OAK","PHI","PIT","SD","SEA","SF","STL","TB","TEX","TOR","WAS")
    for (i in 1:length(team_vec)) {
      # print(team_vec[i])
      team_vec[i] <- dk_team_names[which(source_team_names==team_vec[i])]
    }
  } else {
    stop("Source for team names not found (must be DFN,..).")
  }
  return(team_vec)
}