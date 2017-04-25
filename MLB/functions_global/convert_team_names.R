if(file.exists("~/Projects/DFS/")) {
  setwd("~/Projects/DFS/")
} else {
  setwd("~/Documents/DFS/")
}


####### Description #######
# Function for replace a vector of team names with the DK convention.


####### Function for Computing Covariance Matrix Given Start and End Date #######
convert_team_names <- function(team_vec, from_source, to_source) {
  
  if (from_source=="DK" & to_source=="DFN") {
    from_team_names <- c("Ari","Atl","Bal","Bos","Chc","Cin","Cle","Col","CWS","Det","Hou","KC","LAA","LAD","Mia","Mil","Min","NYM","NYY","Oak","Phi","Pit","SD","Sea","SF","StL","TB","Tex","Tor","Was") # DK team names
    to_team_names <- c("ARI","ATL","BAL","BOS","CHC","CIN","CLE","COL","CWS","DET","HOU","KC","LAA","LAD","MIA","MIL","MIN","NYM","NYY","OAK","PHI","PIT","SD","SEA","SF","STL","TB","TEX","TOR","WAS") # corresponding DFN team names
  } else if (from_source=="DFN" & to_source=="DK") {
    from_team_names <- c("ARI","ATL","BAL","BOS","CHC","CIN","CLE","COL","CWS","DET","HOU","KC","LAA","LAD","MIA","MIL","MIN","NYM","NYY","OAK","PHI","PIT","SD","SEA","SF","STL","TB","TEX","TOR","WAS") # DFN team names
    to_team_names <- c("Ari","Atl","Bal","Bos","Chc","Cin","Cle","Col","CWS","Det","Hou","KC","LAA","LAD","Mia","Mil","Min","NYM","NYY","Oak","Phi","Pit","SD","Sea","SF","StL","TB","Tex","Tor","Was") # corresponding DK team names
  } else {
    stop("From and To Team Name Sources Not Found.")
  }
  
  for (i in 1:length(team_vec)) {
    team_vec[i] <- to_team_names[which(from_team_names==team_vec[i])]
  }
  
  return(team_vec)
}