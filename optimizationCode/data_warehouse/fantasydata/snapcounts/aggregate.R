#setwd("~/Projects/DFS/resultsAnalysis")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")


####### DESCRIPTION #######
# Aggregate the fantasydata.com snapcounts data.


####### 2016 #######
for (i in 1:16) {
  temp5 <- read.csv(file = paste0("optimizationCode/data_warehouse/fantasydata/snapcounts/min5/","fantasydata_snapcounts_2016_week",i,".csv"), stringsAsFactors = F)
  temp25 <- read.csv(file = paste0("optimizationCode/data_warehouse/fantasydata/snapcounts/min25/","fantasydata_snapcounts_2016_week",i,".csv"), stringsAsFactors = F)
  temp50 <- read.csv(file = paste0("optimizationCode/data_warehouse/fantasydata/snapcounts/min50/","fantasydata_snapcounts_2016_week",i,".csv"), stringsAsFactors = F)
  
  temp <- rbind(temp5, temp25, temp50)
  temp <- unique(temp[,-1])
  
  write.csv(temp, file = paste0("optimizationCode/data_warehouse/fantasydata/snapcounts/","fantasydata_snapcounts_2016_week",i,".csv"), row.names = F)
  print(i)
}


####### 2013-2015 #######
for (y in c(2013:2015)) {
  print(y)
  for (i in 1:17) {
    temp5 <- read.csv(file = paste0("optimizationCode/data_warehouse/",y,"/fantasydata/snapcounts/min5/","fantasydata_snapcounts_",y,"_week",i,".csv"), stringsAsFactors = F)
    temp25 <- read.csv(file = paste0("optimizationCode/data_warehouse/",y,"/fantasydata/snapcounts/min25/","fantasydata_snapcounts_",y,"_week",i,".csv"), stringsAsFactors = F)
    temp50 <- read.csv(file = paste0("optimizationCode/data_warehouse/",y,"/fantasydata/snapcounts/min50/","fantasydata_snapcounts_",y,"_week",i,".csv"), stringsAsFactors = F)
    
    temp <- rbind(temp5, temp25, temp50)
    temp <- unique(temp[,-1])
    
    write.csv(temp, file = paste0("optimizationCode/data_warehouse/",y,"/fantasydata/snapcounts/","fantasydata_snapcounts_",y,"_week",i,".csv"), row.names = F)
    print(i)
  }
}


