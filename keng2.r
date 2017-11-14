# Construct portfolio based on the hiring rate distribution
library(dplyr)
library(lubridate)
fromYear=1967
toYear=1967

for(year in fromYear:toYear) {
  load(paste('Merged_Compustat_CRSP',year,'.Rda',sep=""))
  View(Merged_Compustat_CRSP)
  Merged_Compustat_CRSP <- Merged_Compustat_CRSP %>% mutate(decile = ntile(Merged_Compustat_CRSP$hn, 10))
  View(Merged_Compustat_CRSP)
  startdecile <- 1
  enddecile <- 10
  results <- matrix(nrow=0,ncol=3)
  colnames(results) <- c("year", "decile", "averagereturn")
  print(results)
  marketCap <- numeric(length(Merged_Compustat_CRSP$GVKEY))
  return <- numeric(length(Merged_Compustat_CRSP$GVKEY))
  totalmarketCap <- numeric(10)
  totalreturn <- numeric(10)
  averagereturn <- numeric(10)
  
  for (i in 1:length(Merged_Compustat_CRSP$GVKEY)) {
    Merged_Compustat_CRSP$'marketCap'[i] = NA
    marketCap[i] <-as.numeric(Merged_Compustat_CRSP$csho[i]) * as.numeric(Merged_Compustat_CRSP$prcc_f[i])
    return[i] <- as.numeric(Merged_Compustat_CRSP$RET[i]) * (marketCap[i])
    if (!is.na(marketCap[i]) && !is.na(return[i])){
      j=Merged_Compustat_CRSP$decile[i]
      totalmarketCap[j] <- totalmarketCap[j]+marketCap[i]
      totalreturn[j] <- totalreturn[j]+return[i]
    }
  }
  
  for (j in startdecile : enddecile) {
    averagereturn[j] <- totalreturn[j] / totalmarketCap[j]
    results <- rbind(results, c(Merged_Compustat_CRSP$'year'[j], j, averagereturn[j]))
  }
  
  resultTable <- as.data.frame(results)
  View(resultTable)
}
save(resultTable, file = paste("averagereturn",year,".rda",sep=""))