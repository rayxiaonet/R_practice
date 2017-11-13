#dataHash = new.env(hash=TRUE, parent=emptyenv(), size=100L)

dir.create(file.path("./", "output"), showWarnings = FALSE)

fromYear=1965
toYear=2008

companies<-list() 
gvkeys<-list()

#year  gvkey  company_name hn   


#load all data for all years first
for(year in fromYear:toYear) {
  load(paste('Compustat',year,'.Rda',sep=""))
  #assign(paste("data",year,sep=""), annualFunda)
  #assign(paste("data",year,sep=""), annualFunda,dataHash)
#    companies<-unique(append(companies,annualFunda$"conm"))
#    gvkeys<=unique(append(companies,annualFunda$"gvkey"))
  
  #all companies loaded
  results<-matrix(nrow=0,ncol=4)
  
  if (year==fromYear){
    # no hn for year 1965 
    
  }else{
    gvkeys = annualFunda$'gvkey'
    print(paste('Processing year',year,', with records:',length(gvkeys)))

    #loop though every single company in the year
    for (i in 1:length(gvkeys)) {
      companyKey<-gvkeys[i]
      matchLoc <- match(companyKey,prevYearData$'gvkey')
      
      annualFunda$'hn'[i]= NA
      if( !is.na(annualFunda$'emp'[i]) & !is.na(prevYearData$'emp'[matchLoc])) {
        empCurrent<-annualFunda$'emp'[i]*1000
        empLast<-prevYearData$'emp'[matchLoc]*1000
        netHiring<-ceiling(empCurrent-empLast)
        annualFunda$'hn'[i]<- (netHiring/(0.5*empCurrent+empLast))
       # print( annualFunda$'hn'[i])
       # print(paste(' c  employee current year :',empCurrent,',  last year:',empLast,'delta',netHiring,' HN:',annualFunda$'hn'[i]))
        results <- rbind(results, c(year,companyKey,annualFunda$'conm'[i], annualFunda$'hn'[i]))
      }else{
        #print('invalid data - skip this company')
      }
      
    }
    save(annualFunda, file = paste("output/updated_",year,".rda",sep=""))
    print(paste('Finish process year',year))
    #print(results)

    
  }
  prevYearData = annualFunda
  resultTable <- as.data.frame(results)
  colnames(resultTable) <- c("year",  "gvkey","company_name","hn")
  #resultTable
  #to query the resultTable:
  #    subset(resultTable,year==1967)
  save(resultTable, file = paste("output/hn_result",year,".rda",sep=""))
  rm('annualFunda')
  
 
}


