#install.packages(c('dplyr'))
library(dplyr)
library(tidyr)
library(data.table)
library(sigr)

Market<-c("FR","FR","UAE","UAE")
Test<-c("Test 1","Test 1","Test 2","Test 2")
Experiment<-c("Control","Exp 1","Control","Exp 1")
Unique.Visitors<-c(1000,1000,2000,2200)
App.start<-c(100,105,300,345)
App.complete<-c(50,64,80,150)
myData<-data.frame(Market,Test,Experiment,Unique.Visitors,App.start,App.complete)
ncol(myData)
myData$concat<-paste(Market,Test)
myData<-myData[,c(ncol(myData),1:ncol(myData)-1)]

origin<-ncol(myData)
conversions<-match(colnames(myData[as.numeric(which(sapply(myData[], is.numeric)))]), names(myData))[-1] 


for (i in as.numeric(which(sapply(myData[], is.numeric)))[-1]){
  colname<-paste(colnames(myData)[i],"%")
  #make sure unique visitors are the first numeric column from left to right
  myData[, ncol(myData) + 1] <- myData[i]/myData[as.numeric(which(sapply(myData[], is.numeric)))[1]]
  names(myData)[ncol(myData)] <- colname
  
}

#print(as.numeric(myData %>% filter(concat %in% i) %>% filter(Experiment %in% 'Control') %>% select(colnames(myData[i]))))


for (j in unique(myData$concat)){
  data<-myData[(myData$concat %in% j ), ]
  for (k in seq(origin+1, ncol(myData), by=1)){
    colname<-paste(colnames(data)[k],"uplift")
    #make sure unique visitors are the first numeric column from left to right
    data[, ncol(data) + 1] <- (data[k]-as.numeric(data[(data$Experiment=="Control"), ][k]))/as.numeric(data[(data$Experiment=="Control"), ][k])
    names(data)[ncol(data)] <- colname
    
    colname<-gsub("%","extra conversion",colnames(data[k]))
    #make sure unique visitors are the first numeric column from left to right
    data[, ncol(data) + 1] <- (data[k]-as.numeric(data[(data$Experiment=="Control"), ][k]))*sum(data[as.numeric(which(sapply(data[], is.numeric)))[1]])
    names(data)[ncol(data)] <- colname
    
    for(l in conversions){
      colname<-gsub("%","Conf.level",colnames(data[l]))
      #make sure unique visitors are the first numeric column from left to right
      UVControl<-as.numeric(data[(data$Experiment=="Control"), ][["Unique.Visitors"]])
      UCVControl<-as.numeric(data[(data$Experiment=="Control"), ][k])
      UV<-as.numeric(data[, ][["Unique.Visitors"]])
      UCV<-data[l]
      data[, ncol(data) + 1] <- 1-Bernoulli_diff_stat(UVControl,UCVControl,UV,UCV)$pValue
      names(data)[ncol(data)] <- colname
      
    }
    
  }
  ggg<-if(!exists('ggg')){data} else {bind_rows(ggg,data)}  
  
}

