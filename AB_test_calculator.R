#The file containing the data needs to be in a Excel XLSX file and the the first 4 column needs to be in that order
#Date, Experiment, Users and visits, then all the metrics
# Date     Experiment   Users    Sessions Metric1 Metric2
# 20190828 Control      179      199     103      83
# 20190813 Control      162      195     105      72
# 20190820 Control      161      181      95      75

#Set the parameter of the test
#when passing numerical value, do not include '%', only numeric
#statistical power should be set by defaut at 80% 
Statistical_Power<-80
#Confidence level is traditionally set to 95%
Confidence_level<-95
#Number of daily visitors(or monthly and divide by 30)
Daily_visitors<-15
#Current conversion rate in %
Control_conversion_rate<-5
#number of experiment (including control)
Experiment_number<-2
#Maximu number od days to run the test (60 days )
Max_duration<-60
#Alternative - default two.sided, the number 1 selects the index position for two.sided, 2 for less etc...
Alternative=c("two.sided","less","greater")[1]
#Would you like to show the plots? 1 for Yes, 2 for No
PLOTConf=c("YES","NO")[1]
#Would you like to write the results to Excel? 1 for Yes, 2 for No
writeToExcel=c("YES","NO")[2]


if (interactive()) {
  file<-file.choose(new = FALSE)
} else {
  file = system.file(...)
}
print(file)
theme_set(
  theme_bw() +
    theme(legend.position = "bottom")
)

#load librairies
PKG<-c("tidyverse","tidyr","dplyr","reshape2","pwr","ggplot2","strucchange","ggpubr","readr","rlang","openxlsx","zoo","data.table","sigr","lubridate","stringr")
for (pkg in PKG){
  if (pkg %in% rownames(installed.packages()) == FALSE)  {install.packages(pkg, character.only = TRUE)} else {require(pkg,character.only = TRUE)}
}

myData <- read_csv(file, skip_empty_rows = FALSE)
colnames(myData)<-gsub("\\s+","",c("Date","Experiment","Users","Sessions",colnames(myData)[5:length(colnames(myData))]))
myData$Date<-as.Date(as.character(myData$Date), "%Y%m%d")

myData<-myData%>% arrange(Experiment,Date)
colnames(myData)
Experiment_colum<-"Experiment"
#Experiment_choice<-unlist(unique(myData %>% select(!!! rlang::syms(Experiment_colum))))
Experiment_choice<-unique(myData[[Experiment_colum]])
Control_variable<-menu(Experiment_choice, graphics=TRUE, title="Choose control")
Control_variable<-Experiment_choice[Control_variable]

UV_position<-3
Visits<-4
UV_name<-colnames(myData)[UV_position]
conversion_position<-UV_position+2
sampleSize<-myData[,c(UV_position,conversion_position:ncol(myData))]
Dates<-nrow(unique(myData["Date"]))
endCo<-ncol(myData)
Opt_stats<-myData

for (i in UV_position:endCo){
  varname <- paste("cumsum",colnames(myData)[i])
  metric_column<-colnames(myData)[i]
  myData<-myData %>% arrange(Experiment,Date) %>% group_by(Experiment) %>% mutate( cumsum(!!sym(metric_column)))  %>% ungroup()
}

myData<-myData[,-c(UV_position:endCo)]
myData$Date<-as.Date(myData$Date)
plot_list = list()
Concat_column<-"Date"
Statistical_Power<-Statistical_Power/100
Confidence_level<-Confidence_level/100
###do the math####
startTime<-Sys.time()
for (j in conversion_position:ncol(Opt_stats)){
  for (i in 1:nrow(myData)){
    Concat_variable<-unlist(myData %>% filter(row_number() == i) %>% select (!!sym(Concat_column)))
    #create conversion rate from second numeric column and select UV and UC
    conversion<-names (myData)[j]
    visitors<-names (myData)[UV_position]
    UV_control<-unlist(myData %>% filter(!!sym(Concat_column) %in% as.character(Concat_variable))  %>% filter(!!sym(Experiment_colum) %in% as.character(Control_variable)) %>% select(UV_position))      
    UC_control<-unlist(myData %>% filter(!!sym(Concat_column) %in% as.character(Concat_variable))  %>% filter(!!sym(Experiment_colum) %in% as.character(Control_variable)) %>% select(conversion)) 
    UV_variant<-unlist(myData %>% filter(row_number() == i) %>% select (!!sym(visitors)))
    UC_variant<-unlist(myData %>% filter(row_number() == i) %>% select (!!sym(conversion)))
    
    if(UC_variant > 0 | UC_control >0){    
      
      control_CR<-UC_control/UV_control
      variant_CR<-UC_variant/UV_variant
      
      if(control_CR==variant_CR) {MDE<-0 } else{MDE<-power.prop.test(p1=control_CR,n=UV_control, power=Statistical_Power, alternative=Alternative, sig.level=Confidence_level)}
      #MDE<-power.prop.test(p1=control_CR,n=sum(cum_UV_control), power=Statistical_Power, alternative='two.sided', sig.level=Confidence_level)
      if(control_CR==variant_CR){MDE_V1<-0 } else{ MDE_V1<-power.prop.test(p1=control_CR,p2=variant_CR, power=Statistical_Power, alternative=Alternative, sig.level=Confidence_level)}
      
      ###Calculate ideal sample size required to reach SS#######################################################
      ####Fear factor/thrilling/exciting/safe
      rm(sampleSize)
      sampleSize <- data.frame(matrix(ncol = 7, nrow = 0))
      colnames(sampleSize) <- c("n", "Control.conversion.rate", "p2","uplift","conf.level","power","alternative")
      i_Dates<-NULL
      if(round(UV_control/(as.numeric(myData$Date[i] - min(myData$Date))+1)) < 1 ) {Daily_visitors<-1} else{Daily_visitors<-round(UV_control/(as.numeric(myData$Date[i] - min(myData$Date))+1))}
      
      for (i_Dates in seq(Daily_visitors,Daily_visitors*Max_duration, by=Daily_visitors)){
        if(i_Dates<2 && control_CR== 1){i_Dates<-2
        controlCR<-.99999} else {controlCR<-control_CR}
        mde_sampleSize<-power.prop.test(p1=controlCR, n=i_Dates, power=Statistical_Power, sig.level=Confidence_level)
        #print(round(mde_sampleSize$p1*100,2))
        sampleSize<-bind_rows(sampleSize,data.frame(n=mde_sampleSize$n,Control.conversion.rate=paste0(round(mde_sampleSize$p1*100,2),"%"),p2=mde_sampleSize$p2,uplift=(mde_sampleSize$p2-mde_sampleSize$p1)/mde_sampleSize$p1,conf.level=mde_sampleSize$sig.level,power=mde_sampleSize$power,alternative=mde_sampleSize$alternative))
      }
      
      sampleSize_chart<-breakpoints(sampleSize$uplift~1)
      sampleSize[[max(sampleSize_chart$breakpoints),"n"]]
      sampleSize[[max(sampleSize_chart$breakpoints),"p2"]]
      sampleSize[[max(sampleSize_chart$breakpoints),"uplift"]]
      
      ###############################################################################################################
    } 
    
    #PvAlue
    CVRControl<-UC_control/UV_control
    StdErrorControl<-(CVRControl*(1-CVRControl)/UV_control)^(.5)
    
    CVRVariant<-UC_variant/UV_variant
    StdErrorVariant<-(CVRVariant*(1-CVRVariant)/UV_variant)^(.5)
    
    StdErrordiff<-(StdErrorControl^2 + StdErrorVariant^2)^(1/2)
    zScore<-(CVRVariant-CVRControl)/StdErrordiff
    pValue<-pnorm(zScore)
    #print(paste(i,j,StdErrorControl,StdErrorVariant,StdErrordiff,zScore,pValue))
    
    
    
    #MDE

    myData[[paste(conversion,"MDE(%)")]][i]<-if (MDE==0) {0} else if (unlist(myData %>% filter(row_number() == i) %>% select(!!sym(Experiment_colum))) == as.character(Control_variable)) {0} else {MDE$p2-MDE$p1}
    myData[[paste(conversion,"Sample Size")]][i]<-if(control_CR==variant_CR){0} else{ MDE_V1$n}
    myData[[paste(conversion,"Ideal Sample Size")]][i]<-if (unlist(myData %>% filter(row_number() == i) %>% select(!!sym(Experiment_colum))) == as.character(Control_variable)) {sampleSize[[max(sampleSize_chart$breakpoints),"n"]]} else {0} 
    Dates_to_test<-round(sampleSize[[max(sampleSize_chart$breakpoints),"n"]]/Daily_visitors)
    myData[[paste(conversion,"Dates to reach SS")]][i]<-if(control_CR==variant_CR){0 } else{ Dates_to_test}
    #CVR
    myData[[paste(conversion,"CVR (%)")]][i]<-UC_variant/UV_variant
    #create uplift column from conversion rate myData[]
    divider<-unlist(myData %>% filter(!!sym(Concat_column) %in% as.character(Concat_variable))  %>% filter(!!sym(Experiment_colum) %in% as.character(Control_variable)) %>% select(!!sym(paste(conversion,"CVR (%)"))))
    top<-unlist(myData %>% filter(row_number() == i) %>% select (!!sym(paste(conversion,"CVR (%)"))))
    myData[[paste(conversion,"Ideal new CVR")]][i]<-if (unlist(myData %>% filter(row_number() == i) %>% select(!!sym(Experiment_colum))) == as.character(Control_variable)) {0} else {sampleSize[[max(sampleSize_chart$breakpoints),"p2"]]} 
    
    myData[[paste(conversion,"CVR uplift (%)")]][i]<-top/divider-1
    myData[[paste(conversion,"Ideal uplift")]][i]<-if (unlist(myData %>% filter(row_number() == i) %>% select(!!sym(Experiment_colum))) == as.character(Control_variable)) {0} else {sampleSize[[max(sampleSize_chart$breakpoints),"uplift"]]} 
    
    Bernoulli<-1-(2-(pnorm(qnorm(Confidence_level/2+(1-Confidence_level))/(UV_control/sampleSize[[max(sampleSize_chart$breakpoints),"n"]])^2)*2))
    myData[[paste(conversion,"Confidence (%)")]][i]<-pValue
    Bern<-if(Bernoulli < Confidence_level){Confidence_level} else{Bernoulli}
    myData[[paste(conversion,"Bernoulli confidence progress positive")]][i]<-Bern
    myData[[paste(conversion,"Bernoulli confidence progress negative")]][i]<-1-Bern
    myData[[paste(conversion,"statistically significant")]][i]<-if (unlist(myData %>% filter(row_number() == i) %>% select(!!sym(Experiment_colum))) == as.character(Control_variable)) {"No"} else if (pValue > 0.5 & pValue>Bern ) {"Yes"} else if (pValue < 0.5 & pValue<Bern ) {"Yes"} else {"No"}
    
    myData[[paste(conversion,"bankable")]][i]<-CVRVariant*UV_variant - CVRControl*UV_control
    
    myData[is.na(myData)] <- 0
    stopTime<-Sys.time()
    
    
    print(paste(i,conversion,"Time elapsed so far",(stopTime- startTime),"seconds"))
    print(i)
    
  }
  
  #########plot the data############
  
  if(PLOTConf =="YES"){
    dd<-melt(myData %>% filter(!(!!sym(Experiment_colum) == as.character(Control_variable)))  %>%  select(Date,!!sym(paste(conversion,"Confidence (%)")),!!sym(paste(conversion,"Bernoulli confidence progress positive")),!!sym(paste(conversion,"Bernoulli confidence progress negative"))),id="Date")
    dd$variable<-gsub("cumsum|\\(|\\)","",dd$variable)
    dd$Date<-as.Date(dd$Date)
    Date_ss<-unlist(first(myData %>% filter(!(!!sym(Experiment_colum) == as.character(Control_variable))) %>% filter((!!sym(paste(conversion,"statistically significant")) == "Yes")) %>% select(Date)))[1]
    value_ss<-unlist(first(myData %>% filter(!(!!sym(Experiment_colum) == as.character(Control_variable))) %>% filter((!!sym(paste(conversion,"statistically significant")) == "Yes")) %>% select(!!sym(paste(conversion,"Confidence (%)")))))[1]
    if(is.na(value_ss) | is.na(Date_ss) ){Date_ss<-0 
    value_ss<-0 }
    colour_ss<-if(value_ss<0.5){"red"} else{"green"}
    
    conf_progress<-ggplot(data=dd,  aes(x=Date, y=value, colour=variable)) + geom_line() + scale_y_continuous(labels = function(x) paste0(x*100, "%"))
    conf_progress<-conf_progress + geom_hline(yintercept = 0.5, colour = "blue") + geom_hline(yintercept = value_ss,size = .5, colour = colour_ss) + geom_vline(xintercept = as.Date(Date_ss),size = 1, colour = colour_ss) 
    conf_progress<-conf_progress+   xlab("Date") + ylab("Level of confidence") 
    
    sampleSize_def <-ggplot(sampleSize %>% select(n,Control.conversion.rate,uplift), aes(x=n, y=uplift, col=Control.conversion.rate)) + geom_line()+ scale_y_continuous(labels = function(x) paste0(x*100, "%"))
    sampleSize_def<-sampleSize_def + geom_vline(xintercept = max(sampleSize$n[sampleSize_chart$breakpoints]), 
                                                size = 1, colour = "red") + geom_hline(yintercept = min(sampleSize$uplift[sampleSize_chart$breakpoints]), 
                                                                                       size = 1, colour = "green")
    sampleSize_def<-sampleSize_def  +   xlab("Total unique visitors in each experiment") + ylab("Uplift (ratio)") 
    
    
    #UV<-melt(myData  %>%  select(Date,!!sym(Experiment_colum),!!sym(visitors)))
    UV<-myData  %>%  select(Date,!!sym(Experiment_colum),value=!!sym(visitors))
    UV<-UV %>% select(Date,!!sym(Experiment_colum),value)
    UV$Date<-as.Date(UV$Date)
    UV<-ggplot(data=UV,  aes(x=Date, y=value, colour=!!sym(Experiment_colum))) + geom_line() 
    #UV<-UV + geom_hline(yintercept = 0.5, colour = "blue") + geom_hline(yintercept = value_ss,size = .5, colour = colour_ss) + geom_vline(xintercept = as.Date(Date_ss),size = 1, colour = colour_ss) 
    UV<-UV +   xlab("Date") + ylab("Unique visitors") 
    
    conv<-myData %>% select(Date,!!sym(Experiment_colum),value=!!sym(conversion))
    conv$Date<-as.Date(conv$Date)
    conv<-ggplot(data=conv,  aes(x=Date, y=value, colour=!!sym(Experiment_colum))) + geom_line() 
    conv<-conv + xlab("Date") + ylab("Daily Conversions")
    
    #plot daily visitors
    visitors_plot<-Opt_stats %>% select(Date,!!sym(Experiment_colum),value=!!sym(gsub("cumsum\\((.*)\\)","\\1",visitors)))
    visitors_plot$Date<-as.Date(visitors_plot$Date)
    visitors_plot<-ggplot(data=visitors_plot,  aes(x=Date, y=value, colour=!!sym(Experiment_colum))) + geom_line() 
    visitors_plot<-visitors_plot + xlab("Date") + ylab("Daily Visitors") 
    
    #plot conversion rate
    cvr_plot<-Opt_stats %>% select(Date,!!sym(Experiment_colum),Users,value=!!sym(gsub("cumsum\\((.*)\\)","\\1",conversion)))
    cvr_plot$CVR<-cvr_plot$value/cvr_plot$Users
    cvr_plot<-cvr_plot %>% select(Date,!!sym(Experiment_colum),CVR)
    cvr_plot$Date<-as.Date(cvr_plot$Date)
    cvr_plot<-ggplot(data=cvr_plot,  aes(x=Date, y=CVR, colour=!!sym(Experiment_colum))) + geom_line() + scale_y_continuous(labels = function(x) paste0(x*100, "%")) 
    cvr_plot<-cvr_plot + xlab("Date") + ylab("Daily Conversion rate (%)")
    
    #plot daily vists
    visits_plot<-Opt_stats
    visits_plot<-visits_plot %>% select(Date,!!sym(Experiment_colum),Sessions)
    visits_plot$Date<-as.Date(visits_plot$Date)
    visits_plot<-ggplot(data=visits_plot,  aes(x=Date, y=Sessions, colour=!!sym(Experiment_colum))) + geom_line() 
    visits_plot<-visits_plot + xlab("Date") + ylab("Daily Visits") 
    
    #plot daily conversions    
    conv_plot<-Opt_stats %>% select(Date,!!sym(Experiment_colum),value=!!sym(gsub("cumsum\\((.*)\\)","\\1",conversion)))
    conv_plot$Date<-as.Date(conv_plot$Date)
    conv_plot<-ggplot(data=conv_plot,  aes(x=Date, y=value, colour=!!sym(Experiment_colum))) + geom_line() 
    conv_plot<-conv_plot + xlab("Date") + ylab("Daily Conversions")
    
    

    plottheData<-ggarrange(UV,conv,cvr_plot,sampleSize_def,visits_plot,conf_progress ,visitors_plot,conv_plot,
                           labels = c(paste("Cumulative Daily Visitors"),
                                      paste("Cumulative Daily",gsub("cumsum\\((.*)\\)","\\1",conversion)),
                                      paste("Daily conversion rate",gsub("cumsum\\((.*)\\)","\\1",conversion)),
                                      #paste("Uplift to reach",paste0((Confidence_level)*100,"% confidence") ,"for",gsub("cumsum\\((.*)\\)","\\1",conversion)),
                                      paste(""),
                                      paste("Daily Visits"),
                                      #paste("Level of confidence")
                                      paste("")
                                      ),
                           ncol = 2, nrow = 4)

    plot(plottheData)
    #plot_list[[j]] =plottheData
    
  }
}
 ########################################################################################################################################
if(writeToExcel=="YES"){
  dir.create(gsub(".*\\\\(.*).csv","\\1",file))
  
  #pdf(paste0(gsub(".*\\\\(.*).csv","\\1",file),".pdf"))
  pdf(paste0(gsub(".*\\\\(.*).csv","\\1",file),"\\",gsub(".*\\\\(.*).csv","\\1",file),".pdf"))
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()
}

if(write== TRUE){
  stats <- createWorkbook(creator = "Guillaume Lombard"
                         , title = "Optimisation and AB testing Workshop 20/09/2019"
                         , subject = "AB test calculator"
                         , category = "CRO")
  # Create a percent style
  pct = createStyle(numFmt="0.00%")
  # Add the percent style to the desired cells
  ifelse("Data analysed" %in% names(stats),"Exist",  addWorksheet(stats, sheetName = "Data analysed"))
  
  #addStyle(stats, sheet = "Data analysed", style=pct, cols=myData[ , grep( "%" , names( myData ) ) ], rows=2:(nrow(myData)+1), gridExpand=TRUE)
  #posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  #conditionalFormatting(wb=stats, sheet="Data analysed", cols=myData[ , grep( "confidence" , names( myData ) ) ], rows=2:(nrow(myData)+1), rule=" > 90", type = "expression",style = posStyle)
  writeData(stats, sheet = "Data analysed", myData, colNames = TRUE)
  
  saveWorkbook(stats, gsub(".csv$",paste0("_",Sys.Date(),".xlsx"),file),overwrite = TRUE)
  
}

