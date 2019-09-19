'
Do you want to write output to Excel? 1 for YEs, 2 for No
'
writeToExcel=c("YES","NO")[1]

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
Max_duration<-500
#Alternative - default two.sided, the number 1 selects the index position for two.sided, 2 for less etc...
Alternative=c("two.sided","less","greater")[1]
#How many standard deviation from the mean do you want to use (default 2)
number_of_sd_from_mean<-2




#load librairies
PKG<-c("dplyr","strucchange","ggpubr","readr","rlang","ggplot2","reshape2","pwr")
for (pkg in PKG){
  if (pkg %in% rownames(installed.packages()) == FALSE)  {install.packages(pkg, character.only = TRUE)} else {require(pkg,character.only = TRUE)}
}
###Calculate ideal sample size required to reach Statistical Significance#######################################################
####Fear factor/thrilling/exciting/safe
Sample_size_calculator<-function(Experiment_number,Control_conversion_rate,Daily_visitors,Statistical_Power,Confidence_level,Max_duration,Alternative,number_of_sd_from_mean){
  Control_conversion_rate<-Control_conversion_rate/100
  Confidence_level<-1-(Confidence_level/100)
  Statistical_Power<-Statistical_Power/100
  Daily_visitors<-Daily_visitors/Experiment_number
  
  #rm(sampleSize)
  sampleSize <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(sampleSize) <- c("Day","Volume_of_participants_required_in_each_group", "Control_conversion_rate", "Challenger_conversion_rate","Uplift","Confidence_level","Statistical_Power","Alternative","CI_left","CI_right")
  i_days<-NULL
  #if(round(UV_control/(as.numeric(myData$Day[i] - min(myData$Day))+1)) < 1 ) {Daily_visitors<-1} else{Daily_visitors<-round(UV_control/(as.numeric(myData$Day[i] - min(myData$Day))+1))}

  
  counter = 1
  for (i_days in seq(Daily_visitors,Daily_visitors*Max_duration, by=Daily_visitors)){
    if(i_days<2 && Control_conversion_rate== 1){i_days<-2
    controlCR<-.99999} else {controlCR<-Control_conversion_rate}
    mde_sampleSize<-power.prop.test(p1=controlCR, n=i_days, power=Statistical_Power, sig.level=Confidence_level)
    #print(round(mde_sampleSize$p1*100,2))
    error <- qnorm(0.975)*number_of_sd_from_mean/sqrt(i_days)
    sampleSize<-bind_rows(sampleSize,data.frame(Day=counter,Volume_of_participants_required_in_each_group=mde_sampleSize$n,Control_conversion_rate=mde_sampleSize$p1
                                                ,Challenger_conversion_rate=mde_sampleSize$p2,Uplift=(mde_sampleSize$p2-mde_sampleSize$p1)/mde_sampleSize$p1,
                                                Confidence_level=mde_sampleSize$sig.level,Statistical_Power=mde_sampleSize$power,Alternative=mde_sampleSize$alternative
                                                ,CI_left=controlCR-error,CI_right=controlCR+error))
    counter = counter +1
  }


  sampleSize_chart<-breakpoints(sampleSize$Challenger_conversion_rate~1)
  sampleSize$Challenger_conversion_rate<-sampleSize$Challenger_conversion_rate*100
  sampleSize$Uplift<-sampleSize$Uplift*100
  sampleSize$Control_conversion_rate<-sampleSize$Control_conversion_rate*100
  sampleSize<<-sampleSize  
  sampleSize_def <-ggplot(sampleSize %>% select(Volume_of_participants_required_in_each_group,Control_conversion_rate,Challenger_conversion_rate), aes(x=Volume_of_participants_required_in_each_group, y=Challenger_conversion_rate, col=Control_conversion_rate)) + geom_line() +
    geom_hline(yintercept =controlCR*100 , size = 1, colour = "blue")
  sampleSize_def<-sampleSize_def +
    geom_vline(xintercept = max(sampleSize$Volume_of_participants_required_in_each_group[sampleSize_chart$breakpoints]), size = 1, colour = "red") + 
    #geom_vline(xintercept = min(sampleSize$Volume_of_participants_required_in_each_group[sampleSize_chart$breakpoints]), size = 1, colour = "black") + 
    geom_hline(yintercept = min(sampleSize$Challenger_conversion_rate[sampleSize_chart$breakpoints]), size = 1, colour = "green")

  
  sampleSize_def_day <-ggplot(sampleSize %>% select(Day,Control_conversion_rate,Uplift), aes(x=Day, y=Uplift, col=Control_conversion_rate)) + geom_line() +
    geom_hline(yintercept =controlCR*100 , size = 1, colour = "blue")
  sampleSize_def_day<-sampleSize_def_day +
    geom_vline(xintercept = max(sampleSize$Day[sampleSize_chart$breakpoints]), size = 1, colour = "red") + 
    #geom_vline(xintercept = min(sampleSize$Day[sampleSize_chart$breakpoints]), size = 1, colour = "black") + 
    geom_hline(yintercept = min(sampleSize$Uplift[sampleSize_chart$breakpoints]), size = 1, colour = "green")
  
  
  plottheData<-ggarrange(sampleSize_def,sampleSize_def_day ,
                                                 labels = c("Challenger CR per Sample size", "Uplift per day"),
                                                 ncol = 1, nrow = 2)
  return(list(plottheData,
  print(paste0("Minimum number of days required for this test: ",sampleSize[[max(sampleSize_chart$breakpoints),"Day"]]))
  ,print(paste0("Volume of participants required in each experiment: ",sampleSize[[max(sampleSize_chart$breakpoints),"Volume_of_participants_required_in_each_group"]]))
  ,print(paste0("Challenger conversion rate required to reach statistical significance:  ",round(sampleSize[[max(sampleSize_chart$breakpoints),"Challenger_conversion_rate"]],2),"%"))
  ,print(paste0("Uplift required to reach statistical significance: ",round(sampleSize[[max(sampleSize_chart$breakpoints),"Uplift"]],2),"%"))
  ))

  }

Sample_size_calculator(Experiment_number,Control_conversion_rate,Daily_visitors,Statistical_Power,Confidence_level,Max_duration,Alternative,number_of_sd_from_mean)
Sys.sleep(2)
View(sampleSize)

if(write== TRUE){
  path<-choose.dir(default = "", caption = "Select folder to save file")
  setwd(path)
  stats <- createWorkbook(creator = "Guillaume Lombard"
                          , title = "Optimisation and AB testinng Workshop 20/09/2019"
                          , subject = "Sample Size Calculator"
                          , category = "CRO")
  # Create a percent style
  pct = createStyle(numFmt="0.00%")
  # Add the percent style to the desired cells
  ifelse("Sample size" %in% names(stats),"Exist",  addWorksheet(stats, sheetName = "Sample size"))
  
  #addStyle(stats, sheet = "Data analysed", style=pct, cols=myData[ , grep( "%" , names( myData ) ) ], rows=2:(nrow(myData)+1), gridExpand=TRUE)
  #posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  #conditionalFormatting(wb=stats, sheet="Data analysed", cols=myData[ , grep( "confidence" , names( myData ) ) ], rows=2:(nrow(myData)+1), rule=" > 90", type = "expression",style = posStyle)
  writeData(stats, sheet = "Sample size", sampleSize, colNames = TRUE)
  
  saveWorkbook(stats, paste0("Sample_size_",Sys.Date(),".xlsx"), overwrite = TRUE)
}
