#set WD
setwd("C:/Users/Utsav/Desktop/AdvancedR/Lists")

#load file
util<-read.csv("Machine-Utilization.csv")
str(util)
summary(util)

#adding utilization column
util$Utilization<-1-util$Percent.Idle

#converting date
util$PosixTime<-as.POSIXct(util$Timestamp,format="%d/%m/%Y %H:%M")
util$Timestamp<-NULL      #removing old time column
util<-util[,c(4,1,2,3)]   #rearranging columns
head(util,15)

#subsetting for machine RL1
RL1<-util[util$Machine=="RL1",]
summary(RL1)
RL1$Machine<-factor(RL1$Machine)           #not necessary
summary(RL1)

#Construct List
#Character :Machine Name
#Vector    :(min,mean,max) Utilization for the month (excluding unknown hours)
#Logical   :Has utilization ever fallen below 90% (TRUE/FALSE)
util_stats_RL1<-c(min(RL1$Utilization,na.rm=T),mean(RL1$Utilization,na.rm=T),max(RL1$Utilization,na.rm=T))
util_stats_RL1

util_under_90_flag<-length(which(RL1$Utilization<0.9))>0
util_under_90_flag

list_RL1<-list("RL1",util_stats_RL1,util_under_90_flag)
list_RL1                 #List constructed









