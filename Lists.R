#set WD
#setwd("C:/Users/Utsav/Desktop/AdvancedR/Lists")
setwd("\\\\uk/pwc/Citrix/UK-Data/936390/Documents/AdvancedR/Lists")

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

list_RL1<-list(Machine="RL1",Stats=util_stats_RL1,LowThreshold=util_under_90_flag)
list_RL1                 #List constructed

#Extract components of a list
list_RL1
list_RL1[1]         #list type
list_RL1[[1]]       #vector type ('[]' are used to access items/subsetting (of) list)
list_RL1$Machine    #vector type ('[[]]' are used to access components in the list)
list_RL1$Stats[3]
list_RL1[[2]][3]

#Adding and Deleting components
list_RL1[7]<-"xyz"  #adding
list_RL1[4:7]<-NULL #deleting  (index shifts automatically)
list_RL1$UnknownHours<-RL1[is.na(RL1$Utilization),"PosixTime"]
list_RL1

#Add dataframe for this machine
list_RL1$Data<-RL1

summary(list_RL1)
str(list_RL1)

#subset list
list_RL1[1:3]

#Building Time-Series Plot
library(ggplot2)
p<-ggplot(data=util)
myplot<-p+geom_line(aes(x=PosixTime,y=Utilization,colour=Machine),size=1.2)+facet_grid(Machine~.)+geom_hline(yintercept = 0.9,lty=4,size=1.2,col="dark grey")
list_RL1$Plot<-myplot
summary(list_RL1)









