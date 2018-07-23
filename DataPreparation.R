#Change WD
getwd()
setwd("C:/Users/Utsav/Desktop/AdvancedR/Data Preparation")
getwd()
#Basic fin<-read.csv("Future500.csv")
fin<-read.csv("Future500.csv",na.strings = c(""))
head(fin,10)
tail(fin)
str(fin)

#convering non-factor to factor
fin$ID<-factor(fin$ID)
fin$Inception<-factor(fin$Inception)
str(fin)

#converting factor to non-factor(using gsub forpattern matching)
fin$Expenses<-gsub(" Dollars","",fin$Expenses)
fin$Expenses<-gsub(",","",fin$Expenses)
fin$Expenses<-as.numeric(fin$Expenses)

fin$Revenue <-gsub("\\$","",fin$Revenue)
fin$Revenue<-gsub(",","",fin$Revenue)
fin$Revenue<-as.numeric(fin$Revenue)

fin$Growth<-gsub("\\%","",fin$Growth)
fin$Growth<-as.numeric(fin$Growth)

head(fin)
str(fin)

#to get incomplete rows (by using which to filter data)
fin[!complete.cases(fin),]

#updated fin using fin<-read.csv("Future500.csv",na.strings = c(""))

#filtering using is.na()
fin[is.na(fin$State),]

#Remove records with missing data
fin_backup<-fin
fin<-fin[!is.na(fin$Industry),] #selecting which are not NAs
fin[!complete.cases(fin),] #two rows less now

#resetting row index

rownames(fin)<-1:nrow(fin)
tail(fin)
#OR
rownames(fin)<-NULL
tail(fin)

#Replacing missing data (factual analysis method)
fin[!complete.cases(fin$State),]
fin[is.na(fin$State) & fin$City=="New York","State"]<-"NY"
fin[is.na(fin$State) & fin$City=="San Francisco","State"] <- "CA"
fin[!complete.cases(fin),] #4 rows eliminated

#Replacing missing data (median imputation method)
#->Employees
fin[is.na(fin$Employees),]
median(fin[fin$Industry=="Retail","Employees"],na.rm = TRUE)
med_retail_emp<-median(fin[fin$Industry=="Retail","Employees"],na.rm = TRUE)
fin[is.na(fin$Employees)&fin$Industry=="Retail",]
fin[is.na(fin$Employees)&fin$Industry=="Retail","Employees"]<-med_retail_emp
fin[3,]

fin[is.na(fin$Employees),]
median(fin[fin$Industry=="Financial Services","Employees"],na.rm=TRUE)
med_finserv_emp<-median(fin[fin$Industry=="Financial Services","Employees"],na.rm=TRUE)
fin[is.na(fin$Employees)&fin$Industry=="Financial Services",]
fin[is.na(fin$Employees)&fin$Industry=="Financial Services","Employees"]<-med_finserv_emp
fin[330,]
#->Growth
med_constr_growth<-median(fin[fin$Industry=="Construction","Growth"],na.rm=TRUE)
fin[is.na(fin$Growth) & fin$Industry=="Construction","Growth"]<-med_constr_growth
fin[8,]

fin[!complete.cases(fin),] #5 rows eliminated
#->Revenue
med_constr_rev<-median(fin[fin$Industry=="Construction","Revenue"],na.rm=TRUE)
fin[is.na(fin$Revenue) & fin$Industry=="Construction","Revenue"]<-med_constr_rev
fin[c(8,42),]
#->Expenses
med_constr_exp<-median(fin[fin$Industry=="Construction","Expenses"],na.rm=TRUE)
fin[is.na(fin$Expenses) & fin$Industry=="Construction" & is.na(fin$Profit),"Expenses"]<-med_constr_exp
fin[c(8,42),]
fin[!complete.cases(fin),]

#using Profit=Revenue-Expenses
fin[is.na(fin$Profit),"Profit"]<-fin[is.na(fin$Profit),"Revenue"]-fin[is.na(fin$Profit),"Expenses"]
fin[c(8,42),]
fin[!complete.cases(fin),]
fin[is.na(fin$Expenses),"Expenses"]<-fin[is.na(fin$Expenses),"Revenue"]-fin[is.na(fin$Expenses),"Profit"]
fin[15,]
fin[!complete.cases(fin),] # all missing eliminated/replaced(Inception we can leave, not important)

#Visualize
library(ggplot2)
p<-ggplot(data=fin,aes(x=Revenue/100000,y=Expenses/100000,colour=Industry,size=Profit))+geom_point()
p
d<-ggplot(data=fin,aes(x=Revenue/100000,y=Expenses/100000,colour=Industry))+geom_point()+geom_smooth(fill=NA,size=1.2)
d
f<-ggplot(data=fin,aes(x=Industry,y=Growth,colour=Industry))
f+geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.colour = NA)
