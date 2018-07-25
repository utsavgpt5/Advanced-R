#getting data-objective to predict G3
install.packages("corrgram")
install.packages("corrplot")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)
library(caTools)
#https://archive.ics.uci.edu/ml/datasets/Student+Performance  data info
setwd("\\\\uk/pwc/Citrix/UK-Data/936390/Documents/AdvancedR/LR")
df<-read.csv2('student-mat.csv')
or
df<-read.csv('student-mat.csv',sep=";")
summary(df)
any(is.na(df))  # to check NA in dataset
str(df)

#numeric data only
num.cols<-sapply(df,is.numeric)
cor.data<-cor(df[,num.cols])  #correlation

#plot corrplot-only for numeric columns
corrplot(cor.data,method = 'color')

#plot corrgram-we can pass entire df
corrgram(df)
corrgram(df,df=T,lower.panel=panel.shade,upper.panel=panel.pie,text.panel = panel.txt)    #better looking
ggplot(data=df,aes(x=G3))+geom_histogram(bins=20,alpha=0.5,fill="red")        #EDA on G3

#split data into train and test using caTools packages
set.seed(101) #same random numbers for all if seed is same
sample<-sample.split(df$G3,SplitRatio = 0.7)  #we can use any column name-contains T/F
train<-subset(df,sample==T)  #70% training data
test<-subset(df,sample==F)  #30% testing data

#train and build data
model<-lm(G3~.,data=train)
summary(model)
res=residuals(model)
res<-as.data.frame(res)
head(res)
ggplot(res,aes(res))+geom_histogram(fill='blue',alpha=0.5)
plot(model)

#predictions
G3.predictions<-predict(model,test)
results<-as.data.frame(cbind(Predicted=G3.predictions,Actual=test$G3))
head(results)





