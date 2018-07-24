#working with dplyr package
library(nycflights13)
library(dplyr)

filter(flights,month==11)    #filter rows

distinct(select(flights,carrier))
select(flights,carrier)%>%group_by(carrier)%>%summarize(count=n())   #select columns

head(rename(flights,Air_carrier=carrier)) #rename old column name

head(mutate(flights,new_col=arr_delay-dep_delay))  #mutate creates new column
head(transmute(flights,new_col=arr_delay-dep_delay)) #transmute returns only the new column created 

summarise(flights,avg=mean(air_time,na.rm=T))   #summarise

sample_n(flights,10)  #random 10 rows returned
sample_frac(flights,0.10) #fraction of all rows

#working with tidyrpackage
library(tidyr)
#creating data
comp<-c(1,1,1,2,2,2,3,3,3)
yr<-c(1998,1999,2000,1998,1999,2000,1998,1999,2000)
q1<-runif(9,min=0,max=100)
q2<-runif(9,min=0,max=100)
q3<-runif(9,min=0,max=100)
q4<-runif(9,min=0,max=100)
df<-data.frame(comp=comp,year=yr,Qtr1=q1,Qtr2=q2,Qtr3=q3,Qtr4=q4)
#1 gather
gather(df,Quarter,Revenue,Qtr1:Qtr4)

#2 spread
#first we gather
stocks<-data.frame(time=as.Date('2009-01-01')+0:9,X=rnorm(10,0,1),Y=rnorm(10,0,2),Z=rnorm(10,0,4))
stocks
stocks.gathered<-gather(stocks,stock,price,X,Y,Z)
stocks.gathered
#now spreading
stocks.spread<-spread(stocks.gathered,stock,price)
stocks.spread

#3 separate
df<-data.frame(new.col=c(NA,"a.x","b.y","c.z"))
df 
separate(df,col=new.col,into=c('ABC','XYZ'))
or
df1<-data.frame(new.col=c(NA,"a-x","b-y","c-z"))
df2<-separate(df1,col=new.col,into=c('ABC','XYZ'),sep='-')
df2

#4 unite
unite(df2,new.joined.col,ABC,XYZ,sep="%")

