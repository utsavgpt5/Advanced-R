#Set WD
getwd()
setwd("\\\\uk/pwc/Citrix/UK-Data/936390/Documents/AdvancedR/Apply/Weather Data")

#Data Preparation
Chicago<-read.csv("Chicago-F.csv",row.names = 1)
NewYork<-read.csv("NewYork-F.csv",row.names = 1)
Houston<-read.csv("Houston-F.csv",row.names = 1)
SanFrancisco<-read.csv("SanFrancisco-F.csv",row.names = 1)

#using matrix and putting in a list 
Chicago<-as.matrix(Chicago)
NewYork<-as.matrix(NewYork)
Houston<-as.matrix(Houston)
SanFrancisco<-as.matrix(SanFrancisco)

Weather<-list(Chicago=Chicago,NewYork=NewYork,Houston=Houston,SanFrancisco=SanFrancisco)
Weather

#apply-applies to matrix and returns vector
apply(Chicago,1,mean)
apply(NewYork,1,mean)
apply(Houston,1,mean)
apply(SanFrancisco,1,mean)

cbind(Chiago=apply(Chicago,1,mean),NewYork=apply(NewYork,1,mean),
      Houston=apply(Houston,1,mean),SanFrancisco=apply(SanFrancisco,1,mean))
              #<<< Deliverable 1, but there is a faster approach

#using loops instead of apply
output<-NULL
for(i in 1:nrow(Chicago)){
  output[i]<-mean(Chicago[i,])
}
names(output)<-rownames(Chicago)
output

#lapply-applies to list and returns list
t(Chicago)   #transpose
lapply(Weather,t)    #applying transpose to every element in the list
lapply(Weather,rbind,NewRow=1:12) # parameter of the function is specified as 3rd arguement
rowMeans(Chicago)
lapply(Weather,rowMeans)  #gives row mean for each city in the list

#other funs: rowMeans,colMeans,rowSums,colSums
                          #<<< Deliverable 1, but there is a faster approach

#using lapply with []
Weather
lapply(Weather,"[",1,)        #avg high temp for all months
lapply(Weather,"[",1,1)       #avg high temp for jan
lapply(Weather,"[",1,3)       #avg high temp for mar
lapply(Weather,"[",,3)        #avg stats for mar

#Adding own functions
Weather
lapply(Weather,function(x) x[1,])   
lapply(Weather,function(x) x[,1])
lapply(Weather,function(x) x[1,]-x[2,])   #diff in low and high temp
lapply(Weather,function(x) round((x[1,]-x[2,])/x[2,],2)) #variation in low and high temp with respect to low temp
                                #<<<deliverable 2 temp fluctuations-Will Improve

#sapply-wrapper of lapply and returns simplified output
lapply(Weather,"[",1,7)          # returns list-avg high temp for july
sapply(Weather,"[",1,7)          # returns vector-avg high temp for july
lapply(Weather,"[",1,10:12)      # returns list-avg high temp for last quarter
sapply(Weather,"[",1,10:12)      # returns matrix-avg high temp for last quarter

lapply(Weather,rowMeans)          #list
round(sapply(Weather,rowMeans),2) #matrix  
#<<Deliverable 1:Ready>>

lapply(Weather,function(x) round((x[1,]-x[2,])/x[2,],2)) #temp fluctuation-list
sapply(Weather,function(x) round((x[1,]-x[2,])/x[2,],2)) #temp fluctuation-matrix
#<<Deliverable 2:Ready>>
sapply(Weather,rowMeans,simplify = FALSE)   #same as lapply-can use simplify=list,matrix,vector,etc.

#using apply within lapply/sapply
sapply(Weather,apply,1,max)   #<<Deliverable 3:Ready>>  
sapply(Weather,apply,1,min)   #<<Deliverable 4:Ready>>

#which.max/which.min-nesting apply
names(which.max(Chicago[1,]))
apply(Chicago,1,function(x) names(which.max(x)))
sapply(Weather,function(y) apply(y,1,function(x) names(which.max(x)))) #cities with max of the properties in the list

