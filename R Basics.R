
# Data Manipulation Codes

read.csv(choose.files())
Car_data_cf =read.csv("C:/Users/nikunj.kumar/Desktop/R Study Material & Codes/Practice Datasets/Car_data_cf.csv")

sessionInfo()
a=15
b=12
c=a*b
d<-15
cat("Value of C is:", c)
cat(a,b,"Value of C",c)                                       # cat/paste is used for concatenation               

x<- 10
12*15 ->y
typeof(x)
x=as.character(x)
z=as.numeric(x)


# Working with Dates

date1="25MAY2016"
date2="25/05/2017"
date3="25-06-2017"
date4=as.Date(date1,format="%d%b%Y")                          # as.(function) is used to convert
date5=as.Date(date1,format="%d%B%Y")
date6=as.Date(date2,format="%d/%m/%Y")
date7=as.Date(date3,format="%d-%m-%Y")
require(xlsx)
require(rJava)

getwd()
setwd("C:/Users/nikunj.kumar/Documents/R/Files")
# Importing multiple files

#First set a working directory

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)             # lapply & other apply functions are alternatives for loops.

# OR

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)



# Working with dataframes


# Creating a Data frame

# table with the same type within a column and different types between columns
# defined with a data.frame() function
my_df <- data.frame(id = c(1, 2, 3), 
                    name = c("Analytics", "Labs", "alabs"), 
                    Goals = c(50, 49, 25))
dim(my_df)
str(my_df)
summary(my_df)



# Working on Datasets

Car_data_cf = read.csv(choose.files())
str(Car_data_cf)                                      # str -> for determining structure of data-type
dim(Car_data_cf)
Car_data_cf$RPM
summary(Car_data_cf)
class(Car_data_cf$MPGcity)                            # dollar is used to work with characters
p=as.character(Car_data_cf$Manufacturer)
rm(p)                                                 # rm is used to remove
Col_Names=names(Car_data_cf)
Col_Names[Col_Names=="Model"]


# Renaming column

Col_Names=names(Car_data_cf)
Col_Names[Col_Names=="MPGcity"]="CityMileage"
names(Car_data_cf)=Col_Names
names(Car_data_cf)

library(reshape)                                     # Using reshape package
mydata=rename(Car_data_cf,c(Manufacturer="NewManufact"))
names(mydata)

fix(mydata)
new=mydata
rm(new)

# Or use like this names(x)[1:n]=c("a","b",....,"n")



# Adding a column

vector1= c(seq(1:16))
mydata$new_column= vector1
fix(mydata)                                           # Change Data
mydata$sumMPG=mydata$MPGhighway + mydata$CityMileage


# Removing the added column

mydata$sumMPG= NULL
mydata$NewColumn=NULL

v1=c(1:5)
v2=c("Aman","Nikunj","Ujjwal","Snigdh","Rajput")
v3=c("Analyst","Business Analyst","Businessman","Research","Trainee")
newdata=c(v1,v2,v3)
newdata1=data.frame(v1,v2,v3)
mydata[,"Type"]
newdata1[,"v3"]
mydata[,c("Type","Model")]


# Reshuffling columns

newdata2=mydata[,c(1,15,14,5,11,8,20,10,13)]

require(plyr)
input = input[,order(colnames(input),decreasing = T)]


# Subsetting

subsetdata=subset(mydata,select =c("Model","Type") )
subsetdata1=subset(mydata,select = c(1,4,6,7))

filterdata=subset(mydata,select = c("Model","Horsepower"),Horsepower>100)     # Filtering Data



# Sorting Data

sortdata=Car_data_cf[order(Car_data_cf$CityMileage),]                        # Using Order
sortdata1=Car_data_cf[order(Car_data_cf$CityMileage,decreasing = TRUE),]
library(dplyr)
sortdata2=arrange(Car_data_cf, Model)                                        # Using Arrange



# Checking for duplicates

duplicated(stores)
rm(subsetdata1)

# Unique function is used to get unique values

factor1=as.factor(Car_data_cf$Manufacturer)

Demographic_Data = read.csv(choose.files())
Transaction_Summary = read.csv(choose.files())



# Merging data

innermerge=merge(x=Demographic_Data,y=Transaction_Summary,by.x=c("CustName"),by.y=c("CustomerName"),all=F)
fullmerge=merge(x=Demographic_Data,y=Transaction_Summary,by.x=c("CustName"),by.y=c("CustomerName"),all=T)
leftmerge=merge(x=Demographic_Data,y=Transaction_Summary,by.x=c("CustName"),by.y=c("CustomerName"),all.x=T)
rightmerge=merge(x=Demographic_Data,y=Transaction_Summary,by.x=c("CustName"),by.y=c("CustomerName"),all.y =T)

# Detecting N/A
is.na(mydata)
nmiss = nrow(is.na(mydata))

storesdata=read.csv(choose.files())



# Univariate Analysis


tb=table(storesdata$Location)                             #rbind is used for appending by rows
proportion_tb=prop.table(tb)                              #cbind is used for appending by columns
rbind(tb,proportion_tb)
cbind(tb,proportion_tb)

# Getting Frequency of categorical variables

freq_data= data.frame(table(input_data$Institution_Long_Name))

library(plyr)
freq_data.1 = count(input_data,"Institution_Long_Name")


# Note: rbind.fill(plyr) & cbind.fill(rowr) can be used when lengths are different.

binary_tb=table(storesdata$Location,storesdata$StoreType)                # two-way table
require(gmodels)
CrossTable(storesdata$Location,storesdata$StoreType)                     # similar to SAS

require(psych)
describe(storesdata)
min(storesdata$TotalSales)
max(storesdata$TotalSales)
quantile(storesdata$TotalSales)
quantile(storesdata$TotalSales,probs = c(0.01,0.05,0.1,0.2,0.3,0.5,0.7,0.99))



# Creating own function

summary_var=function(x){
  totalobs=length(x)
  sumobs=sum(x,na.rm = T)
  meanobs=mean(x,na.rm = T)
  minobs=min(x,na.rm = T)
  maxobs=max(x,na.rm=T)
  return(c(TotalObservation=totalobs,Sum=sumobs,Average=meanobs,Minimum=minobs,Maximum=maxobs))
}

summary=summary_var(storesdata$TotalSales)                     # Using own function created

numerical_stores=storesdata[,5:15]
apply(numerical_stores,2,summary_var)    # apply is used to avoid loops(they are faster than loops)
# 1 means apply to rows, 2 means apply to columns, c(1,2) means apply to both rows & columns.

v1=c(1:15)
matrix_apply=matrix(v1,nrow = 5,ncol = 3)
apply(matrix_apply,1,mean)
apply(matrix_apply,2,mean)


aggregate(storesdata$TotalSales~storesdata$Location,FUN = summary_var)
aggregate(storesdata$TotalSales~storesdata$Location,FUN = mean)
# ~ is used to find relation between two objects
# aggregate is used for applying a function over a formula.


# Applying rbind to multiple files, ie. aggregating multiple files into one single file

files <- list.files(pattern="*.csv")
DF <- NULL
for (f in files) {
  dat <- read.csv(f)
  #dat$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  DF <- rbind(DF, dat)
}




# Introduction to PLOTS


# Scatter Plots

x1=c(10:20)
plot(x1)
y1=c(12,34,55,66,33,44,77,88,99,11,22)

plot(x=x1,y=y1,
     main="Forecast Results",
     xlab = "Month",ylab = "Production",
     col=c("red","black","green"))
plot(x=x1,y=y1,
     main="Forecast Results",                           # l type is used for lines
     xlab = "Month",ylab = "Production",                # p type for points
     col=c("red","black","green"),                      # b for both
     type="l")                                          # h for histogram like vertical lines
plot(x=x1,y=y1,
     main="Forecast Results",
     xlab = "Month",ylab = "Production",
     col=c("red","black","green"),                     #add-ons work only after recent/newest plot
     type="b") 

grid()                                                 #grid and abline are add-ons.
abline(a=0,b=1)                                        #abline is used to add straight line.

plot(x=storesdata$TotalSales,y=storesdata$OperatingCost,
     main="Forecast Results",
     xlab = "Total Sales",ylab = "Operating Cost",
     col=c("red","black","green"),
     type="b")

x3=seq(1,10,by=0.5)
y3=c(21:39)
plot(x3,y3)
plot(x3,y3,type="l")



# Bar Charts for single variable

storetype=table(storesdata$StoreType)
barplot(storetype)
barplot(storetype,horiz = TRUE,main="Barplot")
barplot(storetype,width = c(0.25,0.15,0.40))
barplot(storetype,
        horiz=TRUE,
        main="Barplot of Storetype",
        col=c("red","blue","black"),
        xlab="Type of Storetype",
        ylab="Frequency")



# Barplot for multivariate

OC=storesdata$OperatingCost
sales=storesdata$TotalSales

barplot(OC,sales,
        main="Sales vs OperatingCost",
        xlab="Operating Cost",
        ylab = "total sales",
        cex.lab=1.4,cex.main=2.0,                       # cex is used for font size
        beside=FALSE,col=colors())

stores1=storesdata[,5:15]
stores2=stores1[1:5]
barplot(as.matrix(stores2),
        main="barchart",
        ylab="Numbers",
        col=colors())

?barplot


# Pie Charts

x=c(10,37,45,23,56)
y=c("A","B","c","D","E")
pie(x,y)

pie_table=table(storesdata$StoreSegment)
pie(pie_table)
pie_percent=prop.table(pie_table)                       # paste is used to concatenate
pie(pie_table,main="PieChart for Bivariate",            # paste0 concatenates without space
    labels = paste0(names(pie_table),
                    "\n","(",pie_percent*100,"%)"))




# Univariate Histogram

# Histogram represents the distribution of numerical data or frequency distribution
# It is basically a distribution of a continuous variable

hist(storesdata$TotalSales,freq=F,col="blue")


data(lynx)
hist(lynx)
hist(lynx,
     breaks = 11,                                       # modifies the breaks/adds up the bins
     freq = TRUE,                                       # freq=FALSE draws normal distribution
     main = "Histogram of Lynx\n1821-1934",             # \n is used to enter the new line
     xlab = "No of Lynx trapped")                       # freq=TRUE used fordensity distribution



# Boxplot

boxplot(USJudgeRatings$RTEN)



# Stratified Sampling

# User-defined function for stratified sampling

stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}

#ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")