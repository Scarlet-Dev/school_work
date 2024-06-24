#Clear environment
rm(list=ls())

#Clear plots
dev.off()

#Set working directory
setwd("C:\\Users\\Owner\\Documents\\ANLY\\EDA\\Lectures\\Draft 1st Executive Fall18")

###Variable Types###

#Slide 44
#Define vectors of 4 variable types
#Numeric
a=c(2,4,6,8)
class(a)
#Character
b=c("Joe","Bob","Jim","Tom")
class(b)
#Logical
c=c(T,F,F,T)
class(c)
#Factor
d=factor(c("big","small","small","big"))
class(d)

#Slide 46
#Combine into dataframe
#The vectors all need to be the same length in order to do this.
df=data.frame(a,b,c,d)
glimpse(df)
#Use stringsAsFactors=F to read in strings as characters instead of factors.
df2=data.frame(a,b,c,d,stringsAsFactors=F)
glimpse(df2)

#Slide 47
#Change variable types
#Convert d to character
df2$d=as.character(df2$d)
glimpse(df2)
#Convert back to factor
df2$d=as.factor(df2$d)
glimpse(df2)

#Slide 48
#########Exercise##########

###Missing Values####

#Slide 50
#Define vector
x=c(1,2,NA,4)
#NAs will get in the way of normal operations
sum(x)
#Find NAs
is.na(x)
#Find number of NAs for a variable
sum(is.na(x))
y=c(2,2,3,4)
df=data.frame(x,y)
#Find number of NAs for a dataset
summary(df)

#Slide 52
colSums(df)
##1.Delete
#Subset to remove NAs
colSums(na.omit(df))
##2.Ignore
#Ignore NAs in calculation
colSums(df,na.rm=T)
##3.Replace
#Replace with average
df$x[is.na(df$x)]=mean(na.omit(df$x))
df$x
colSums(df)

#Slide 53
#########Exercise##########

###Performing Operations Across Variables and Across Datasets###

#Slide 55
#Across Variables
X=c(0.1,0.7,0.3,0.9)
HighLow=ifelse(X>0.5,"High","Low")
HighLow

#Slide 56
#Across Datasets
glimpse(df2)
df3=lapply(df2,function(x)as.character(x))
df3=data.frame(df3,stringsAsFactors = F)
glimpse(df3)

#Slide 57
#########Exercise##########
c