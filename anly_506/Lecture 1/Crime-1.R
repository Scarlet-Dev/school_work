#cntrl enter run
#cnrl l clear console
#cnrl s save

#set working directory-folder where all files come to/from R
help(setwd)
setwd("C:/Users/Owner/Documents/Tommy")
#get students comfortable with R studio environment. Discuss what each window is. Run simple commands and get response
#TBA-simple example ie. x=1+2 etc

#Explain packages
#Install packages only once on your computer
install.packages("dplyr")
install.packages("datasets")
install.packages("ggplot2")
# we use it everytime we open RStudio you need to load them again
library("ggplot2")
library("datasets")
library("dplyr")
# to download our dataset
read.csv("crimedata.csv")
# to read it in the work space
crime=read.csv("crimedata.csv")
#our dataset has 2215 rows/observations and 147 columns/variables
#to start cleaning lets run glimpse to give us an overview of our dataset.
#glimpse tells us what all of our variables are, what variable type they are, and the first few values
#str is used to accomplish the same thing as glimpse. so str is another option
glimpse(crime)
str(crime)
summary(crime)
#rename one of the variables (data cleaning)
crime=rename(crime,communityname=???communityname)


#----------------Unique Identifier-Check for Duplicates and Missing Values--------------------------------#
#quick check for duplicates. create dataset a of unique observations in crime.
#if number of observations in a = number of observations in crime then we have no duplicates. move on.
#a has 2018 observations so we know we need to investigate duplicates further.
a=unique(crime$communityname)
a=as.data.frame(a)
#to investigate further we use table. This tells us number of duplicates for every observation.
table=data.frame(table(crime$communityname))
#subset this dataset to be only the duplicates (frequency not equal to 1)
duplicates=table[table$Freq!=1,]
#below is an example of %in%
x=c(1,2,3,4)
y=c(1,1,7,3)
y%in%x
#use %in% to subset crime to keep only duplicated values for city name
crime_dups=crime[crime$communityname%in%duplicates$Var1,]
#use arrange to sort alphabetically
crime_dups=arrange(crime_dups,communityname)
#this led us to the conclusion that cityname alone is not a good unique identifier
#this is because muliple states can have the same city name
#below we create our own unique identifier by concatenating cityname and state
crime$citystate=paste(crime$communityname,crime$state)
glimpse(crime)
#start the process over. use unique as a quick check for duplicates.
#2215 values for b means there are no duplicates for our variable citystate. move on.
b=unique(crime$citystate)





#we look at the data and see the dbl,Int and factors
#in data cleaning, a key goal is to make sure that data is represented in R as it was intended to be represented
#explain variable types (numeric, character, logical, factor)
#look at attribute information on data website, tells us the 5 factor variables we have. we see that we have extra factor variables.
#convert variable type, lets give this example

a=2
#use class to ask R for the variable type ( run a, then class )
class(a)
# to change to character use as.character ( u can use as.datafram to change list to dataframe)
b=as.character(a)
class(b)
c=as.logical(a)
class(c)
d=as.factor(a)
class(d)
#now how to change the factors in crime dataset to numeric except the real factors
#lets do it first on one variable from Fac to Num
#to refer to a variable in a dataset, use dataset$variable name
class(crime$rapes)
crime$rapes=as.numeric(crime$rapes)
class(crime$rapes)
#Example of using sapply and Lapply
a=c(1,2,3)
#sapply returns vector
b=sapply(a,function(x) x*2)
#lapply returns list
c=lapply(a,function(x) x*2)
# now lets do this on our dataset
# we will creat crime2 inorder to compare  esp when using glimpse
crime2=crime
ncol(crime)
nrow(crime)
#in R if you change factor to numeric, sometimes it creats hidden issues like errors, 
#so the best way is to change factors to characters firs then change characters to numeric
crime2[,6:ncol(crime2)]=sapply(crime2[,6:ncol(crime2)], as.character)
str(crime2)
crime2[,6:ncol(crime2)]=sapply(crime2[,6:ncol(crime2)], as.numeric)
#lets run warning , what we see here NAS intro by coercion ( force), it took the ? and could not replace it by a number so it put N/A instead

warnings()
# str doesnt show the N/A the missing variables ( which is confusing)but glimpse does
str(crime2)
glimpse(crime2)
crime=crime2
rm(crime2)
# lets introduce missing values and how to do with it, lts creat a= vector c....
a=c(1,2,3,NA)
mean(a)
sum(a)
is.na(a)
!is.na(a)
#count number of missing values
sum(is.na(a))
mean(a,na.rm=TRUE)
mean(crime$ViolentCrimesPerPop)
mean(crime$ViolentCrimesPerPop,na.rm = TRUE)

#we have the mean NA bec we have missing values, inorder to fix the NA result of the mean


is.na(crime$ViolentCrimesPerPop)

# is . na means , it returns the value True for each missing value and False for each other value
mean(!is.na(crime$ViolentCrimesPerPop))


#----------Crimeset example Questions--------------------#
summary(crime)






max(crime$population)
min(crime$population)
mean(crime$medIncome)
sum(crime$population)
sum(crime$murders)
sum(crime$arsons,na.rm = TRUE)
states=aggregate(x=crime$ViolentCrimesPerPop,by=list(crime$state),mean,na.rm=T)
states=arrange(states,desc(x))
#remove object 

rm(a)
#remove all objects
rm(list=ls())