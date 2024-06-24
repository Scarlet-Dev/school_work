#Clear workspace
rm(list=ls())

#Remove any plots
dev.off()


#Set working directory
setwd("C:\\Users\\Owner\\Documents\\ANLY\\EDA\\Lectures\\Lec#4")

#Slide 9
#Read in data
df=read.csv("companies.csv")

#Slide 10
str(df)
# the reformatting command depends on how the date variable looks when it's read into R
#if the variable is initially represented as 1/31/2016, use "%m/%d/%y"
#if the variable is initially represented as 2016-01-31, use "%Y-%m-%d"

#Reformat to date type
df$Date=as.Date(df$Date, format = "%m/%d/%Y")
#df$Date=as.Date(df$Date, format = "%Y-%m-%d")
df$Date <- as.Date(df$Date, format ="%m/%d/%Y")

str(df)

#Slide 12
#Create statistical summary of data
summary(df)

#Slide 13
if(!require(reshape2)){install.packages("reshape2")}
if(!require(ggplot2)){install.packages("ggplot2")}

library("reshape2")
library("ggplot2")
#Convert data to long format
df1 <- melt(df, id="Date")
#Create line plot comparing companies over time
ggplot(data=df1, aes(x=Date, y=value, color=variable)) + geom_line() + labs(y="Monthly Revenue ($)")+ theme(legend.position="bottom")

#Slide 14
#Create correlation matrix to look at similarity between companies
#Exclude Date column because not numeric
cor_matrix=cor(df[,-1])
#Convert data to long format
cor_matrix=melt(cor_matrix)
#Create correlation heat map plot to investigate correlation
qplot(data = cor_matrix, x=Var1, y=Var2, fill=value,geom="tile")+scale_fill_gradient2(limits=c(-1, 1))+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Slide 15
#Subset original data to just your company and Company4 (most similar)
df2=df[c("Company4","YourCompany")]
#Create scatterplot to compare companies
ggplot(data=df2,aes(x=Company4,y=YourCompany))+geom_point()
