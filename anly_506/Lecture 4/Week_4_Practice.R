library(dplyr)
library(ggplot2)
library(psych)

master = read.csv('./Week 4/companies.csv')

data = master
str(data)

data$Date

data$Date = as.Date(data$Date, "%m/%d/%Y")

summary(data[,-1])
# Company5 has the largest numbers. 'YourCompany' and Company 1,2 and 4 
# have comparable numbers. Company3 has the lowest numbers

# Plotting a Time Series

timeRange = melt()