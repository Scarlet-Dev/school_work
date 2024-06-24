##########-------------------Chapter 6-----------------------###########

#Clear workspace
rm(list=ls())
#Clear plots
dev.off()

#Install packages. This will only install if you don't already have the package downloaded.
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tsModel)){install.packages("tsModel")}
if(!require(ggplot2)){install.packages("ggplot2")}

#Load packages
library(dplyr)
library(tsModel)

#Set working directory
setwd("C:\\Users\\Owner\\Documents\\Tommy\\Lectures\\Lecture 5")

#Read in data
ny1=read.csv("ny.csv")

####----Data manipulation----###
#Take a glimpse at data
glimpse(ny1)
#We can see from our glimpse output that the variable called "date" is actually currently a Factor variable type.
#We convert this to a Date variable type.
ny1$date=as.Date(ny1$date)
#Take another glimpse to make sure that it converted successfully.
glimpse(ny1)

library(ggplot2)
#Slide 11-Show Multivariate Data
qplot(pm10, death, data = ny1, geom = c("point", "smooth"), method = "lm", xlab = expression(PM[10] * " concentration (centered)"), ylab = "Daily mortality (all cuases)")

#For the next plot, we only want the dates up to 1991.
#Subset the data using "filter()" from dplyr.
ny2=filter(ny1, date < as.Date("1991-01-01"))

#Slide 12-Show Multivariate Data (continued)
qplot(date, death, data = ny2, ylab = "Daily mortality", geom = c("point", "smooth"), method = "loess", span = 1/10)

#Slide 13-Show Mulivariate Data (continued)
qplot(date, pm10tmean, data = ny1, geom = c("point", "smooth"), method = "loess", span = 1/10, ylab = expression(PM[10]))

#Slide 14-Show Multivariate Data (continued)
qplot(pm10, death, data = ny1, facets = . ~ season, geom = c("point", "smooth"), method = "lm", xlab = expression(PM[10] * " concentration (centered)"), ylab = "Daily mortality (all cuases)")

##########-------------------Chapter 7-----------------------###########

#Slide 27
#Set working directory
setwd("C:\\Users\\Owner\\Documents\\Tommy\\Lectures\\Lecture 5")

#Read in our second dataset
pollution=read.csv("avgpm25.csv")

#Take a quick look at our data
#First few observations
head(pollution)
#Properties of the variables
glimpse(pollution)

#Slide 29
#Statistical summary
summary(pollution)
summary(pollution$pm25)

#Slide 30-Boxplot
boxplot(pollution$pm25, col = "blue")

#Slide 31-Histogram
hist(pollution$pm25, col = "green")

#Slide 32-Histogram (continued)
hist(pollution$pm25, col = "green")
rug(pollution$pm25)

#Slide 33-Histogram (continued)
hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)

#Slide 34-Overlaying Features
boxplot(pollution$pm25, col = "blue")
abline(h = 12)

#Slide 35-Overlaying Features (continued)
hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

#Slide 36-Barplot
table(pollution$region) %>% barplot(col = "wheat")

#Slide 38-Multiple Boxplots
boxplot(pm25 ~ region, data = pollution, col = "red")

#Slide 39-Multiple Histograms
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

#Slide 40-Scatterplots
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

#Slide 41. Scatterplot-Using Color
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

#Slide 42-Multiple Scatterplots
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))
