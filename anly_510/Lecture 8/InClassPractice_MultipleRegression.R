library(readxl)
library(car)
library(Hmisc)


setwd('~/hu_lectures/anly_510/Lecture 8')

data = readxl::read_xlsx('./RegressionExample.xlsx')

summary(data)

data$Ad <- factor(data$Ad, labels = c(1,2,3))
data$Sex <- factor(data$Sex, labels = c(1,2))

summary(data)
class(data)
str(data)

dataMatrix <- as.matrix(data)
rcorr(dataMatrix)

plot(density(data))