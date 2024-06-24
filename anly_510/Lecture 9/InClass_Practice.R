library(readxl)
library(car)
library(mlogit)
setwd('~/source/class_assignments/anly_510/Week 9')
data <- readxl::read_xlsx('LRclassP.xlsx')

summary(data)

with(data, table(Food, Gender, Weight, HC))
scatterplot(data$Age, data$Weight)
scatterplot(data$Food, data$Weight)


data$Weight <- factor(data$Weight)
data$Gender <- factor(data$Gender)
data$Food <- factor(data$Food)
data$HC <- factor(data$HC)

summary(data)

model <- glm(Weight ~ Age, data = data, 'binomial')
newData <- mlogit.data(data, choice="ES", shape="wide")
