library(readxl)
library(car)
library(mlogit)

setwd('~/source/class_assignments/anly_510/Week 9')
data <- readxl::read_xlsx('LogisticData.xlsx')

summary(data)

with(data, table(Age, TimeOfDay, Ad))

scatterplot(data$Age, data$Age)
scatterplot(data$TimeOfDay, data$Age)
scatterplot(data$ClickThrough, data$Age)

data$Ad <- factor(data$Ad)
data$TimeOfDay <- factor(data$TimeOfDay)
data$ClickThrough <- factor(data$ClickThrough)

model <- glm(ClickThrough)