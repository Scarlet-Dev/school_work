library(moments)
library(readxl)

master = readxl::read_xlsx('./Week 3/BlockingExample.xlsx')

data = master

data$color = factor(data$color)
data$focusgroup = factor(data$focusgroup)
data$rating = factor(data$rating)
data$price = factor(data$price)

plot(density(data$rating))
agostino.test(data$rating)

qqnorm(data$rating)
shapiro.test(data$rating)

earphones.lm = lm(rating ~ price, data = data)
earphones.res = resid(earphones.lm)

plot(data$rating, earphones.res)
abline(0,0)

bartlett.test(data$rating, data$price)
tapply(data$rating, data$price, var)

model1 = aov(rating ~ factor(price) * factor(focusgroup), data = data)
summary(model1)

model2 = aov(rating ~ factor(price) + factor(focusgroup), data = data)
summary(model2)

pairwise.t.test(data$rating,data$price, p.adjust.method = 'bon', paired = FALSE)

library(computes.cs)
by(data$Performance_score, data$Age, stat.desc)

mes()
