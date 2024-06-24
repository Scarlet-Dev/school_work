library('readxl')
library('moments')

master = read_excel('./Week 4/Week4Practice.xlsx')
data = master

data$Drug1 = factor(data$Drug1)
data$Drug2 = factor(data$Drug2)

summary(data)

str(data)

plot(density(data$Rating))
agostino.test(data$Rating)

anscombe.test(data$Rating)
# Some kurtosis with p < 0.05

qqnorm(data$Rating)
shapiro.test(data$Rating)

data

model.lm = lm(Rating ~ Drug1 + Drug2, data)
md.resd = resid(model.lm)

plot(data$Rating, md.resd)
abline(0,0)

bartlett.test(data$Rating, data$Drug1)
tapply(data$Rating, data$Drug1, var)

bartlett.test(data$Rating, data$Drug2)
tapply(data$Rating, data$Drug2, var)

model.aov = aov(Rating ~ Drug1 * Drug2, data)
summary(model.aov)

interaction.plot(data$Drug1, data$Drug2, data$Rating)

TukeyHSD(model.aov)
