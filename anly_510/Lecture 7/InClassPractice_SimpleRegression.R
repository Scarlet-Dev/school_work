library(readxl)
library(Hmisc)


setwd('~/hu_lectures/anly_510/Lecture 7')

data = readxl::read_xlsx("simplerelationships.xlsx")

summary(data)

cor(data)

model1 <- lm(Income ~ PI, data = data)
summary(model1)

model2 <- lm(Income ~ Age, data = data)
summary(model2)

model3 <- lm(Income ~ Age + PI, data = data)
summary(model3)

model4 <- lm(Income ~ Age * PI, data = data)


anova(model1, model2)
AIC(model1)
BIC(model1)

AIC(model2)
BIC(model2)

anova(model1, model3)
anova(model1, model4)
