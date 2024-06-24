library(readxl)
library(moments)
# library(pastecs)
# library(compute.es)
master = readxl::read_xlsx('./BankInfo.xlsx',1)

data = master

summary(data)
data$Group = factor(data$Group)

plot(density(data$Income))
qqnorm(data$Income)

agostino.test(data$Income)
shapiro.test(data$Income)

bank.lm = lm(data$Income ~ data$Creditcard, data = data)
bank.res = resid(bank.lm)
plot(data$Income, bank.res, ylab = "Residual", xlab="Income", main = "Bank Info")
abline(0,0)

bartlett.test(data$Income, data$Group)
tapply(data$Income, data$Group, var)

bank.aov = aov(data$Income ~ data$Group, data = data)
summary(bank.aov)

