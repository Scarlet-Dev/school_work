library(readxl)
library(moments)
library(psych)

Q1 = read_xlsx('~/Downloads/Exam1Q1.xlsx')

str(Q1)
summary(Q1)
describe(Q1)

# Skewness
plot(density(Q1$Highschool))
agostino.test(Q1$Highschool)

plot(density(Q1$BS))
agostino.test(Q1$BS)
# Skewness Assumption made for both

# Normality
qqnorm(Q1$Highschool)
shapiro.test(Q1$Highschool)

qqnorm(Q1$BS)
shapiro.test(Q1$BS)
# Normality assumption made

# Equal Variance
Q1.lm = lm(Highschool ~ BS, Q1)
Q1.res = resid(Q1.lm)

plot(Q1$Highschool, Q1.res)
abline(0,0)

plot(Q1$BS, Q1.res)
abline(0,0)
# Equal Variance assumption made.

# t test Analysis
t.test(Q1$Highschool, Q1$BS, var.equal = TRUE, alternative = 'two.sided')
