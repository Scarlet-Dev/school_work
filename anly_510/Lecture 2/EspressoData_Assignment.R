library(moments)

master = read.csv('./EspressoData.csv')

data = master
summary(data)

data$brewmethod = factor(data$brewmethod)

# Skewness
plot(density(data$cereme))
agostino.test(data$cereme)
# The Agostino test is for the skewness test.
# The result showed the that the p-value not being less that the significant value
# of 0.05 (p< 0.05). We can assume that the sample does not have skewness.

# Normality
qqnorm(data$cereme)
shapiro.test(data$cereme)
# THe Shpairo Test shows that the Espresso data is normal and significant. 
# With a p < 0.05 we can reject the assumption that the Espresso data is normal.


# Independence
espresso.lm = lm(cereme ~ brewmethod, data = data)
espresso.res = resid(espresso.lm)

plot(data$cereme, espresso.res, xlab = "Creme Level", ylab = "Residual", main = "Residual against Creme Level")
abline(0,0)
# The values from the residual is


# Equal Variance
bartlett.test(data$cereme, data$brewmethod)
tapply(data$cereme, data$brewmethod, var)
# The Bartlett test checks for variance in the data. Since the p< 0.05 we can 
# assume that it passes the variance assumption


# ANOVA Analysis
espresso.aov = aov(cereme ~ brewmethod, data = data)
summary(espresso.aov)
# ``


# Post Hoc
library(pgirmess)

TukeyHSD(espresso.aov)

# Effect Size
library(pastecs)
library(compute.es)

by(data$cereme, data$brewmethod, stat.desc)

mes(32.4,61.3,7.3,10.1,9,9)

mes(61.3,39.7,10.1,7.7,9,9)

mes(32.4,39.7,7.3,7.7,9,9)
