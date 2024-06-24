library(moments)
library(readxl)
library(pastecs) 
library(compute.es) 

Q2 = read_xlsx('~/Downloads/Exam1Q2.xlsx')

str(Q2)
summary(Q2)
Q2$attendeddrivingschool = factor(Q2$attendeddrivingschool)
Q2$dayornight = factor(Q2$dayornight)
Q2$dayoftesting = factor(Q2$dayoftesting)

# Skewness
plot(density(Q2$errorsmade))
agostino.test(Q2$errorsmade)
# p value < 0.05, skewness assumption not made

# Kurtosis
anscombe.test(Q2$errorsmade)
# p value > 0.05. Kurtosis assumption made

# Normality
qqnorm(Q2$errorsmade)
shapiro.test(Q2$errorsmade)
# p value < 0.05. Normality assumption made

# Independence
Q2.lm = lm(errorsmade ~ factor(dayornight),Q2)
Q2.resd = resid(Q2.lm)
plot(Q2$errorsmade, Q2.resd)
abline(0,0)
# Independence assumption made

# Equal Variance
bartlett.test(Q2$errorsmade, factor(Q2$attendeddrivingschool))
tapply(Q2$errorsmade, factor(Q2$dayornight), var)
#  Variance value is less than 3. Variance assumption made

# Additivity
Q2.add = aov(errorsmade ~ attendeddrivingschool*dayornight, Q2)
summary(Q2.add)
# There is a significant interaction to attendeddriving school with
# the dayornight variable.

Q2.block = aov(errorsmade ~ attendeddrivingschool*dayoftesting, Q2)
summary(Q2.block)
# There is no significant interaction to attendeddrivingschool with
# the dayornight variable.

# ANOVA Analysis
Q2.aov = aov(errorsmade ~ attendeddrivingschool, Q2)
Q2.aov2 = aov(errorsmade ~ attendeddrivingschool + dayornight, Q2)
Q2.aov3 = aov(errorsmade ~ attendeddrivingschool + dayoftesting, Q2)

anova(Q2.aov, Q2.aov2, Q2.aov3)
# From the results adding dayornight to attendeddrivingschool has a significant
# difference to the model. adding dayoftesting does not show a significant 
# difference but does explain some of the variance.
summary(Q2.aov2)

interaction.plot(Q2$attendeddrivingschool, Q2$dayornight, Q2$errorsmade)

# Post Hoc Analysis
TukeyHSD(Q2.aov2)

# Effect Size
by(Q2$errorsmade, Q2$attendeddrivingschool, stat.desc)

mes(0.202, 0.283, 0.07, 0.12, 24, 24)
