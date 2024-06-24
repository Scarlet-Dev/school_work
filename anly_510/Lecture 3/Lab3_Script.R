library(moments)
library(readxl)
library(pgirmess)
library(pastecs)
library(compute.es)

master = readxl::read_xlsx('./Week 3/Lab3.xlsx')
data = master

summary(data)

# Skewness
plot(density(data$Performance_score))
agostino.test(data$Performance_score)
# Skewness assumption is not broken

# Kurtosis
anscombe.test(data$Performance_score)
# Anscombe test shows high kurtosis of data. 
# Kurtosis assumption is broken with p < 0.05

# Normality
qqnorm(data$Performance_score)
shapiro.test(data$Performance_score)
# Shapiro test and Q-Q plot shows points points are mostly normal. p-value < 0,05
# Normality assumption is not broken.

# Independence
model.lm = lm(Performance_score ~ Age , data = data)
model.res = resid(model.lm)
plot(data$Performance_score, model.res,)
abline(0,0)
# Plot shows values being evenly distributed above and below abline. 
# Independence assumption is not broken

# Equal Variance
bartlett.test(data$Performance_score, data$Age)
tapply(data$Performance_score, data$Age, var)
# Results show there is no major variance across the levels and p > 0,05. Equal Variance
# assumption is not broken

# Additivity of Interaction
model = aov(Performance_score ~ factor(Age) * factor(Condition), data = data)
summary(model.aov)
# The Interaction model 1 shows that there is a  significant interaction between
# Condition and Age. Residuals show that Condition and its connection does explain
# some of the error in the model.

# ANOVA Analysis
model.aov2 = aov(Performance_score ~ factor(Age) + factor(Condition), data = data)
summary(model.aov2)

model.aov3 = aov(Performance_score ~ factor(Age), data = data)
summary(model.aov3)

anova(model.aov2, model.aov3)
# COmparing the model with the blocker versus without the blocker shows that
# \


# Post Hoc Analysis
pairwise.t.test(data$Performance_score, data$Age, p.adjust.method = "bon", 
                paired = FALSE)
# 

# Effect Size
by(data$Performance_score, data$Age, stat.desc)

# Groups 2 and 1
mes(27.94, 32.45, 4.35, 3.59, 31, 29)

# Group 3 and 1
mes(22.14, 32.45, 3.98, 3.59, 29, 29)

# Group 3 and 2
mes(22.14, 27.94, 3.98, 3.59, 29, 31)