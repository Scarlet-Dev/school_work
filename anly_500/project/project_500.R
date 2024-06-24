# About the Data(From Kaggle Source - https://www.kaggle.com/manishkc06/startup-success-prediction)
# 
# The data contains industry trends, investment insights and individual company information. There are 48 columns/features. Some of the features are:
#   
# agefirstfunding_year – quantitative(Age of first time being funded by investors)
# agelastfunding_year – quantitative(Age of last time being funcded )
# relationships – quantitative (how many business relationships)
# funding_rounds – quantitative (how many rounds of funding company has gone through)
# fundingtotalusd – quantitative (total funding acieved in USD per thousands)
# milestones – quantitative (business milestones for a startup)
# agefirstmilestone_year – quantitative (age of first milestone achieved)
# agelastmilestone_year – quantitative (age of last milestone achieved)
# state – categorical
# industry_type(this is now labelled as category_type) – categorical
# has_VC – categorical
# has_angel – categorical
# has_roundA – categorical
# has_roundB – categorical
# has_roundC – categorical
# has_roundD – categorical
# avg_participants – quantitative
# is_top500 – categorical
# status(acquired/closed) – categorical (the target variable, if a startup is ‘acquired’ by some other organization, means the startup succeed) 


library(dplyr)
library(mice)
library(corrplot)
library(moments)
library(VIM, quietly = T)

# setwd('./project')
master <- read.csv("./startup_data.csv")

str(master)
names(master)
dim(master)

percentmiss <- function(x){
  sum(is.na(x))/length(x)*100
}

summary(master, is.na = TRUE)

## Data Cleaning

## Accuracy
# First we need to drop coulmns that we do not need in our analysis
drop_cols = c("Unnamed..0",'id','latitude','longitude','zip_code','Unnamed..6', 'state_code.1',
              'is_CA', 'is_NY', 'is_MA', 'is_TX', 'is_otherstate', 'object_id', 'name',
              "founded_at",'closed_at','first_funding_at','last_funding_at',
              'age_last_funding_year', 'age_first_milestone_year', 'age_last_milestone_year',
              'labels', 'is_software','is_web','is_mobile','is_enterprise',
              'is_advertising','is_gamesvideo', 'is_ecommerce','is_biotech','is_consulting',
              'is_othercategory','has_VC','has_angel','has_roundA','has_roundB','has_roundC',
              'has_roundD','is_top500')
acc <- master[,!(names(master) %in% drop_cols)]

# Factoring
acc$status <-  factor(acc$status, labels = c(1, 0), levels = c('acquired', 'closed'))
acc$state_code <- factor(acc$state_code)
acc$category_code <- factor(acc$category_code)

str(acc)

### Continuous
summary(acc)

acc$funding_total_usd = acc$funding_total_usd/100000
names(acc)[6] <- 'funding_total_usd_by_100,000'
# summary(acc)

## Missing Data
# At this point we cannot replace some of the missing dates so instead we can remove the some observations
# from the data set or attempt to replace some.
missing_ = acc


# Over 32000 observation do not have missing while 1009 do
# table(missing_)

# Make an aggregate table(excluding the closed_dates column) to see 
# what percentage of combinations are missing
aggr(missing_, numbers = T)

# Use the percentmiss function to get a missing data percentile for each observation.
missing_data = apply(missing_,1,percentmiss)

# Separate based on percent and keep those under 5% missing
table(factor(missing_data, levels = c(0, 20, 40, 60), labels = c("0%", "20%","40%","60%")))
under_rows = subset(missing_, missing_data < 0.05, use = 'pairwise.obs')
table(under_rows$status)

# Outliers
## In this section we must remove outliers using Cooks, leverage or Mahalanobis
outliers = under_rows
summary(outliers)


# dim(outliers)
table(outliers$status)

mahal_summ = mahalanobis(outliers[,c(3:7,9)], 
                         colMeans(outliers[,c(3:7,9)]), 
                         cov(outliers[,c(3:7,9)])
                         )

df.mahal = ncol(outliers[, c(3:7,9)])-1
cutoff = qchisq(1-0.001, df.mahal)

table(as.numeric(mahal_summ > cutoff))
total_outliers = sum(mahal_summ > cutoff, na.rm = TRUE)
  
noout <- subset(outliers, mahal_summ < cutoff)

# Assumptions
## setup
# TODO: create multivariable linear model. have to decide whether to make stat
# screened = lm(`funding_total_usd_by_100,000` ~ ., noout)
noout$status =  factor(noout$status, labels = c(1, 0), levels = c('acquired', 'closed'))
screened = glm(status ~ relationships + funding_rounds, noout, family = 'binomial')

fitted <- scale(screened$fitted.values)
standardized <- rstudent(screened)


# Additivity
summary(screened, correlation = T)


# Linearity


# Normality


# Homogen & sce

# Hierarchial

# Step 1

step1 = update(screened, . ~ . - relationships )
summary(step1)
anova(screened, step1)

step2 = update(step1, . ~ . + milestones + avg_participants)
summary(step2)
anova(step1, step2)

step3 = update(step2, . ~ . + (city + state_code))
summary(step3)
anova(step2, step3)

step4 = update(step3, . ~ . + (avg_participants + age_first_funding_year))
summary(step4)
anova(step3, step4)

step5 = update(step4, . ~ . + category_code)
summary(step5)
anova(step4, step5)

step6 = update(step5, . ~ . - category_code + `funding_total_usd_by_100,000`)
summary(step6)
anova(step5, step6)

step7 = update(step6, . ~ . + relationships)
# summary(step7)
anova(step6, step7, test = 'Chisq')

step8 = update(step7, . ~ . + category_code)
summary(step8)
anova(step1, step2, step3, step4, step5, step6, step7, step8, test = 'LRT')

# Models 2 and 7 had the best residuals so we will focus on those for our possible
# predictions

step2a <- update(step2, .~. +relationships )
step2b <- update(step2a, .~. -funding_rounds)
anova(step2,step2a,step2b, test='LRT')

anova(step2b, step7, test='LRT')

anova(step2, step7, test='LRT')

# From the ANOVA analysis Step 7 had the better significance


