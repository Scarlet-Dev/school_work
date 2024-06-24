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


library(ggpubr)
library(sf)
library(mapview)
library(dplyr)
library(mice)
library(corrplot)
library(moments)
library(VIM, quietly = T)
library(ggplot2)
library(lubridate)

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
drop_cols = c("Unnamed..0",'id','zip_code','Unnamed..6', 'state_code.1',
              'is_CA', 'is_NY', 'is_MA', 'is_TX', 'is_otherstate', 'object_id', 'name',
              'labels', 'is_software','is_web','is_mobile','is_enterprise',
              'is_advertising','is_gamesvideo', 'is_ecommerce','is_biotech','is_consulting',
              'is_othercategory','has_VC','has_angel','has_roundA','has_roundB','has_roundC',
              'has_roundD')
acc <- master[,!(names(master) %in% drop_cols)]


# After dropping columns we will need to change the data types for some of these columns

acc$first_funding_at <- as.Date(acc$first_funding_at, "%m/%d/%Y")
acc$last_funding_at <- as.Date(acc$last_funding_at, "%m/%d/%Y")
acc$founded_at <- as.Date(acc$founded_at, "%m/%d/%Y")
acc$closed_at <- as.Date(acc$closed_at, "%m/%d/%Y")

acc$is_top500 <- as.factor(acc$is_top500)
acc$status <- factor(acc$status, labels = c(1, 0), levels = c('acquired', 'closed'))
acc$city <- as.factor(acc$city)
acc$state_code <- as.factor(acc$state_code)
acc$category_code <- as.factor(acc$category_code)

acc$funding_total_usd_std <- acc$funding_total_usd/100000
str(acc)

### Continuous
summary(acc)

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
table(missing_data)

total <- prod(dim(missing_))
miss <- sum(is.na(missing_))
(miss * 100 )/(total)

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

#####----------- EDA -----------######
# Here it shows 3 col which have missing data and the percentages
summary(acc)
num_NA <- colSums(is.na(acc))
num_NA[c('closed_at', 'age_first_milestone_year', 'age_last_milestone_year')]/nrow(acc)

# As we see the graph, we can say high correlation between funding date and age funding. Difference between "last_funding_at" and "founded_at" is related "age_last_funding_year".
# "age_first_funding_year" and "age_last_funding_year" have negative values,it shouldn't be and also it can not be that "founded" date higher than "first_funding_at" and "last_funding_at"
# So we must get the absolute value of columns including negative value
g1 <- ggplot(data = noout, aes((first_funding_at - founded_at)/365, age_first_funding_year)) +
    geom_point() + theme_light()
g2 <- ggplot(data = noout, aes((last_funding_at - founded_at)/365, age_last_funding_year)) +
    geom_point() + theme_light()
g3 <- ggplot(data = noout, aes(age_first_funding_year, age_first_milestone_year)) +
    geom_point() + theme_light()
g4 <- ggplot(data = noout, aes(age_last_funding_year, age_last_milestone_year)) +
    geom_point() + theme_light()

ggarrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

noout$age_first_funding_year <- abs(noout$age_first_funding_year)
noout$age_last_funding_year <- abs(noout$age_last_funding_year)
noout$age_first_milestone_year <- abs(noout$age_first_milestone_year)
noout$age_last_milestone_year <- abs(noout$age_last_milestone_year)

# then the scatter plot makes more sense
g5 <- ggplot(data = noout, aes(abs(first_funding_at - founded_at)/365, age_first_funding_year)) +
    geom_point() + theme_light()
g6 <- ggplot(data = noout, aes(abs(last_funding_at - founded_at)/365, age_last_funding_year)) +
    geom_point() + theme_light()
g7 <- ggplot(data = noout, aes(age_first_funding_year, age_first_milestone_year)) +
    geom_point() + theme_light()
g8 <- ggplot(data = noout, aes(age_last_funding_year, age_last_milestone_year)) +
    geom_point() + theme_light()

ggarrange(g5, g6, g7, g8, ncol = 2, nrow = 2)

# we can remove the variables founded at, closed at, first funding at and last funding at because
# those relationships are reflected by the age_first_funding_year, age_last_funding_year
drop_cols2 = c('founded_at', 'closed_at', 'first_funding_at', 'last_funding_at')
noout <- noout[,!(names(noout) %in% drop_cols2)]

# Visualizations
# Location Distribute
locations_sf <- st_as_sf(noout, coords = c("longitude", "latitude"), crs = 4326)
mapview(locations_sf)

# Different states
g_state1 <- ggplot(data = noout, aes(x = reorder(state_code, state_code, function(x) - length(x)))) + geom_bar() +
    labs(y = 'Count of the StartUps', x = 'States', title = 'Count of StartUps in States') +
    theme_light()
    
g_state2 <- ggplot(data = noout, aes(x = reorder(state_code, -funding_total_usd_std), mean(funding_total_usd_std))) + geom_col() +
    labs(y = 'Avg Total Funding USD in 100,000', x = 'States', title = 'Average Total Fundings in States') +
    theme_light()

g_state3 <- ggplot(data = noout, aes(state_code, funding_rounds)) + geom_boxplot() +
    labs(y = 'Funding Rounds', x = 'States', title = 'Funding Rounds in States') +
    theme_light()

g_state1
g_state2
g_state3
#ggarrange(g_state1, g_state2, g_state3, ncol = 2, nrow = 2)

# category visualization
g_cate1 <- ggplot(data = noout, aes(x = reorder(category_code, category_code, function(x) - length(x)))) + geom_bar() +
    labs(y = 'Count of the StartUps', x = 'Categories', title = 'Count of StartUps in different Categories') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
g_cate1

g_cate2 <- ggplot(data = noout, aes(x = reorder(category_code, -funding_total_usd_std), mean(funding_total_usd_std))) + geom_col() +
    labs(y = 'Avg Total Funding USD in 100,000', x = 'Categories', title = 'Average Total Fundings in Categories') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
g_cate2

g_cate3 <- ggplot(data = noout, aes(x = reorder(category_code, avg_participants), mean(avg_participants))) + geom_col() +
    labs(y = 'Avg Participants', x = 'Categories', title = 'Average Participants in Categories') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
g_cate3

g_cate4 <- ggplot(data = noout, aes(x = reorder(category_code, category_code, function(x) - length(x)), fill = is_top500)) +
    geom_bar(position = 'dodge') +
    labs(y = 'Count of the StartUps', x = 'Categories', title = 'Top 500 Count of StartUps in different Categories') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
g_cate4

g_cate5 <- ggplot(data = noout, aes(x = reorder(category_code, category_code, function(x) - length(x)), fill = status)) +
    geom_bar(position = 'dodge') +
    scale_fill_discrete(name = "Status", labels = c("Acquired", "Closed")) +
    labs(y = 'Count of the StartUps', x = 'Categories', title = 'Success Status Count of StartUps in different Categories') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
g_cate5

#ggplot(data = acc, aes(funding_rounds, age_first_funding_year)) + geom_col()
# now we find if there are other variables impact firm status
ggplot(data = noout, aes(status, age_first_funding_year)) +
    scale_x_discrete(labels = c("Acquired", "Closed")) +
    labs(y = 'First Funding Year Age', x = 'Company Status', title = 'First Funding Year Age in different Statuses') +
    geom_boxplot() + theme_light()

ggplot(data = noout, aes(is_top500, fill = status)) +
    geom_bar() +
    scale_fill_discrete(name = "Status", labels = c("Acquired", "Closed")) +
    labs(y = 'Count of Companies', x = 'Is Top 500', title = 'Top 500 Count of Aquired or Closed Statuses') +
    theme_light()

ggplot(data = noout, aes(funding_rounds, fill = status)) +
    geom_bar(position = 'dodge') +
    scale_fill_discrete(name = "Status", labels = c("Acquired", "Closed")) +
    labs(y = 'Count of Companies', x = 'Funding Rounds', title = 'Aquired or Closed Companies\' Count in Funding Rounds') +
    theme_light()


# Assumptions
## setup
noout$status =  factor(noout$status, labels = c(1, 0), levels = c('acquired', 'closed'))
screened = glm(status ~ relationships + funding_rounds, noout, family = 'binomial')

fitted <- scale(screened$fitted.values)
standardized <- rstudent(screened)


# Additivity
summary(screened, correlation = T)

# Linearity
{qqnorm(standardized)
  abline(0,1)}

# Normality
hist(standardized)

# Homogen & sce
{plot(standardized, fitted)
  abline(0,0)
  abline(v=0)}


# ANOVA Analysis
# Hierarchial ANOVA Analysis

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

# Prediction Model

