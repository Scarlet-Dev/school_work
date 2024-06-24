library(sf)
library(mapview)
library(ggplot2)
library(plotly)
library(lubridate)

setwd('/Users/rosiewang/Documents/HU/500/Project')
Data <- read.csv('Startup Data.csv', header = TRUE)
summary(Data)
### Data Clean Up 
Data$first_funding_at <- as.Date(Data$first_funding_at, "%m/%d/%Y")
Data$last_funding_at <- as.Date(Data$last_funding_at, "%m/%d/%Y")
Data$founded_at <- as.Date(Data$founded_at, "%m/%d/%Y")
Data$closed_at <- as.Date(Data$closed_at, "%m/%d/%Y")

Data$is_top500 <- as.factor(Data$is_top500)
Data$status <- factor(Data$status, labels = c(1, 0), levels = c('acquired', 'closed'))

# table(ifelse(Data$labels==Data$status,"Yes","No"))
# ifelse(Data$state_code==Data$state_code.1,"Yes","No")
Data_master <- subset(Data, select = -c(Unnamed..0, id, Unnamed..6, labels, state_code.1, object_id))

# percentage of missing values:
total <- prod(dim(Data_master))
miss <- sum(is.na(Data_master))
(miss * 100 )/(total)

num_NA <- colSums(is.na(Data_master))
num_NA[c('closed_at', 'age_first_milestone_year', 'age_last_milestone_year')]/nrow(Data_master)


# EDA
# Location Distribute
locations_sf <- st_as_sf(Data_master, coords = c("longitude", "latitude"), crs = 4326)
mapview(locations_sf)

# Relationships
# Data cleaning
# As we see the graph, we can say high correlation between funding date and age funding. Difference between "last_funding_at" and "founded_at" is related "age_last_funding_year".
# "age_first_funding_year" and "age_last_funding_year" have negative values,it shouldn't be and also it can not be that "founded" date higher than "first_funding_at" and "last_funding_at"
# So we must get the absolute value of columns including negative value
ggplot(data = Data_master, aes((first_funding_at - founded_at)/365, age_first_funding_year)) +
    geom_point()

ggplot(data = Data_master, aes((last_funding_at - founded_at)/365, age_last_funding_year)) +
    geom_point()

ggplot(data = Data_master, aes(age_first_funding_year, age_first_milestone_year)) +
    geom_point()
ggplot(data = Data_master, aes(age_last_funding_year, age_last_milestone_year)) +
    geom_point()

Data_master$age_first_funding_year <- abs(Data_master$age_first_funding_year)
Data_master$age_last_funding_year <- abs(Data_master$age_last_funding_year)
Data_master$age_first_milestone_year <- abs(Data_master$age_first_milestone_year)
Data_master$age_last_milestone_year <- abs(Data_master$age_last_milestone_year)

# then the scatter plot makes more sense
ggplot(data = Data_master, aes(abs(first_funding_at - founded_at)/365, age_first_funding_year)) +
    geom_point()

ggplot(data = Data_master, aes(abs(last_funding_at - founded_at)/365, age_last_funding_year)) +
    geom_point()

ggplot(data = Data_master, aes(age_first_funding_year, age_first_milestone_year)) +
    geom_point()
ggplot(data = Data_master, aes(age_last_funding_year, age_last_milestone_year)) +
    geom_point()

# outliers:
par(mfrow=c(1,4))
boxplot(Data_master$age_first_funding_year)
boxplot(Data_master$age_last_funding_year)
boxplot(Data_master$age_first_milestone_year)
boxplot(Data_master$age_last_milestone_year)
par(mfrow=c(1,1))
hist(Data_master$age_first_funding_year)
lines(density(Data_master$age_first_funding_year))
densityplot(~ age_first_funding_year, data = Data_master)
plot(density(Data_master$age_first_funding_year, bw = 0.05))
rug(Data_master$age_first_funding_year)


# visualization

ggplot(data = Data_master, aes(state_code)) +
    geom_bar()

ggplot(data = Data_master, aes(state_code, funding_total_usd)) +
    geom_col()

ggplot(data = Data_master, aes(state_code, funding_rounds)) +
    geom_col()

ggplot(data = Data_master, aes(category_code)) +
    geom_bar()

ggplot(data = Data_master, aes(factor(category_code), fill = is_top500)) +
    geom_bar(position = 'dodge') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
ggplot(data = Data_master, aes(factor(category_code), fill = status)) +
    geom_bar(position = 'dodge') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggplot(data = Data_master, aes(status, age_first_funding_year)) +
    geom_boxplot()

ggplot(data = Data_master, aes(category_code, mean(funding_total_usd))) +
    geom_col()

ggplot(data = Data_master, aes(category_code, avg_participants)) +
    geom_col()

ggplot(data = Data_master, aes(funding_rounds, age_first_funding_year)) +
    geom_col()

ggplot(data = Data_master, aes(is_top500, fill = status)) +
    geom_bar()

ggplot(data = Data_master, aes(funding_rounds, fill = status)) +
    geom_bar(position = 'dodge')








