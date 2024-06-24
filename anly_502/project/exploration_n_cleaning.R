# 08/21/2021 - Akane Simpson
  # Initial project start.
  # Import data from Github and began breaking into sub groups of years (2020, 2021)
  # Get population and total deaths for 2020 and 2021
  # 


# 08/23/2021 - Akane Simpson
  # Added basic histogram for Deaths per Reporting Months in 2020 and 2021
  # Added function to group and squash death reports for 

download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv","c19_time_series_deaths.csv")
# library("ggplot2")
# library("dplyr")

library("tidyverse")# Should include ggplot2, dplyr and other useful packages
library("reshape")
library("psych")
library("moments")

# Import Data
c19_ts.deaths <- read.csv("./c19_time_series_deaths.csv")

str(c19_ts.deaths)
dim(c19_ts.deaths)
names(c19_ts.deaths)
head(c19_ts.deaths)
tail(c19_ts.deaths)

sum(is.na(c19_ts.deaths))

# Data Cleaning
    dv <- c(names(c19_ts.deaths[,13:584]))
    us_states_territories <- unique(c19_ts.deaths$Province_State)
    
    c19_deaths <- melt(c19_ts.deaths, id = c("UID","Combined_Key","Province_State","Population"), measure.vars = dv)
    colnames(c19_deaths)[3] <- "State"
    colnames(c19_deaths)[5:6] <- c("Sample Date","COVID-19 Deaths")
  
    temp_dates = c19_deaths[,5]+
      gsub("X",'',temp_dates)+
      gsub("\\.","/",temp_dates)+
      as.Date(temp_dates, format = "%m/%d/%y")
    
    reporting_years <- as.numeric(format(temp_dates, format = "%Y"))+
      as.numeric(format(temp_dates, format = "%m"))
  
  # Export New Data Frame
      # write.csv(c19_deaths, "c19_deaths_simplified.csv")
  
  # Created additional columns for aggregation 
      c19_deaths[,5] <- temp_dates
      c19_deaths$Reporting_Months <- reporting_months
      c19_deaths$Reporting_Years <- reporting_years
  
  # Subgroups
      deaths_in_2020 <- subset(c19_deaths, c19_deaths$Reporting_Years == 2020)
      deaths_in_2021 <- subset(c19_deaths, c19_deaths$Reporting_Years == 2021)
  
# Data Summaries
      summary(c19_deaths)
  
      population <- sum(c19_deaths$Population)# Total population from 2020 till now
      total_deaths <- sum(c19_deaths$`COVID-19 Deaths`)# Total deaths from 2020 till now
  
      describe(c19_deaths$`COVID-19 Deaths`)
      describe(c19_deaths$Population)
    # There is a difference between the population variable and the value provided by the describe. Not sure why
  
  total_per_death <- total_deaths/population * 100 #This is the percentage of deaths within the data sets
  total_prop_death <- 1 - total_deaths/population #This is the proportion of deaths within the data sets
  
  # Need to make some more subgroups in relation to monthly changes within the states
  # Since the data set is more assessment based there is a need to find how each state faired throghout the year
  # To do this we need to subset the data set based on state name, then squash the counties deaths for the state
  # per month and group each states total deaths for each month
    # monthly_death_totals <- function(x){
    #   
    #     for (i in us_states_territories)
    #     {
    #       #First select the state
    #      g <- subset(x, State == i)
    #      
    #      #Next sum up the total deaths per monthfor the state
    #      
    #     }
    # }
    
    g <- subset(deaths_in_2020, State == "New York")
    g %>% 
      group_by(month = ceiling_date(g$`Sample Date`, "month")) %>%
      summarise(cummulative = cumsum(g$`COVID-19 Deaths`))
  
  # Frequency Tables - These are the observation for each categorical data, not total deaths.
    table(factor(c19_deaths$State))
    table(factor(c19_deaths$Reporting_Years))


# Data Visualization
 
  # Total Deaths per Month in 2020
    ## The Reporting Months limit is based on the time when the virus was first reported in the US. 
      hist_deaths_2020 <- ggplot(deaths_in_2020, aes(x = Reporting_Months, fill = `COVID-19 Deaths`))
      hist_deaths_2020 + 
        geom_histogram(bins = 50, binwidth = 0.5)+
        theme_classic()
  
  # Total Deaths per Month in 2021 (So Far)
      hist_deaths_2021 <- ggplot(deaths_in_2021, aes(x = Reporting_Months, fill = `COVID-19 Deaths`))
      hist_deaths_2021 + 
        geom_histogram(bins = 50, binwidth = 0.5)+
        theme_classic()
  
  # Population Change from 2020 to 2021
      pop_change_2020_2021 <- ggplot(c19_deaths, aes(x = `Reporting_Years`, y = `Population`))
      
      pop_change_2020_2021 +
        stat_summary(fun = sum, geom = "bar")+
        xlim(0,8)+
        theme_classic()
      # Population for 2021 is not complete
      
  # We should also see what are the population changes due to the deaths
  # Population Change per Month in 2020
      
      
  # Population Change per Month in 2021
      
      