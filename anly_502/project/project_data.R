# import libraries
library("reshape")
library("ggplot2")
library("psych")
library("moments")
library("dplyr")

# download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv","c19_time_series_deaths.csv")

# download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv","c19_time_series_confirmed.csv")

# download.file("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv", "us_state_by_region.csv")

c19_ts.deaths <- read.csv("./c19_time_series_deaths.csv")
c19_ts.confirmed <- read.csv("./c19_time_series_confirmed.csv")
us_map <- read.csv("./us_state_by_region.csv")

# Functions
dates_to_text <- function(x){
  temp_dates = x
  temp_dates = gsub("X",'',temp_dates)
  temp_dates = gsub("\\.","/",temp_dates)
  temp_dates <- as.Date(temp_dates, format = "%m/%d/%y")
  
  return(temp_dates)
}

agg_by_region = function (df, input_region, year){
  region = subset(df, Region == input_region)
  
  regionYear = subset(region, Reporting_Years == year) # all regions in 2020
  
  ans = aggregate(regionYear$'COVID-19 Deaths', by=list(Category=regionYear$'Sample Date'), FUN=sum)
  names(ans) = c('Date', 'death_count')
  return (ans)
}

# Data Sets Structures

## US C19 Deaths
str(c19_ts.deaths)
dim(c19_ts.deaths)
names(c19_ts.deaths)

## USC19 Confirmed Cases
str(c19_ts.confirmed)
dim(c19_ts.confirmed)
names(c19_ts.confirmed)

## Get US state file
str(us_map)
dim(us_map)
names(us_map)

# Data Wrangling

## C19 Deaths
dv <- c(names(c19_ts.deaths[,13:584])) # NOTE: this is hardcoded.
c19_deaths <- melt(c19_ts.deaths, id = c("UID","Admin2","Province_State","Population"), measure.vars = dv)
colnames(c19_deaths)[c(2:3,5:6)] <- c("County","State","Sample Date","COVID-19 Deaths")

## Created additional columns for aggregation purposes
c19_deaths[,5] <- dates_to_text(c19_deaths[,5])
c19_deaths$Reporting_Months <- as.numeric(format(c19_deaths[,5], format = "%m"))
c19_deaths$Reporting_Years <- as.numeric(format(c19_deaths[,5], format = "%Y"))
c19_deaths <- c19_deaths %>% left_join(us_map, by="State")


## C19 Confirmed Cases
dc <- c(names(c19_ts.confirmed[,12:583]))
c19_cases <- melt(c19_ts.confirmed, id = c("UID","Admin2", "Province_State"), measure.vars = dc)
colnames(c19_cases)[c(2:5)] <- c("County", "State", "Sample Date", "COVID-19 Cases")

## Create Additional columns for aggregate
c19_cases[,4] <- dates_to_text(c19_cases[,4])
c19_cases$Reporting_Months <- as.numeric(format(c19_cases[,4], format = "%m"))
c19_cases$Reporting_Years <- as.numeric(format(c19_cases[,4], format = "%Y"))
c19_cases <- c19_cases %>% left_join(us_map, by="State")


deaths_per_state <- aggregate(c19_deaths$`COVID-19 Deaths`, by = list(State = c19_deaths$State, Year = c19_deaths$Reporting_Years, Population), FUN = function(x) { round((sum(x)/12)) })
names(deaths_per_state)[3] <- "Total Deaths per Year"

cases_per_state <- aggregate(c19_cases$`COVID-19 Cases`, by = list(State = c19_cases$State, Year = c19_cases$Reporting_Years), FUN = function(x) { round((sum(x)/12))})
names(cases_per_state)[3] <- "Total Cases per Year"

merged_c19_cases_deaths <- merge(cases_per_state, deaths_per_state, by = c("State","Year"))

covid_in_2020 <- subset(merged_c19_cases_deaths, merged_c19_cases_deaths$Year == 2020)
covid_in_2021 <- subset(merged_c19_cases_deaths, merged_c19_cases_deaths$Year == 2021)
