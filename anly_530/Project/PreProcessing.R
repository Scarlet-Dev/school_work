library(caret)
library(tidyverse)
library(readODS)
library(stringr)
library(magrittr)
library(mice)
library(outlieR)

replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", 
                     gsub("\\ ", "", x)))
}

zip_extract <- function(d){
  zip_only <- d %>% 
    filter(grepl("ZCTA5", Label)) %>% 
    select(Label) %>% 
    trimws(which = "both") %>% 
    str_remove_all(pattern = "ZCTA5 ") %>% 
    str_split(", ") %>% 
    unlist() %>% 
    str_remove_all("[c, \"\", (, )]")
}

extract_median_values <- function(m){
  median_values <- subset(x = m, Label == "Median income (dollars)")
}

station_percents <- function(df, n){
  n/df$Totals * 100
}


# setwd('./Project/')

# Alternative Fuel Stations
raw_historic_afs <- readxl::read_xlsx('./raw/historical_afs_count_2020.xlsx', 1, col_names = FALSE)
raw_afs <- readr::read_csv('./raw/alt_fuel_stations (Oct 7 2022).csv')
raw_vehicle_reg_pa <- readODS::read_ods('./raw/pa_vehicle_reg_type_by_zip_11_2020.ods')
raw_pa_county_zip <- read_ods('./raw/pa_county_zip_cross_refrence.ods')
raw_pa_ev_reg_zip <- read_ods('./raw/pa_vehicle_reg_type_by_zip_11_2020.ods')

# Median Salary of PA
# raw_median_pa_earners <- readxl::read_xlsx('./raw/ACSST5Y2020.S1903-2022-10-08T040519.xlsx', 5)
# raw_median_pa_famsize <- readxl::read_xlsx('./raw/ACSST5Y2020.S1903-2022-10-08T040519.xlsx', 4)
# raw_median_pa_agegrp <- readxl::read_xlsx('./raw/ACSST5Y2020.S1903-2022-10-08T040519.xlsx', 3)


# Get historic alt fuel stations per year
colnames(raw_historic_afs) <- raw_historic_afs[2,]
raw_historic_afs <- raw_historic_afs[-c(1, 2), ]

northeast.historic.ev <- raw_historic_afs %>% filter(State == "New York" | State == "Pennsylvania" |
                                             State == "New Jersey" | State == "Connecticut" |
                                               State == "Maryland" | State == "Delaware" | State == "Maine" |
                                               State == "Connecticut" | State == "New Hampshire" |
                                               State == "Vermont" | State =="Rhode Island" |
                                               State == "Massachusetts" | State == "District of Columbia") %>% 
  select(State, `Electrica (stations / charging outlets Level 1 / Level 2 / DC Fast)`) %>% 
  rename(Electric.Stations = `Electrica (stations / charging outlets Level 1 / Level 2 / DC Fast)`) %>% 
  mutate(Electric.Stations = str_replace_all(Electric.Stations, pattern = ' ', replacement = '/')) %>% 
  mutate(Electric.Stations = str_replace_all(Electric.Stations, pattern = '///', replacement = '/')) %>% 
  separate(Electric.Stations, into = paste0([c('Electric.Stations','Charging.Outlets', 'Charging.Lv1', 'Charging.Lv2', 'Dc.Fast')] , sep = '[/]') %>% 
  mutate(across(c(2:6), replaceCommas))

northeast.historic.ev <- northeast.historic.ev %>% 
  mutate(Totals = rowSums(across(where(is.numeric))))
  
northeast.historic.ev <- northeast.historic.ev %>% 
  mutate(starts_with("Electric"), ~ . /Totals * 100, .names = paste0("Percent.Station.Only.", 1:5))


# Northeast states
ne <- c("PA", "NY", "NJ", "CT", "DE", "VT", "RI", "NH", "MA", "MD", "DC", "ME")
ne.afs <- raw_afs %>%  filter(State == ne)

## We identify columns that should be converted into a factor
factorCols <- c('State', 'Status Code', 'Access Code', 'Facility Type')

table(factor(ne.afs$`Fuel Type Code`))

ne.afs[factorCols] <- lapply(ne.afs[factorCols], factor)
ne.afs <- ne.afs %>% 
  mutate(`Fuel Type Code` = factor(`Fuel Type Code`,
                                   labels = c("Biodisel","Compressed Natural Gas",
                                              "Ethanol E85", "Electric", 
                                              "Liquid Natural Gass", 
                                              "Liquid Petroleum Gas"))) %>% 
  select(`Fuel Type Code`, City, State, ZIP, `Status Code`,
         `Access Code`, `Facility Type`) %>% 
  filter(!is.na(`Facility Type`)) %>% 
  arrange(State)


# PA only
## Study is focused on North East States
penn_afs <- ne.afs %>% filter(State == 'PA')
# write.csv(penn_afs, './processed/v2/pa_AFV_cleaned_v2.csv')

pa_counties_zip <-raw_pa_county_zip %>% 
  select('county', 'zip') %>% 
  mutate(county = toupper(county)) %>% 
  mutate(county = sub('COUNTY', '', county)) %>% 
  mutate(county = factor(county)) %>% 
  mutate(zip = factor(zip)) %>% 
  rename(County = 'county', Zip.Code = 'zip')
# write.csv(pa_counties_zip, './processed/v2/pa_counties_zip_clean_v2.csv')

pa_ev_reg <- raw_pa_ev_reg_zip %>% 
  rename(Zip.Code = 'zip,C,80') %>% 
  rename_with(.fn = ~ str_replace(.x, ',N,24,15', ''), 
              .cols = ends_with(',N,24,15')) %>% 
  rename_with(.fn = ~ str_replace(.x, ',C,1', ''), 
              .cols = ends_with(',C,1')) %>% 
  select(Zip.Code, elec_cnt) %>% 
  mutate(Zip.Code = as.character(Zip.Code)) %>% 
  rename(Ev.Registration.Count = elec_cnt) %>% 
  mutate(Ev.Registration.Count = as.numeric(Ev.Registration.Count))
# write.csv(pa_ev_reg, './processed/v2/pa_registered_ev_v2.csv')


# Only need to do this once since the zips will be the same
pa.median.zip <- zip_extract(raw_median_pa_earners)
pa.median.salary.earners <- extract_median_values(raw_median_pa_earners)
pa.median.salary.famsize <- extract_median_values(raw_median_pa_famsize)
pa.median.salary.agegrp <- extract_median_values(raw_median_pa_agegrp)

# Remove raw data
# remove(raw_median_pa_agegrp)
# remove(raw_median_pa_famsize)
# remove(raw_median_pa_earners)
# remove(raw_afs)
# remove(raw_historic_afs)
# remove(raw_vehicle_reg_pa)
# remove(raw_pa_county_zip)
# remove(raw_pa_ev_reg_zip)

earners_median_salary <- apply(pa.median.salary.earners[,-1], MARGIN = 2, replaceCommas) %>% 
  apply(MARGIN = 2, FUN = as.numeric) %>% 
  bind_cols(Zip.Code = pa.median.zip) %>% 
  relocate(Zip.Code) %>% 
    rename(No.Earner = `No earners`, 
           Single.Earner = `1 earner`,
           Double.Earners = `2 earners`,
           Multiple.Earners = `3 or more earners`)

famsize_median_salary <- apply(pa.median.salary.famsize[,-1], MARGIN = 2, replaceCommas) %>% 
  apply(MARGIN = 2, FUN = as.numeric) %>% 
  bind_cols(Zip.Code = pa.median.zip) %>% 
  relocate(Zip.Code) %>% 
  rename(Two.Person = `2-person families`,
         Three.Person = `3-person families`,
         Four.Person = `4-person families`,
         Five.Person = `5-person families`,
         Six.Person = `6-person families`,
         Seven.Plus.Families = `7-or-more person families`)

agegrp_median_salary <- apply(pa.median.salary.agegrp[,-1], MARGIN = 2, replaceCommas) %>% 
  apply(MARGIN = 2, as.numeric) %>% 
  bind_cols(Zip.Code = pa.median.zip) %>% 
  relocate(Zip.Code) %>% 
  rename(Age.15.24 = `15 to 24 years`,
         Age.25.44 = `25 to 44 years`,
         Age.45.64 = `45 to 64 years`,
         Age.65.Over = `65 years and over`)

full.median.sal.vars <- earners_median_salary %>% 
  left_join(famsize_median_salary, by = "Zip.Code") %>% 
  left_join(agegrp_median_salary, by = "Zip.Code") %>% 
  filter(complete.cases(.))

pa.base <- penn_afs %>% 
  rename(Zip.Code = ZIP) %>% 
  left_join(x = ., y = pa_counties_zip, by = 'Zip.Code') %>% 
  left_join(x = ., y = full.median.sal.vars, by = 'Zip.Code') %>% 
  filter(`Fuel Type Code` == 'Electric' & `Access Code` == 'public')%>% 
  select(c(2:4, 8:22))
# write.csv(pa.base, "./processed/v2/pa_median_salary_county_zip_v2.csv")


pa.station.salary.earners <- pa.base %>% 
  group_by(County) %>% 
  left_join(x = ., y = pa_ev_reg, by = 'Zip.Code') %>% 
  select(-c(1,2)) %>% 
  mutate(Total.Station.Per.County = n()) %>% 
  mutate(Availability = (Total.Station.Per.County/Ev.Registration.Count)) %>% 
  drop_na()%>% 
  mutate(across(3:14, mean, .names = "Avg.Median.{.col}")) %>% 
  mutate(HasAvailability = as.factor(ifelse(Availability >= 0.20, 'Y', 'N')))
# write.csv(pa.station.salary.earners, "./processed/v2/pa_salary_earners_station_availability_v2.csv")


# Check for NA's
missing <- mice(pa.station.salary.earners, m = 5, method = 'sample', seed = 876, print = FALSE)
missing <- mice::complete(missing)
pa.station.salary.earners_no.miss <- na.omit(missing)

# write.csv(pa.station.salary.earners_no.miss, './processed/v2/pa_availability_no_missing_v2.csv')

# Check for Outliers
outlies <- pa.station.salary.earners_no.miss[-c(1:2,32)]
outlies <- outlies %>% outlieR::impute(flag = NULL, fill = 'mean', level = 0.1,
                                       nmax = NULL, side = NULL, crit = 'lof',
                                       k = 5, metric = 'euclidean', q = 3)
pa.station.salary.earners_no.outliers <- tibble(cbind(outlies, 'Zip.Code' = pa.station.salary.earners_no.miss$Zip.Code,
                                                                 'County' = pa.station.salary.earners_no.miss$County,
                                                                 'HasAvailability' = pa.station.salary.earners_no.miss$HasAvailability)
                                                ) %>% 
  relocate(County) %>% 
  relocate(Zip.Code)

# write.csv(pa.station.salary.earners_no.outliers, './processed/v2/pa_availability_no_outliers_v2.csv')
                                               
# Normalize the Data
preProClean <- preProcess(x = pa.station.salary.earners_no.outliers, method = c('scale', 'center'))
pa.availability.clean <- predict(preProClean, pa.station.salary.earners_no.outliers %>%  na.omit)

# write.csv(pa.availability.clean, './processed/v2/pa_availability_cleaned_v2.csv')

# rm(list=ls())

