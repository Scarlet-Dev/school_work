
library("reshape")
library("ggplot2")
library("psych")
library("moments")
library("dplyr")



# Load dataset
# link: //github.com/JieYingWu/COVID-19_US_County-level_Summaries
# link: //github.com/CSSEGISandData/COVID-19

region = read.csv('./us_state_by_region.csv')
county = read.csv('./counties.csv')

df_confirmed = read.csv('./c19_time_series_confirmed.csv')
df_deaths = read.csv('./c19_time_series_deaths.csv')

dim(region)
dim(county)

dim(df_confirmed)
dim(df_deaths)
str(df_confirmed)
str(df_deaths)

# get the columns to simplify county with selected columns
basics= c("FIPS", "State", "Area_Name") 
age_gr=c('Total_age0to17','Male_age0to17','Female_age0to17',
         'Total_age18to64','Male_age18to64','Female_age18to64','Total_age65plus',
         'Male_age65plus','Female_age65plus','Total_age85plusr','Male_age85plusr','Female_age85plusr')

hospital = c('Total Hospitals (2019)')

edu = c(
   'Percent.of.adults.with.less.than.a.high.school.diploma.2014-18',
   'Percent.of.adults.with.a.high.school.diploma.only.2014-18',
   "Percent.of.adults.completing.some.college.or.associate's.degree.2014-18",
   "Percent.of.adults.with.a.bachelor's.degree.or.higher.2014-18"
)

income=c('Employed_2018',
         'Unemployed_2018',
         'Unemployment_rate_2018',
         'Median_Household_Income_2018',
         'ICU.Beds'
)

county_columns = c(basics, age_gr, hospital, edu, income)
print(county_columns)


# county level data: 
simpleCounty = county
dim(simpleCounty)
str(simpleCounty)


df_deaths2 =  merge (x = df_deaths, y = region, by.x='Province_State', by.y='State', x.all=TRUE, y.all=FALSE)
dim(df_deaths2)

df_deaths3 = merge(x=df_deaths2, y = simpleCounty, by.x = c('State.Code', 'FIPS'), by.y = c('State', 'FIPS'))

dim(df_deaths3)

index_columns = names(df_deaths3)[-grep("^X", colnames(df_deaths3))]
print(index_columns)

# TODO: death_cases and confirmed cases
id_columns = names(df_deaths3)[-grep("^X", colnames(df_deaths3))]
measure_columns = names(df_deaths3)[grep("^X", colnames(df_deaths3))]

id_columns_confirmed = names(df_confirmed)[-grep("^X", colnames(df_confirmed))]
measure_columns_confirmed = names(df_confirmed)[grep("^X", colnames(df_confirmed))]


death_cases = melt(df_deaths3, id.vars = id_columns, measure.vars = measure_columns)
# operation takes too long, so read from disk.
#death_cases = read.csv('./death_cases.csv')
#confirmed_cases = read.csv('./confirmed_case.csv')
confirmed_cases = melt(df_confirmed, id.vars = id_columns_confirmed, measure.vars = measure_columns_confirmed)

# merging the data
dim(death_cases)
dim(confirmed_cases)

date_column_index = which(colnames(confirmed_cases) == 'variable')
print('the index of date column index: ')
print(date_column_index)
names(confirmed_cases)[date_column_index] = 'date'

confirmed_column_index = which(colnames(confirmed_cases) == 'value')
print(confirmed_column_index)
names(confirmed_cases)[confirmed_column_index] = 'confirmed'

confirmed_cases$date = gsub("X","",as.character(confirmed_cases$date))
confirmed_cases$date = gsub("\\.", '/', as.character(confirmed_cases$date))



confirmed = confirmed_cases[,c('Combined_Key', 'date', 'confirmed', 'Province_State')]
dim(confirmed)


confirmed = merge(x = confirmed, y = region, by.x = 'Province_State', by.y = 'State', x.all=TRUE, y.all=FALSE)
confirmed = confirmed[, c('Combined_Key', 'date', 'State.Code', 'Region', 'confirmed')]

dim(confirmed)

deaths = death_cases

# merging the data

print('check out the data')

head(confirmed)

head(deaths)

date_column_index = which(colnames(deaths) == 'variable')
print('the index of date column index: ')
print(date_column_index)
names(deaths)[date_column_index] = 'date'

death_column_index = which(colnames(deaths) == 'value')
print(death_column_index)
names(deaths)[death_column_index] = 'death'

# X1/12/20 => 1/12/20 converting
deaths$date = gsub("X","",as.character(deaths$date))
deaths$date = gsub("\\.", '/', as.character(deaths$date))

# this is the transformed covid data

covid_data = merge(x = confirmed, y = deaths, by = c('Combined_Key', 'date', 'Region', 'State.Code'))
dim(covid_data)

#library('tidytable')
#tmp_Region = get_dummies.(covid_data['Region'], prefix_sep = 'Region_')
#tmp_State = get_dummies.(covid_data['State_x'], prefix_sep = 'State_')

#tmp_Region_fixed = tmp_Region[, -c(1)]
#tmp_State_fixed = tmp_State[, -c(1)]

# get training and testing data

#X = cbind(covid_data, tmp_Region_fixed, tmp_State_fixed)
X = covid_data
dim(X)


death_column_index2 = which(colnames(X) == 'death')
print('death column index')
print(death_column_index2)

X_new = X[, -c(death_column_index2)]
dim(X_new)

Y_new = X[c(death_column_index2)]
dim(Y_new)
# at this point, we have the training data


# non-training columns
columns_to_align = c('Combined_Key', 'date', 'Region', 'UID', 'iso2', 'iso3', 
                     'code3', 'FIPS', 'Admin2', 'Province_State', 'Country_Region', 'Lat', 'Long_',
                     'State.Code', 'Division', 'Area_Name')


# could be improved on....
columns_to_align_index = rep(NA, length(columns_to_align))
i = 1
for (col in columns_to_align){
   date_column_index_tmp = which(colnames(X_new) == col)
   print(date_column_index_tmp)
   columns_to_align_index[i] = date_column_index_tmp
   i = i + 1
}
print(columns_to_align_index)


X_for_training = X_new[, -c(columns_to_align_index)]

Y_for_training = Y_new

# convert to numeric data
X_for_training = apply(X_for_training, 2, function(x) as.numeric(as.character(x)))
Y_for_training = as.numeric(Y_for_training)


data = cbind(X_for_training, Y_for_training)

dim(data)

dim(X_for_training)
dim(Y_for_training)


# start linear regression model
chooseSampleSize = 10000 # NOTE: 

X_in_use = X_for_training[c(seq(1+chooseSampleSize:chooseSampleSize * 2)), ]
Y_in_use = Y_for_training[c(seq(1+chooseSampleSize:chooseSampleSize * 2)), ]

dim(X_in_use)
length(Y_in_use)
str(X_in_use)
str(Y_in_use)

sapply(X_in_use, class)

data_in_use = cbind(X_in_use, Y_in_use)
dim(data_in_use)

# get the data that will be for training

data_in_use = na.omit(data_in_use)
dim(data_in_use)
sum(is.na(data_in_use))

last_index = length(data_in_use)
print(last_index)

X_in_use = data_in_use[, -c(last_index)]
Y_in_use = data_in_use[, c(last_index)]

dim(X_in_use)
length(Y_in_use)

# this section, try to convert columns to numeric

Totalhousehold_columns = names(X_in_use)[grep("^Total.household", colnames(X_in_use))]
for (col in Totalhousehold_columns){
   print(col)
   X_in_use[, col] = as.numeric(X_in_use[, col])
   
}
X_in_use$HOUSEHOLDS.BY.TYPE.. = as.numeric(X_in_use$HOUSEHOLDS.BY.TYPE..)


Relationship_columns = names(X_in_use)[grep("^RELATIONSHIP", colnames(X_in_use))]
for (col in Relationship_columns){
   print(col)
   X_in_use[, col] = as.numeric(X_in_use[, col])
   
}

Martial_columns = names(X_in_use)[grep("^MARITAL", colnames(X_in_use))]
for (col in Martial_columns){
   print(col)
   X_in_use[, col] = as.numeric(X_in_use[, col])
   
}

School_columns = names(X_in_use)[grep("^SCHOOL", colnames(X_in_use))]
for (col in School_columns){
   print(col)
   X_in_use[, col] = as.numeric(X_in_use[, col])
   
}

VETERAN_columns = names(X_in_use)[grep("^VETERAN", colnames(X_in_use))]
for (col in VETERAN_columns){
   print(col)
   X_in_use[, col] = as.numeric(X_in_use[, col])
   
}

DISABILITY_columns = names(X_in_use)[grep("^DISABILITY", colnames(X_in_use))]
for (col in DISABILITY_columns){
   print(col)
   X_in_use[, col] = as.numeric(X_in_use[, col])
   
}

# normalize data
min_max_norm <- function(x) {
   (x - min(x)) / (max(x) - min(x))
}

X_in_use_norm = as.data.frame(lapply(X_in_use, min_max_norm))
Y_in_use_norm = min_max_norm(Y_in_use)


#X_in_use = X_in_use %>% mutate_at(c(seq(1, length(X_in_use))), ~(scale(.) %>% as.vector))
#Y_in_use = Y_in_use %>% mutate_at(c(1), ~(scale(.) %>% as.vector))


# training the entire dataset

# year 2020 and 2021?????

models = rep(NA, length(X_in_use_norm))
i = 1
for (col in names(X_in_use_norm)) {
   model = lm(Y_in_use_norm ~ X_in_use_norm[, col])
   summary(model)
   models[i] = model
   i = i + 1
}

sum(is.na(X_in_use))

# base model
base_model = lm(Y_in_use_norm~., X_in_use_norm)
summary(base_model)

plot(base_model)

# better model

