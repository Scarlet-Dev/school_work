# -*- coding: utf-8 -*-
"""
Created on Sun Sep 19 14:37:02 2021

@author: Arkane
"""

import pandas as pd
import re
import matplotlib.pyplot as plt
from os.path import join

 


dataPath = 'D:/raw_data' # TODO: change your dir
confirmedPath = 'time_series_covid19_confirmed_US.csv'
deathPath = 'time_series_covid19_deaths_US.csv'
usRegions = 'us_state_by_region.csv'
df_confirmed = pd.read_csv(join(dataPath, confirmedPath))
df_deaths = pd.read_csv(join(dataPath, deathPath))
df_confirmed.shape, df_deaths.shape
confirmed_cases = df_confirmed.set_index(df_confirmed.columns[:11].tolist()).stack().reset_index()
confirmed_cases.rename(columns={'level_11': 'date', 0: 'confirmed'}, inplace=True)
region = pd.read_csv(join(dataPath, usRegions)) # TODO: change your dir

 


death_cases = df_deaths.set_index(df_deaths.columns[:12].tolist()).stack().reset_index()
df_confirmed.shape
df_deaths.shape
death_cases.rename(columns={'level_12': 'date', 0: 'death'}, inplace=True)
death_cases.shape[0] * 1.0 / df_deaths.shape[0]
death_cases.shape, confirmed_cases.shape
confirmed_cases[['Lat', 'Long_']].drop_duplicates().shape, confirmed_cases['Combined_Key'].nunique()
confirmed = confirmed_cases[['Combined_Key', 'date', 'confirmed', 'Province_State']]
confirmed = confirmed.merge(region, left_on = 'Province_State', right_on = 'State', how='left')
confirmed = confirmed[['Combined_Key', 'date', 'State', 'Region', 'confirmed']]
confirmed.head()
deaths = death_cases[['Combined_Key', 'Population', 'date', 'death']]
covid_data = confirmed.merge(deaths, on =['Combined_Key', 'date'])
confirmed.shape, deaths.shape, covid_data.shape
covid_data.head()
tmp_Region = pd.get_dummies(covid_data['Region'], prefix='Region_')
tmp_State = pd.get_dummies(covid_data['State'], prefix='State_')
X = covid_data[['date', 'confirmed', 'Population']]
X['date'] = pd.to_datetime(X['date'], format='%m/%d/%y')
X = pd.concat([X, tmp_Region, tmp_State], axis=1)
X.head()
Y = covid_data['death']
X.shape
Y.shape

 

# save preprocessed data
X.to_csv('D:/raw_data/covid_data_features.csv', index=False)
Y.to_csv('D:/raw_data/covid_data_val.csv', index=False)