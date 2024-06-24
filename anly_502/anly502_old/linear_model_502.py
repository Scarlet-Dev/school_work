# -*- coding: utf-8 -*-
"""
Created on Sun Sep 19 14:34:37 2021

@author: Arkane
"""

import pandas as pd
import numpy as np
import sklearn

from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression

import matplotlib.pyplot as plt

X = pd.read_csv('D:/raw_data/covid_data_features.csv', parse_dates=['date'])
Y = pd.read_csv('D:/raw_data/covid_data_val.csv')
X.shape, Y.shape
x_in_use = X[(X.index < 2000000)]
y_in_use = Y[Y.index < 2000000]
x_in_use.shape, y_in_use.shape
2000000 < X.shape[0]
x_train, x_test, y_train, y_test = train_test_split(x_in_use, y_in_use, test_size=0.01)
x_train.shape, x_test.shape, y_train.shape, y_test.shape
lr = LinearRegression()
reg = lr.fit(x_train.drop('date', axis=1).values, y_train.values)
reg.score(x_train.drop('date', axis=1).values, y_train.values)
y_pred = reg.predict(x_test.drop('date', axis=1).values)
y_pred
y_pred = np.where(y_pred < 0, 0, y_pred)
y_pred.shape, y_test.shape
plt.figure(figsize=(20, 10))
SIZE = 100
plt.plot(np.arange(SIZE), y_pred[:SIZE], label='predict')
plt.plot(np.arange(SIZE), y_test[:SIZE], label='ground truth')
plt.legend()
get_ipython().run_line_magic('save', 'linear_regression.py 1-10000000')
plt.figure(figsize=(20, 10))
SIZE = 100
plt.plot(np.arange(SIZE), y_pred[:SIZE], label='predict')
plt.plot(np.arange(SIZE), y_test[:SIZE], label='ground truth')
plt.legend()
plt.savefig(f'pred_death_{SIZE}.png')