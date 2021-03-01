# -*- coding: utf-8 -*-
"""IST718_Final_Project.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1IOLyp5zSxK2_7K9c_BjNmy5t7Y6icM7h
"""

import pandas as pd  # data frame operations
from pandas.plotting import scatter_matrix  # scatter plot matrix
from pandas import Series
import numpy as np  # arrays and math functions
from scipy.stats import uniform  # for training-and-test split
import statsmodels.api as sm  # statistical models (including regression)
import statsmodels.formula.api as smf  # R-like model specification
from sklearn.tree import DecisionTreeRegressor  # machine learning tree
from sklearn.ensemble import RandomForestRegressor # ensemble method
from matplotlib import pyplot as plt
from matplotlib import pylab

from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
from google.colab import auth
from oauth2client.client import GoogleCredentials

auth.authenticate_user()
gauth = GoogleAuth()
gauth.credentials = GoogleCredentials.get_application_default()
drive = GoogleDrive(gauth)

downloaded = drive.CreateFile({'id':"1TwkkoxOjnUPV8S8iIiI6QQQFyq9PScQ8"})
downloaded.GetContentFile('trust-healthcare-vs-vaccine-safety.csv')

trust_safety = pd.read_table('trust-healthcare-vs-vaccine-safety.csv', delimiter=',')
  #, header = None, \
  #   delim_whitespace = True, skipinitialspace = True, \
   #  names = ['value', 'income', 'age', 'rooms', 'bedrooms', \
    # 'pop', 'hh', 'latitude', 'longitude'])
# prelim_houses['idx'] = range(len(prelim_houses))  # for use as index
# houses = prelim_houses.set_index(['idx']) 
     
print(trust_safety.shape)
print(trust_safety.head())

plt.plot(trust_safety['TRUST_CLINICIANS_PCT'], trust_safety['VACCINES_NOT_SAFE_PCT'], '.', color='orange')
plt.xlabel('% of Population that Trusts Clinicians')
plt.ylabel('% of Population that Believes Vaccines are Not Safe')
plt.title('Trust in Clinicians vs. Belief in Vaccine Safety, 2018')
plt.xlim(right=100,left=0)
plt.ylim(top=40,bottom=0)

downloaded = drive.CreateFile({'id':"1wF2XY29BWSl7lIkRvf6QcRiYr-aCRbc7"})
downloaded.GetContentFile('FLU_RATE_FINAL.csv')

flu_rate = pd.read_table('FLU_RATE_FINAL.csv', delimiter=',')
print(flu_rate.head())

plt.plot(flu_rate['date'], flu_rate['6m_to_17y_coverage_rate'])
plt.xlabel('')
plt.ylabel('% Coverage')
plt.title('Flu Vaccine Coverage Rate (6m-17y)')
plt.ylim(top=100,bottom=0)

downloaded = drive.CreateFile({'id':"19YyRkXYoFOFPBNW-Wtml0VjwMWzGqDyv"})
downloaded.GetContentFile('posts_full_2.csv')

social = pd.read_table('posts_full_2.csv', delimiter=',')
social.head(5)

social = social[['article_host','text','timestamp','anti_vax']]
social['id'] = social.index
social.head()

social.groupby('anti_vax').agg('count')

social.dtypes

social['timestamp'] = pd.to_datetime(social['timestamp'])
social.dtypes

social['year'] = pd.DatetimeIndex(social['timestamp']).year
social['month'] = pd.DatetimeIndex(social['timestamp']).month
social['month_year'] = pd.to_datetime(social['timestamp']).dt.to_period('M')
social.head()

years = [2016,2017,2018,2019]
social = social[social.year.isin(years)]

fig, ax = plt.subplots(figsize=(12,7))
social.groupby(['month_year','anti_vax']).agg('count')['id'].unstack().plot(ax=ax)
plt.xlabel('', rotation=90)
plt.ylabel('Number of Posts')
plt.title('Vaccine Related Posts per Month on Facebook (2016-2019)')