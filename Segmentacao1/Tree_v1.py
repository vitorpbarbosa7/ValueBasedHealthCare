from os import chdir
chdir(path = 'C:\GD\DS\Lefort\Segmentacao1')
import pandas as pd
import matplotlib.pyplot as plt 
import numpy as np 
import seaborn as sns 

# %%
data = pd.read_csv('binaria_e_ordinal.csv', encoding = "ISO-8859-1")
data = data.drop(['Unnamed: 0',"ID"], axis = 1)

X = data.drop('Livrar_perna', axis = 1)
y = data['Livrar_perna']

X_train, X_test, y_train, y_test  = train_test_split(X, y, test_size = 0.3, random_state = 42)

# %% KNN algorithm 

from sklearn.neighbors import KNeighborsClassifier

knn = KNeighborsClassifier(n_neighbors = 10)

knn.fit(X_train, y_train)


# %%

from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier(n_estimators = 10,
                            criterion = "entropy", 
                            max_depth = 5, 
                            min_samples_split = 10, 
                            min_samples_leaf = 10, 
                            max_features = "auto", 
                            verbose = 1)

rf.fit(X_train, y_train)
