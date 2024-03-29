import os
os.chdir('C:\GD\DS\Lefort\ML')

# %%
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt 
import numpy as np

# %%
data = pd.read_csv('data/fewmissing.csv', sep = ';')

# %%
from sklearn.impute import SimpleImputer

imp = SimpleImputer(missing_values = np.nan, strategy = 'median')

imp.fit(data)

data_nonan = pd.DataFrame(imp.transform(data), columns = list(data))

data_nonan.isnull().sum().sum()

data_bin = data_nonan.iloc[:,np.r_[0:97]]
data_ord = data_nonan.iloc[:,np.r_[98:data_nonan.shape[1]]]

# %% Normalização dos ordinais

from sklearn.preprocessing import StandardScaler
ss = StandardScaler()

ss.fit(data_ord)

data_ord_ss = pd.DataFrame(ss.transform(data_ord), columns = list(data_ord))

data_pca = pd.concat([data_bin,data_ord_ss], axis = 1)

# %% PCA

from sklearn.decomposition import PCA

pca = PCA(n_components=10)
pca.fit(data_pca)
pca.explained_variance_ratio_
pca2 = PCA(n_components = 2)
pca2.fit(data_pca)

components = pca2.fit_transform(data_pca)

df_pca = pd.DataFrame(data = components, columns = ['PC1','PC2'])
    
# %% Plot component

from plotnine import *

(ggplot(df_pca) + 
 aes(x = 'PC1', y= 'PC2') + 
 geom_point())

# %% ML - Random Forest

df = data_pca

X = df.drop('D30', axis = 1)
pca_model = PCA(n_components = 10)
pca_model.fit(X)
X_pca = pca_model.transform(X)

y = df['D30']

from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X_pca,y, test_size = 0.3)

# %% Regressão Logística

from sklearn.linear_model import LogisticRegression

lr = LogisticRegression()

lr.fit(X_train, y_train)

previsoes = lr.predict(X_test)

from sklearn.metrics import confusion_'matrix, accuracy_score

precisao = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)


# %% Neuralnet classifier

import keras
from keras.models import Sequential 
from keras.layers import Dense

classificador = Sequential()
neurons = int(round(X_pca.shape[1]/2,2))
classificador.add(Dense(units = neurons , activation = 'relu', input_dim = int(X_pca.shape[1])))
classificador.add(Dense(units = 1, activation = 'sigmoid'))

classificador.compile(optimizer = 'adam', loss = 'binary_crossentropy', 
                      metrics = ['accuracy'])

classificador.fit(X_train, y_train.values, batch_size = 5, epochs = 200)

from sklearn.metrics import confusion_matrix, accuracy_score

precisao = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)


#%%
from sklearn.ensemble import RandomForestClassifier

classificador = RandomForestClassifier(n_estimators=20, 
                                       criterion = 'gini', 
                                       max_depth = 50)

classificador.fit(X_train,y_train)

previsoes = classificador.predict(X_test)

df_predict = pd.DataFrame({'Real':y_test, 'Previsao':previsoes})

from sklearn.metrics import confusion_matrix, accuracy_score

precisao = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)



















