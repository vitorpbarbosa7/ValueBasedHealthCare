import os
os.chdir('C:\GD\DS\Lefort\ML')

# %%
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt 
import numpy as np

# %%
data = pd.read_csv('data/alldata.csv', sep = ',')

# %% Selecionar principais variaveis:
    
#Crosstable
    
crosstable = pd.read_csv('data/CrosstableVariables.csv', sep = ';')
crosstable.drop(crosstable.columns[[0,4,5,6,7,8,9,10]], axis = 1, inplace =  True)

#Apenas dor na lombar
crosstable = crosstable.loc[crosstable['denominador']=='D30']

#Filtro da frequência
crosstable = crosstable.loc[crosstable['Frequência']>0.6]

crossnames = list(crosstable.numerador)

# Bisserial
biserial = pd.read_csv('data/biseralmaincorrelations.csv', sep = ',')
biserial.drop(biserial.columns[[3,4,5,6]], axis = 1, inplace = True)

#Apenas dor na lombar
biserial = biserial.loc[biserial['Binaria']=='Lombalgia']

#Filtro do coeficiente de correlação de ponto bisserial
biserial = biserial.loc[(biserial['rpb'] > 0.5) | (biserial['rpb'] < -0.5)]

biserialnames = list(biserial.Ordinal)
#Final

biserialandcross = crossnames + biserialnames

data = data[biserialandcross]

#Retornar apenas as linhas que possuem resultado em D30

data = data[data.D30.notnull()]
data.drop('Há quanto tempo em São Paulo se de outro local', axis = 1, inplace = True)

#Retornar apenas as linhas que possuem menos de 10 % de valores nulos
nulldataframe = pd.DataFrame(data.isnull().sum())
miss = nulldataframe/data.shape[0]

miss.columns = ['Valor']

miss = miss[miss['Valor'] < 0.15]

listanaonulos = list(miss.index)

data = data[listanaonulos]
# %% Imputar valores nulos
from sklearn.impute import SimpleImputer

imp = SimpleImputer(missing_values = np.nan, strategy = 'median')

imp.fit(data)

data_nonan = pd.DataFrame(imp.transform(data), columns = list(data))

data_nonan.isnull().sum().sum()

#Testando só com binários:
data_pca = data_nonan

# %% Normalização dos ordinais

data_bin = data_pca.iloc[:,np.r_[0:7]]
data_ord = data_pca.iloc[:,np.r_[7:10]]

from sklearn.preprocessing import StandardScaler
ss = StandardScaler()

ss.fit(data_ord)

data_ord_ss = pd.DataFrame(ss.transform(data_ord), columns = list(data_ord))

data_pca = pd.concat([data_bin,data_ord_ss], axis = 1)

# %% PCA

from sklearn.decomposition import PCA

df = data_pca

X = df.drop('D30', axis = 1)
#pca_model = PCA(n_components = 10)
#pca_model.fit(X)
#X_pca = pca_model.transform(X)
X_pca = X

y = df['D30']


# PCA para observar a variância explicada
pca = PCA()
pca.fit(X)
pca.explained_variance_ratio_


# PCA com dois componentes para realizar o plot
pca2 = PCA(n_components = 3)
pca2.fit(X)

components = pca2.fit_transform(X)

df_pca = pd.DataFrame(data = components, columns = ['PC1','PC2','PC3'])

df_pca = pd.concat([df_pca, y], axis = 1)
df_pca.columns = ['PC1','PC2','PC3','classe']

df_pca['classe'] = df_pca['classe'].astype(bool)


#Plot3D

%matplotlib inline
from mpl_toolkits.mplot3d import Axes3D
import matplotlib as mpl

def plot3d(data, width = 8, height = 6):
    fig = plt.figure(figsize = (width, height))
    ax = Axes3D(fig)
    
    x0 = list(data.loc[data['classe']==0, list(data)[0]])
    y0 = list(data.loc[data['classe']==0, list(data)[1]])
    z0 = list(data.loc[data['classe']==0, list(data)[2]])
    ax.scatter(x0,y0,z0,s=200, c = 'r', label = '0', marker = 'o')

    x1 = list(data.loc[data['classe']==1, list(data)[0]])
    y1 = list(data.loc[data['classe']==1, list(data)[1]])
    z1 = list(data.loc[data['classe']==1, list(data)[2]])
    ax.scatter(x1,y1,z1,s=200, c = 'g', label = '1', marker = 'v')

    ax.set_xlabel(list(data)[0])
    ax.set_ylabel(list(data)[1])
    ax.set_zlabel(list(data)[2])
    
    plt.legend()

    return plt.show()

plot3d(df_pca, width=10, height=8)    

#Plot component

from plotnine import *

(ggplot(df_pca) + 
 aes(x = 'PC1', y= 'PC2') + 
 geom_point(aes(fill = 'classe')))

# %% Divisao entre treino e teste
df = data_pca

X = df.drop('D30', axis = 1)
#pca_model = PCA(n_components = 10)
#pca_model.fit(X)
#X_pca = pca_model.transform(X)
X_pca = X

y = df['D30']

from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X_pca,y, test_size = 0.3)

df_test = pd.concat([pd.DataFrame(X_test), pd.DataFrame(y_test)], axis = 1)

df_test.to_csv('C:/GD/DS/Lefort/ML/data/TesteCoeficientes/df_test.csv')

# %% Regressão Logística

from sklearn.linear_model import LogisticRegression

lr = LogisticRegression()
lr.fit(X_train,y_train)

THRESHOLD = 0.45
previsoes = np.where(lr.predict_proba(X_test)[:,1] > THRESHOLD, 1, 0)

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes); acuracia
matriz = confusion_matrix(y_test, previsoes); matriz

# %% Com validação cruzada:
    
from sklearn.model_selection import cross_validate

def cvfunction(model):
    acuracia = cross_validate(model, X, y, cv = 10, 
                              scoring = ['accuracy','f1','recall','precision'],
                              return_train_score = True)
    
    f1 = acuracia['test_f1'].mean()
    precision = acuracia['test_precision'].mean()
    recall = acuracia['test_recall'].mean()
    acuracia = acuracia['test_accuracy'].mean()
    
    cvresults = pd.DataFrame({'acuracia':[acuracia],
                              'precisao':[precision],
                              'recall':[recall],
                              'f1':[f1]})
    
    return cvresults

cvresults = cvfunction(lr)

cvresults
                          

# %% Neuralnet classifier

import keras
from keras.models import Sequential 
from keras.layers import Dense

classificador = Sequential()
neurons = int(round(X_train.shape[1]/2,2))
classificador.add(Dense(units = neurons + 5 , activation = 'relu', input_dim = int(X_train.shape[1])))
classificador.add(Dense(units = 1, activation = 'sigmoid'))

classificador.compile(optimizer = 'adam', loss = 'binary_crossentropy', 
                      metrics = ['accuracy'])

classificador.fit(X_train, y_train.values, batch_size = 5, epochs = 200)

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)


#%% Random Forest
from sklearn.ensemble import RandomForestClassifier

classificador = RandomForestClassifier(n_estimators=20, 
                                       criterion = 'gini', 
                                       max_depth = 50)

cvresults = cvfunction(classificador)

classificador.fit(X_train,y_train)

previsoes = classificador.predict(X_test)

df_predict = pd.DataFrame({'Real':y_test, 'Previsao':previsoes})

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)

cvresults

# %%SVM 
from sklearn.svm import SVC

svm = SVC(kernel = 'rbf', 
          C = 1, 
          gamma = 'scale')

cvresults = cvfunction(svm)

svm.fit(X_train, y_train)

previsoes = svm.predict(X_test)

df_predict = pd.DataFrame({'Real':y_test, 'Previsao':previsoes})

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)

cvresults























