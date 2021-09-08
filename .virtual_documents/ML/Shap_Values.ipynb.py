# get_ipython().run_line_magic("%", "")
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt 
import numpy as np


# get_ipython().run_line_magic("%", "")
data = pd.read_csv('data/alldata.csv', sep = ',')

# get_ipython().run_line_magic("%", " Selecionar principais variaveis:")
    
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
# get_ipython().run_line_magic("%", " Imputar valores nulos")
from sklearn.impute import SimpleImputer

imp = SimpleImputer(missing_values = np.nan, strategy = 'median')

imp.fit(data)

data_nonan = pd.DataFrame(imp.transform(data), columns = list(data))

data_nonan.isnull().sum().sum()

#Testando só com binários:
data_pca = data_nonan

# get_ipython().run_line_magic("%", " Normalização dos ordinais")

data_bin = data_pca.iloc[:,np.r_[0:7]]
data_ord = data_pca.iloc[:,np.r_[7:10]]

from sklearn.preprocessing import StandardScaler
ss = StandardScaler()

ss.fit(data_ord)

data_ord_ss = pd.DataFrame(ss.transform(data_ord), columns = list(data_ord))

data_pca = pd.concat([data_bin,data_ord_ss], axis = 1)


# get_ipython().run_line_magic("%", " Divisao entre treino e teste")
df = data_pca


X = df.drop('D30', axis = 1)
#pca_model = PCA(n_components = 10)
#pca_model.fit(X)
#X_pca = pca_model.transform(X)
X_pca = X


y = df['D30']


from sklearn.model_selection import train_test_split


X_train, X_test, y_train, y_test = train_test_split(X_pca,y, test_size = 0.3)


# get_ipython().run_line_magic("%", " XBoost")
from xgboost import XGBClassifier
from sklearn.model_selection import RepeatedStratifiedKFold, cross_validate


xboost = XGBClassifier(n_estimators=1000, max_depth=4, eta=0.3, subsample=0.7, colsample_bytree=0.7)


xboost.fit(X_train, y_train)


import shap 


explainer = shap.TreeExplainer(xboost)


shap_values = explainer.shap_values(X_train)


shap.summary_plot(shap_values, features=X_train, feature_names=X_train.columns)


shap.dependence_plot("Dor_pernas", shap_values, X_train)


shap.dependence_plot("D29", shap_values, X_train)
