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

#Gravar para fazer tudo de PCA no R:
X_pca.to_csv('outputdata/Xpcadata.csv', index = False)  

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

df_pca.to_csv('outputdata/pca.csv')


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

lr = LogisticRegression(penalty = 'elasticnet', 
                        C = 0.9, 
                        l1_ratio = 0.2,
                        solver = 'saga')
lr.fit(X_train,y_train)

THRESHOLD = 0.45
previsoes = np.where(lr.predict_proba(X_test)[:,1] > THRESHOLD, 1, 0)

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes); acuracia
matriz = confusion_matrix(y_test, previsoes); matriz

# %%ROC Curve

from sklearn.metrics import roc_curve, roc_auc_score
import matplotlib.pyplot as plt

def roccurve(y_test, probs, modelname):
    # Gerar os dados da diagonal (no skill classifier)
    ns_probs = [0 for item in range(len(y_test))]
    ns_fpr, ns_tpr, ns_thres = roc_curve(y_test, ns_probs)
    
    #Probabilidades da classe positiva
    fpr, tpr, thresholds = roc_curve(y_test, probs[:,1])
    auc = roc_auc_score(y_test, probs[:,1])
    
    plt.plot(fpr, tpr, marker = '.', label = modelname)
    plt.plot(ns_fpr, ns_tpr, linestyle = '--', label = 'Classificador base')
    plt.xlabel('Razão de Falsos Positivos')
    plt.ylabel('Razão de Verdadeiros Positivos')
    plt.title('ROC Curve')
    plt.legend()
    plt.show()

probs = lr.predict_proba(X_test)

roccurve(y_test, probs, 'Regressão Logística')


# %% Precision Recall Curve 

from sklearn.metrics import precision_recall_curve

def precisionrecall(y_test, probs, modelname):
    
    precision, recall, thresholds = precision_recall_curve(y_test, probs[:,1])
    
    no_skill = len(y_test[y_test ==1]) / len(y_test)
    
    plt.plot([0,1], [no_skill, no_skill], linestyle = '--', label = 'Classificador base')
    plt.plot(precision, recall, marker = '.', label =  modelname)
    plt.xlabel('Recall')
    plt.ylabel('Precision')
    plt.title('Precision-Recall Curve')
    plt.legend()
    plt.show()
    
probs = lr.predict_proba(X_test)
modelname = 'Regressão Logística'

precisionrecall(y_test, probs, modelname)

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

import tensorflow as tf
import keras
from keras.models import Sequential 
from keras.layers import Dense

nn = Sequential()
neurons = int(round(X_train.shape[1]/2 + 1,2))
nn.add(Dense(units = neurons, activation = 'relu', input_dim = int(X_train.shape[1])))
nn.add(Dense(units = 1, activation = 'sigmoid'))

nn.compile(optimizer = 'adam', loss = 'binary_crossentropy', 
                      metrics = ['accuracy',
                                 tf.keras.metrics.Recall(),
                                 tf.keras.metrics.Precision()])
                                 
nn.fit(X_train, 
                  y_train.values, 
                  batch_size = 5, 
                  epochs = 200)

score = nn.evaluate(X_test, y_test, verbose = 1)

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)


# %% Árvore de decisão

from sklearn.tree import DecisionTreeClassifier

dt = DecisionTreeClassifier()

# cvresults = cvfunction(dt)

dt.fit(X_train,y_train)

previsoes = dt.predict(X_test)

df_predict = pd.DataFrame({'Real':y_test, 'Previsao':previsoes})

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)

# cvresults

# # %Visualize
# from sklearn import tree

# treenames = list(X_train)
# lombalgia = ['Não','Sim']

# fig, axes = plt.subplots(nrows = 1, ncols = 1, figsize = (8,8),
#                          dpi = 600)

# tree.plot_tree(dt,
#                feature_names = treenames, 
#                class_names = lombalgia, 
#                filled = True)

# fig.savefig('outputdata/img/Tree.png')
#%% Random Forest
from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier(n_estimators=50, 
                                       criterion = 'gini', 
                                       max_depth = 8)

# cvresults = cvfunction(rf)

rf.fit(X_train,y_train)

from sklearn.calibration import CalibratedClassifierCV
rf_isotonic = CalibratedClassifierCV(rf, method = 'isotonic')
rf_isotonic.fit(X_train, y_train)

previsoes = rf.predict(X_test)

df_predict = pd.DataFrame({'Real':y_test, 'Previsao':previsoes})

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)

rf_depths = [estimator.tree_.max_depth for estimator in rf.estimators_]

# cvresults

# %%SVM 
from sklearn.svm import SVC

svm = SVC(kernel = 'rbf', 
          C = 0.9, 
          gamma = 'scale', 
          probability = True)

# cvresults = cvfunction(svm)

svm.fit(X_train, y_train)

from sklearn.calibration import CalibratedClassifierCV
svm_cal = CalibratedClassifierCV(svm, method = 'sigmoid')
svm_cal.fit(X_train, y_train)

previsoes = svm.predict(X_test)

df_predict = pd.DataFrame({'Real':y_test, 'Previsao':previsoes})

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)

# cvresults

# %% XBoost
from xgboost import XGBClassifier
from sklearn.model_selection import RepeatedStratifiedKFold, cross_validate

xboost = XGBClassifier()

xboost.fit(X_train, y_train)

probs = xboost.predict_proba(X_test)

roccurve(y_test, probs, 'XBoost Classifier')
precisionrecall(y_test, probs, 'XBoost Classifier')

cv = RepeatedStratifiedKFold(n_splits=5, n_repeats=3, random_state=42)
# evaluate model
scores = cross_validate(xboost, X_pca, y, 
                          scoring=['roc_auc','accuracy','recall','precision','f1'],
                          cv=cv, n_jobs=-1,
                          verbose = True)

f1 = scores['test_f1'].mean()
precision = scores['test_precision'].mean()
recall = scores['test_recall'].mean()
acuracia = scores['test_accuracy'].mean()

cvresults = pd.DataFrame({'acuracia':[acuracia],
                              'precisao':[precision],
                              'recall':[recall],
                              'f1':[f1]})

previsoes = xboost.predict(X_test)

from sklearn.metrics import confusion_matrix, accuracy_score

acuracia = accuracy_score(y_test, previsoes)
matriz = confusion_matrix(y_test, previsoes)


# %%Plot geral

#Probabilidades de todos modelos
lr_probs = lr.predict_proba(X_test)
xboost_probs = xboost.predict_proba(X_test)
nn_probs = nn.predict_proba(X_test)
dt_probs = dt.predict_proba(X_test)
rf_probs = rf.predict_proba(X_test)
rf_iso_probs = rf_isotonic.predict_proba(X_test)
svm_probs = svm.predict_proba(X_test)

#Precision Recall
lr_precision, lr_recall, lr_thresholds = precision_recall_curve(y_test, lr_probs[:,1])
xboost_precision, xboost_recall, xboost_thresholds = precision_recall_curve(y_test, xboost_probs[:,1])
nn_precision, nn_recall, nn_thresholds = precision_recall_curve(y_test, nn_probs)
dt_precision, dt_recall, dt_thresholds = precision_recall_curve(y_test, dt_probs[:,1])
rf_precision, rf_recall, rf_thresholds = precision_recall_curve(y_test, rf_probs[:,1])
rf_iso_precision, rf_iso_recall, rf_iso_thresholds = precision_recall_curve(y_test, rf_iso_probs[:,1])
svm_precision, svm_recall, svm_thresholds = precision_recall_curve(y_test, svm_probs[:,1])

no_skill = len(y_test[y_test ==1]) / len(y_test)

#Precision Recall
fig1, ax1 = plt.subplots()
plt.plot([0,1], [no_skill, no_skill], linestyle = '--', label = 'Classificador base')
plt.plot(lr_precision, lr_recall, marker = '.', label =  'Regressão Logística')
plt.plot(xboost_precision, xboost_recall, marker = '.', label = 'XBoost Classifier')
plt.plot(nn_precision, nn_recall, marker = '.', label = 'Neural Net Classifier')
plt.plot(dt_precision, dt_recall, marker = '.', label = 'Decision Tree')
plt.plot(rf_precision, rf_recall, marker = '.', label = 'Random Forest')
plt.plot(rf_iso_precision, rf_iso_recall, marker = '.', label = 'Random Forest Calibrated')
plt.plot(svm_precision, svm_recall, marker = '.', label = 'Support Vector Machine')
plt.xlabel('Recall')
plt.ylabel('Precision')
plt.title('Precision-Recall Curve')
plt.legend()
plt.show()

fig1.savefig('outputdata/img/Precision_Recall_Curve.png', format = 'png', dpi = 1200)

# ROC Curve
# Gerar os dados da diagonal (no skill classifier)
ns_probs = [0 for item in range(len(y_test))]
ns_fpr, ns_tpr, ns_thres = roc_curve(y_test, ns_probs)

#Probabilidades da classe positiva
lr_fpr, lr_tpr, lr_thresholds = roc_curve(y_test, lr_probs[:,1])
xboost_fpr, xboost_tpr, xboost_thresholds = roc_curve(y_test, xboost_probs[:,1])
nn_fpr, nn_tpr, nn_thresholds = roc_curve(y_test, nn_probs)
dt_fpr, dt_tpr, dt_thresholds = roc_curve(y_test, dt_probs[:,1])
rf_fpr, rf_tpr, rf_thresholds = roc_curve(y_test, rf_probs[:,1])
rf_iso_fpr, rf_iso_tpr, rf_iso_thresholds = roc_curve(y_test, rf_iso_probs[:,1])
svm_fpr, svm_tpr, svm_thresholds = roc_curve(y_test, svm_probs[:,1])

fig2, ax2 = plt.subplots()
plt.plot(lr_fpr, lr_tpr, marker = '.', label = 'Regressão Logística')
plt.plot(xboost_fpr, xboost_tpr, marker = '.', label = 'XBoost Classifier')
plt.plot(nn_fpr, nn_tpr, marker = '.', label = 'Neural Net Classifier')
plt.plot(dt_fpr, dt_tpr, marker = '.', label = 'Decision Tree')
plt.plot(svm_fpr, svm_tpr, marker = '.', label = 'Support Vector Machine')

plt.plot(rf_fpr, rf_tpr, marker = '.', label = 'Random Forest')
plt.plot(rf_iso_fpr, rf_iso_tpr, marker = '.', label = 'Random Forest Calibrated')

plt.plot(ns_fpr, ns_tpr, linestyle = '--', label = 'Classificador base')
plt.xlabel('Razão de Falsos Positivos')
plt.ylabel('Razão de Verdadeiros Positivos')
plt.title('ROC Curve')
plt.legend()
plt.show()

fig2.savefig('outputdata/img/ROC_Curve.png', format = 'png', dpi = 1200)


















