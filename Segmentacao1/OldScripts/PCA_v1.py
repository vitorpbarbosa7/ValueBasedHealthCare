'''Análise de PCA e agrupamento para ver se é possível separar os dados'''

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np 

#%%
data = pd.read_csv('data.csv', encoding = 'UTF-8', sep = ',')

ordlist = orddrop = ['Se_SP_haquantosanos','QtCirColuna','Trabalho_Cansativo',
           'Licenca_Tempo','Tempo_dor_costas','Tempo_dor_pernas',
           'Andar_velocidade','Dor_costas_VAS','Dor_pernas_VAS',
           'IDADE']

#Dados com relação de ordinalidade
ordinaldf = data[ordlist].copy()

bindata = pd.read_csv('bindata7.csv')
bindata.drop(columns = ['Unnamed: 0', 'ID'], inplace = True)

df = pd.concat([ordinaldf,bindata], axis = 1)

#%% PCA do sklearn não lida com valores nulos
# Não deve fazer muito sentido mesmo fazer isso com dados faltantes
from ppca import PPCA

ppca = PPCA()

ppca.fit(df.values)

var_exp = ppca.var_exp

#%% K-Means

from sklearn.cluster import KMeans

# Número de clusters?
wcss = []
for i in range(1,10):
    kmeans = KMeans(n_clusters=i,
                    random_state=0)
    kmeans.fit(df.values)
    wcss.append(kmeans.inertia_)
    
plt.plot(wcss)  
