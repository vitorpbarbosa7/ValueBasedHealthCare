'''Este código possui o objetivo de fornecer correlações entre os variáveis 
contínuas e entre as variáveis contínuas e binárias'''

import pandas as pd 
import numpy as np
import plotnine
import seaborn as sns
import matplotlib.pyplot as plt

# %%
data = pd.read_csv('data.csv', encoding = 'UTF-8', sep = ",") 

#Variáveis continuas
ordinalvar = ['Se_SP_haquantosanos','QtCirColuna','Trabalho_Cansativo',
           'Licenca_Tempo','Tempo_dor_costas','Tempo_dor_pernas',
           'Andar_velocidade','Dor_costas_VAS','Dor_pernas_VAS',
           'IDADE']
#Variáveis continuas
ordinaldf = data[ordinalvar]

# %% Dado que são variáveis contínuas, podemos utilizar PairPlot
sns.pairplot(data)
