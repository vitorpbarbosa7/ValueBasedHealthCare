'''Criar dados com one hot encoder'''

import pandas as pd 
import numpy as np

#Aplicar bin para variáveis com opções 1), 2), 3)
data = pd.read_csv('data.csv')
data2 = data
cols = ['Fuma','Cirurgia_coluna','Aposentado','Licenca_Medica','Aposentado_Invalidez','Analgésicos','OutraDoenca']

#Apply bin 
#https://stackoverflow.com/questions/21287624/convert-pandas-column-containing-nans-to-dtype-int
for ft in cols:
    data2[ft] = pd.cut(data2[ft], bins = [-0.5,0.9,5], labels = False).astype('Int64')

#Bin last question: Possibilidade de voltar a trabalhar
col = ['Retornar_trabalho']
data2[col[0]] = pd.cut(data2[col[0]], bins = [-.5,+4.5,+6.5], labels = False).astype('Int64')
    
#Transformar para Int64 as variáveis que já são binárias
bincols = ['Desempregado','Dor_costas','Dor_pernas']
for ft in bincols:
    data2[ft] = data2[ft].astype('Int64')

#Sexo
sexo = pd.DataFrame(pd.get_dummies(data['Sexo'])).astype('Int64')
sexo.columns = ['Masc','Fem']

#Estado Civil
civil = pd.DataFrame(pd.get_dummies(data['Estado_Civil'])).astype('Int64')
civil.columns = ['Casado_Amasiado','Solteiro','Separado_Divorciado','4','5','6']

#Sintoma livrar
sintomalivrar = pd.DataFrame(pd.get_dummies(data['Qual_sintoma'])).astype('Int64')
sintomalivrar.columns = ['Livrar_costas','Livrar_perna','Livrar_perda_sens','Livrar_perda_forc']

#Agora precisamos dar drop nas anteriores e colocar estas novas
data3 = data.drop(columns = ['Sexo','Estado_Civil','Qual_sintoma'])

#Concatenar os dataframes
data4 = pd.concat([data3, sexo, civil, sintomalivrar], axis = 1)

#Drop nas colunas com variáveis categóricas textuais
data5 = data4.drop(columns = list(data.select_dtypes(include = 'object')))

#Drop em variáveis que não são binárias
orddrop = ['Se_SP_haquantosanos','QtCirColuna','Trabalho_Cansativo',
           'Licenca_Tempo','Tempo_dor_costas','Tempo_dor_pernas',
           'Andar_velocidade','Dor_costas_VAS','Dor_pernas_VAS',
           'IDADE']
data6 = data5.drop(columns = orddrop)

#Passar para csv e utilizar no R a matriz de correlação tetracórica
data6.to_csv('data6toR.csv')



