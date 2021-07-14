'''Criar dados com one hot encoder'''

import pandas as pd 
import numpy as np

#SE NÃO COLOCAR O INT64, VAI DAR ERRO NO R 
data = pd.read_csv('data.csv')

#Colunas que na verdade 0 é NaN, porque não tem a opção 0 no questionário
cols = ['Fuma','Cirurgia_coluna','Desempregado','Qual_sintoma']
data1 = data.copy()
for ft in cols:
    data1[ft] = data1[ft].replace({0:np.nan}, inplace=False).astype('Int64')
    
#Inverter as respostas para deixar no padrão binário certo 
#1) não pode ser SIM, e 2) não pode ser NÃO. 
#Tem que ser o contrário!!!
cols = ['Fuma','Cirurgia_coluna','Desempregado']
data2 = data1.copy()
for ft in cols:
    data2[ft] = data2[ft].replace({2:0}, inplace=False).astype('Int64')

#Aplicar bin para variáveis com opções 1), 2), 3)
data3 = data2.copy()
cols = ['Fuma','Cirurgia_coluna','Aposentado','Licenca_Medica',
        'Aposentado_Invalidez','Analgésicos','OutraDoenca',
        'Desempregado']
#Apply bin 
#https://stackoverflow.com/questions/21287624/convert-pandas-column-containing-nans-to-dtype-int
for ft in cols:
    data3[ft] = pd.cut(data3[ft], bins = [-0.5,0.9,5], labels = False).astype('Int64')

#Bin last question: Possibilidade de voltar a trabalhar
col = ['Retornar_trabalho']
data3[col[0]] = pd.cut(data3[col[0]], bins = [-.5,+4.5,+6.5], labels = False).astype('Int64')
    
#Transformar para Int64 as variáveis que já são binárias
bincols = ['Desempregado','Dor_costas','Dor_pernas']
for ft in bincols:
    data3[ft] = data3[ft].astype('Int64')

#One Hot Encoder Dummy

#O padrão do get_dummies retorna 0 para linhas NaN, o que é incorreto
def dumNaN(dfin):
    df = dfin.copy()
    dfsum = df.sum(axis = 1)
    for i in range(df.shape[0]):
        if dfsum[i] == 0:
            df.iloc[i,:] = np.nan
    return(df)

#Sexo
sexo = pd.DataFrame(pd.get_dummies(data['Sexo'])).astype('Int64')
sexo.columns = ['Masc','Fem']
sexo = dumNaN(sexo.copy())

#Estado Civil
civil = pd.DataFrame(pd.get_dummies(data['Estado_Civil'])).astype('Int64')
civil.columns = ['Casado_Amasiado','Solteiro','Separado_Divorciado','4','5','6']
civil = dumNaN(civil.copy())

#Sintoma livrar
sintomalivrar = pd.DataFrame(pd.get_dummies(data['Qual_sintoma'])).astype('Int64')
sintomalivrar.columns = ['Livrar_costas','Livrar_perna','Livrar_perda_sens','Livrar_perda_forc']
sintomalivrar = dumNaN(sintomalivrar.copy())          

#Agora precisamos dar drop nas anteriores e colocar estas novas
data4 = data3.drop(columns = ['Sexo','Estado_Civil','Qual_sintoma']).copy()

#Concatenar os dataframes
data5 = pd.concat([data4, sexo, civil, sintomalivrar], axis = 1)

#Drop nas colunas com variáveis categóricas textuais
data6 = data5.drop(columns = list(data.select_dtypes(include = 'object'))).copy()

#Drop em variáveis que não são binárias
orddrop = ['Se_SP_haquantosanos','QtCirColuna','Trabalho_Cansativo',
           'Licenca_Tempo','Tempo_dor_costas','Tempo_dor_pernas',
           'Andar_velocidade','Dor_costas_VAS','Dor_pernas_VAS',
           'IDADE']
data7 = data6.drop(columns = orddrop).copy()

#Passar para csv e utilizar no R a matriz de correlação tetracórica
data7.to_csv('data7toR.csv')
data7.to_excel('data7toPivot.xlsx')



