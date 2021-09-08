'''Criar dados com one hot encoder'''
import pandas as pd 
import numpy as np

#SE NÃO COLOCAR O INT64, VAI DAR ERRO NO R 
data = pd.read_excel('TesteOneHotEncoder_2.xlsx')
    
#Foi difícil descobrir como fazer
#https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.str.get_dummies.html
#https://stackoverflow.com/questions/21292552/equivalent-of-paste-r-to-python

#Inicializar a lista que vai receber os objetos do tipo "1|2|3"
strwords = list(np.zeros(len(data)))
for i in range(0,len(data)):
    lista = [str(number) for number in list(data.iloc[i,:])]
    strwords[i] = "|".join(lista) 
    
'''Com este lista de números separados por | extraido de cada linha, conseguimos
depois fazer um OneHotEncoder geral'''
    
dfwords = pd.DataFrame({"A":strwords})

dummies = dfwords.A.str.get_dummies()
dummies.drop('nan',axis = 1, inplace = True)

#Renomear essas colunas
original_names = list(dummies.columns.astype('str'))

new_names = ["D" + item for item in original_names]

dummies.columns = new_names

#Gravar para csv agora
dummies.to_csv('dorescorpo.csv')
