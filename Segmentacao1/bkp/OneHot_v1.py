'''Criar dados com one hot encoder'''

import pandas as pd

data = pd.read_csv('Passo3.csv')

# #Sexo
sexo = pd.DataFrame(pd.get_dummies(data['Sexo']))
sexo.columns = ['Masc','Fem']

#Estado Civil
civil = pd.DataFrame(pd.get_dummies(data['Estado_Civil']))
civil.columns = ['Casado_Amasiado','Solteiro','Separado_Divorciado','4','5','6']

#Sintoma livrar
sintomalivrar = pd.DataFrame(pd.get_dummies(data['Qual_sintoma']))
sintomalivrar.columns = ['Livrar_costas','Livrar_perna','Livrar_perda_sens','Livrar_perda_forc']

#Agora precisamos dar drop nas anteriores e colocar estas novas
data2 = data.drop(columns = ['Sexo','Estado_Civil','Qual_sintoma'])

#Concatenar os dataframes
data3 = pd.concat([data2, sexo, civil, sintomalivrar], axis = 1)

#Drop nas colunas com variáveis categóricas textuais
lsdrop = ['Profissao','Estado_nascimento','CirColHospit','Se_SIM_quais_medicamentos',
          'Se_SIM_quais_doenças']
data4 = data3.drop(columns = lsdrop)

#Drop em variáveis que não são binárias
orddrop = ['Se_SP_haquantosanos','QtCirColuna','Trabalho_Cansativo',
           'Licenca_Tempo','Tempo_dor_costas','Tempo_dor_pernas',
           'Andar_velocidade','Dor_costas_VAS','Dor_pernas_VAS']

data5 = data4.drop(columns = orddrop)

#Pode converter tudo para inteiro:
data5 = data5.astype(int)




