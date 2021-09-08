import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np


data = pd.read_excel(io = 'EDA_v1.xlsx', sheet_name = 'data')


faltantes = pd.DataFrame(data.isna().sum(axis = 0), columns = ['faltantes'])


#Permitir ver todas as linhas
pd.options.display.max_rows = None


faltantes.sort_values(by = 'faltantes', ascending = False)


data = pd.read_excel(io = 'EDA_v2.xlsx', sheet_name = 'TabelaDores')


data.drop('ID', axis = 1).corr()



