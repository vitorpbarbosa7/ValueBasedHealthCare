setwd('C:/GD/DS/Lefort/PCA')

data = read.csv('C:/GD/DS/Lefort/PCA/indata/Binaria_Ordinal_Q123456_HALF_SEMMISSINGBPI.csv')
data[c(1,2)] = NULL

#Porcentagem de valores faltantes:
missing = sum(is.na(data))/(nrow(data)*ncol(data))*100

#Aplicar o PCA
library(pcaMethods)

pca = nlpca(data, nPcs = 2)
