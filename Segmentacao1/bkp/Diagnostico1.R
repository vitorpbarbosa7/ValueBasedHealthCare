setwd('C:/GD/DS/Lefort/Segmentacao1')

#Inicialmente diagnostico da primeira porção das 5 partes do questionário

library(tidyverse)

data = read.csv('data5toR.csv', encoding = 'UTF-8')
names(data)[1]= "ID"
data[1] = NULL #index do python

#Converter tudo para inteiro (Dá para converter no excel também )

# OneHotEncoder e renomear ------------------------------------------------
datacorr = data
datacorr[1] = NULL


# Biblioteca que permite executar a correlação tetracórica ----------------
library(psych)

corrmatrix = data.frame(tetrachoric(datacorr, correct = FALSE)$rho)

#Analisar dor nas costas
costas = data.frame(cbind(names(corrmatrix),corrmatrix$Dor_costas))
names(costas)[1:2] = c('fatores','costas')

library(dplyr)

costas[order(costas),]
