rm(list = ls())
setwd('C:/GD/DS/Lefort/RelevanciaEstatistica')

library(tidyverse)
library(foreign)

Bin_Got_BPI_RMDQ_DN = read.csv('indata/Bin_Got_BPI_RMDQ_DN.csv', encoding = 'UTF-8')

datacorr = Bin_Got_BPI_RMDQ_DN
datacorr[1] = NULL


# Buscar pela relevância estatística destes dados -------------------------
library(gmodels)
cross = CrossTable(datacorr$Desempregado, datacorr$D30, chisq = TRUE)

#Valor de chi quadrado calculado
chicalc = as.numeric(cross$chisq[1])

#Valor de chi quadrado tabelado para gl = (2 linhas - 1)*(2colunas - 1) da tabela de contingência e alpha = 0.05
chitab = qchisq(0.05, (2-1)*(2-1))

#Desse modo, o valor do p-valor é:
pchisq(chicalc, df = 1)

#Valor de p muito grande, logo a probabilidade de rejeitar a hipotese nula e estar errado eh muito grande, desse modo nao 
#rejeito a hipotese nula de independencia (a hipotese alternativa eh de dependencia)
