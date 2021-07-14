setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')

library(tidyverse)
library(GGally)
library(lattice)

# Objetivo deste código é conseguir visualizar correlações entre as variáveis contínuas, e entre as variáveis 
# contínuas e variáveis binárias

data = read.csv('data.csv', encoding = 'UTF-8', sep = ',')
names(ordinaldf)[1] = "ID"

ordinallist = c('Se_SP_haquantosanos','QtCirColuna','Trabalho_Cansativo',
                    'Licenca_Tempo','Tempo_dor_costas','Tempo_dor_pernas',
                    'Andar_velocidade','Dor_costas_VAS','Dor_pernas_VAS',
                    'IDADE')

ordinaldf = subset(x = data, select = ordinallist)

pairs(ordinaldf)

splom(ordinaldf)


# Juntar com as variáveis binárias ----------------------------------------
bindata = read.csv('bindata7.csv')
bindata[1] = NULL

#Não há relação de ordinalidade, logo, todos os dados do dataset bindata são
#Se não adicionarmos [], R não mantém a estrutura de fatores
bindata[] = lapply(bindata[], factor)

dfbind = cbind(ordinaldf,bindata)

library(reshape2)

meltdata = melt(data = dfbind, id.vars = names(bindata), measure.vars = names(ordinaldf))
names(meltdata)

#Dor nas pernas
ggplot(data = meltdata, aes(Dor_costas,value)) + 
  geom_boxplot(aes(fill = Desempregado)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") + 
  facet_wrap(.~variable, scales = "free")

#Dor nas costas
ggplot(data = meltdata, aes(Dor_pernas,value)) + 
  geom_boxplot(aes(fill = Desempregado)) + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") + 
  facet_wrap(.~variable, scales = "free")


# Coeficiente de correlação de ponto bisserial ----------------------------

library(polycor)

polyserial(as.numeric(dfbind$IDADE),dfbind$Dor_costas)


# Matriz de correlação  ---------------------------------------------------

polyserial(as.numeric(ordinaldf[i],bindata[1]))



























# Para tentar obter a matrix de correlação ponto bisserial 
library(BinNonNor)
# Para obter kurtosis
library(PerformanceAnalytics)

skew = matrix(NA,nrow = length(ordinaldf), ncol = 1)
kurt = matrix(NA,nrow = length(ordinaldf), ncol = 1)
for (i in 1:length(ordinaldf)){
  skew[i,] = lapply(ordinaldf[i], skewness)[[1]]
  kurt[i,] = lapply(ordinaldf[i], kurtosis)[[1]]
}


fleishman.coef(length(ordinaldf), skewness.vec = skew, kurtosis.vec = kurt)
