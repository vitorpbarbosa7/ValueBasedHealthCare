setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao1')

library(tidyverse)
library(GGally)
library(lattice)

# Objetivo deste código é conseguir visualizar correlações entre as variáveis contínuas, e entre as variáveis 
# contínuas e variáveis binárias

data = read.csv('data.csv', encoding = 'UTF-8', sep = ',')
names(data)[1] = "ID"

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

write.csv(dfbind,'binaria_e_ordinal.csv')
library(reshape2)

meltdata = melt(data = dfbind, id.vars = names(bindata), measure.vars = names(ordinaldf))
names(meltdata)

#Dor nas pernas
ggplot(data = meltdata, aes(Dor_pernas,value)) + 
  geom_boxplot(aes(fill = Desempregado)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") + 
  facet_wrap(.~variable, scales = "free")

#Dor nas costas
g = ggplot(data = meltdata, aes(Dor_costas,value)) + 
  geom_boxplot(aes(fill = Dor_costas)) + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") + 
  facet_wrap(.~variable, scales = "free")

tiff('outputdata/Dor_costas.tiff', units = "in", width = 20, height = 12, res = 300)
g
dev.off()

# Coeficiente de correlação de ponto bisserial ----------------------------

library(polycor)

polyserial(as.numeric(dfbind$IDADE),dfbind$Dor_costas)


# Matriz de correlação  ---------------------------------------------------

bindata2 = bindata[-c(1)]
rpb = matrix(NA, nrow = length(ordinaldf)*length(bindata2), ncol = 1)
xaxis = matrix(NA, nrow = length(ordinaldf)*length(bindata2), ncol = 1)
yaxis = matrix(NA, nrow = length(ordinaldf)*length(bindata2), ncol = 1)

cont = 1
for (j in 1:length(bindata2)){
  for (i in 1:length(ordinaldf)){
  
    rpb[cont,] = polyserial(as.numeric(ordinaldf[[i]]), bindata2[[j]])
    xaxis[cont,] = names(ordinaldf[i])
    yaxis[cont,] = names(bindata2[j])  
    cont = cont + 1
  }
}

#Aqui finalmente é a matriz de correlação
pbmatrix = data.frame(cbind(yaxis, xaxis, rpb))
names(pbmatrix) = c("Binaria","Ordinal","rpb")

write.csv(pbmatrix, 'outputdata/biseralcorrelation.csv')




