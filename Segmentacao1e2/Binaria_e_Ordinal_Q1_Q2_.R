rm(list=ls())
# setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
# setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao1e2')

library(tidyverse)
library(GGally)
library(lattice)

# Objetivo deste código é conseguir visualizar correlações entre as variáveis contínuas, e entre as variáveis 
# contínuas e variáveis binárias para as variáveis do questionário 1 e 2


# Juntar as bases de 1 e 2 ------------------------------------------------
data1 = read.csv('indata/binaria_Q1_Q2_ordinal_Q1.csv', encoding = 'UTF-8', sep = ',')
data1[1] = NULL

data2 = read.csv('indata/BPI_3-6_8-9.csv', encoding = 'UTF-8', sep = ',')
names(data2)[1] = "BPI_3_v1"

#Converter para numÃ©rico
data2 <- mutate_all(data2, function(x) as.numeric(as.character(x)))

dfsum = as.data.frame(rowSums(data2[c(6:12)]))
names(dfsum)[1] = 'sum'

data2_9 = data2[c(6:12)]
data2_antes9 = data2[c(1:5)]

#Tratar a disgracinha do NA no sum:

dfsum[is.na(dfsum)]=1000

cont = 1
for (i in 1:nrow(dfsum)){
  cont = cont + 1 
  if (dfsum$sum[i] == 0){
    data2_9[i,] = NA
  }
}

data2 = cbind(data2_antes9,data2_9)

Binaria_Ordinal_Q1_Q2 = cbind(data1,data2)

write.csv(Binaria_Ordinal_Q1_Q2, 'outputdata/Binaria_Ordinal_Q1_Q2.csv')

# Iniciar a análise com essas variáveis binárias e ordinais ---------------
data = Binaria_Ordinal_Q1_Q2

# Subset com as variáveis ordinais ----------------------------------------
ordinallist = names(data[c(68:89)])


ordinaldf = subset(x = data, select = ordinallist)

#pairs(ordinaldf)
#splom(ordinaldf)

# Variáveis Binárias ----------------------------------------
bindata = data1[c(1:67)]

#Não há relação de ordinalidade, logo, todos os dados do dataset bindata são
#Se não adicionarmos [], R não mantém a estrutura de fatores
bindata[] = lapply(bindata[], factor)

dfbind = cbind(ordinaldf,bindata)

#write.csv(dfbind,'binaria_e_ordinal.csv')
library(reshape2)

meltdata = melt(data = dfbind, id.vars = names(bindata), measure.vars = names(ordinaldf))
names(meltdata)
dev.off()
#Dor na lombar
ggplot(data = meltdata, aes(D30,value)) + 
  geom_boxplot(aes(fill = D30)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") + 
  facet_wrap(.~variable, scales = "free")

#Dor na lombar
ggplot(data = meltdata, aes(D30,value)) + 
  geom_boxplot() +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") + 
  facet_wrap(.~variable, scales = "free")

# Coeficiente de correlação de ponto bisserial ----------------------------

library(polycor)

polyserial(as.numeric(dfbind$IDADE),dfbind$Dor_costas)


# Matriz de correlação  ---------------------------------------------------

rpb = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
xaxis = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
yaxis = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)

cont = 1
for (j in 1:length(bindata)){
  for (i in 1:length(ordinaldf)){
  
    rpb[cont,] = polyserial(as.numeric(ordinaldf[[i]]), bindata[[j]])
    xaxis[cont,] = names(ordinaldf[i])
    yaxis[cont,] = names(bindata[j])  
    cont = cont + 1
  }
}

#Aqui finalmente é a matriz de correlação
pbmatrix = data.frame(cbind(yaxis, xaxis, rpb))
names(pbmatrix) = c("Binaria","Ordinal","rpb")

write.csv(pbmatrix, 'outputdata/biseralcorrelation_Q1_Q2.csv')




