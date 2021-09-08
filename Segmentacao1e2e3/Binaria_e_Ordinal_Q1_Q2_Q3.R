rm(list=ls())
# setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
# setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao1e2e3')

library(tidyverse)
library(GGally)
library(lattice)
library(corrplot)

# Objetivo deste código é conseguir visualizar correlações entre as variáveis contínuas, e entre as variáveis 
# contínuas e variáveis binárias para as variáveis do questionário 1 e 2


# Juntar as bases de 1 e 2 ------------------------------------------------
data1 = read.csv('indata/Binaria_Ordinal_Q1_Q2.csv', encoding = 'UTF-8', sep = ',')
data1[1] = NULL

data2 = read.csv('indata/Ordinal_Q3.csv', encoding = 'UTF-8', sep = ',')
names(data2)[1] = "Oswestry1_v1"

Binaria_Ordinal_Q1_Q2_Q3 = cbind(data1,data2)

write.csv(Binaria_Ordinal_Q1_Q2_Q3, 'outputdata/Binaria_Ordinal_Q1_Q2_Q3.csv')

# Iniciar a análise com essas variáveis binárias e ordinais ---------------
data = Binaria_Ordinal_Q1_Q2_Q3

# Subset com as variáveis ordinais ----------------------------------------
ordinallist = names(data[c(68:100)])

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

#Dor na lombar
ggplot(data = meltdata, aes(D30,value)) + 
  geom_boxplot(aes(fill = D30)) +
  geom_jitter(size = 0.5) +  
  facet_wrap(.~variable, scales = "free")

# Coeficiente de correlação de ponto bisserial ----------------------------

library(polycor)

polyserial(as.numeric(dfbind$Somatoria_Owestry),dfbind$D30)


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

write.csv(pbmatrix, 'outputdata/biseralcorrelation_Q1_Q2_Q3.csv')

#CorrelationMatrix
bindata


corrmatrix = matrix(NA, nrow = length(bindata), ncol = length(ordinaldf))
#Criar matrix 
for (j in 1:length(bindata)){
  for (i in 1:length(ordinaldf)){
    
    #Preencher a matriz
    corrmatrix[j,i] = polyserial(as.numeric(ordinaldf[[i]]), bindata[[j]])
  }
}

#Para obter a matrix de correlação
corrdf = data.frame(corrmatrix)

rownames(corrdf) = c(names(bindata))
colnames(corrdf) = names(ordinaldf)

tiff('outputdata/Binaria_Ordinal_CorrelationMatrix.tiff', units = 'in', width = 12, height = 20, res = 600)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black',
         title = "Matrix de correlação de ponto bisserial (correlograma) entre as variáveis binárias (linhas) 
         e variáveis ordinais (colunas) para os Questionários Gotemburgo, BPI e Oswestry",
         mar=c(0,0,3,0))
dev.off()


tiff('outputdata/Binaria_Ordinal_CorrelationMatrix_Number.tiff', units = 'in', width = 20, height = 30, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black',
         title = "Matrix de correlação de ponto bisserial (correlograma) entre as variáveis binárias (linhas) 
         e variáveis ordinais (colunas) para os Questionários Gotemburgo, BPI e Oswestry",
         mar=c(0,0,3,0), 
         method = 'number')
dev.off()





