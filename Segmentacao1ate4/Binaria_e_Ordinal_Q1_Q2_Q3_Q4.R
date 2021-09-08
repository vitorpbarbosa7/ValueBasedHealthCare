rm(list=ls())
# setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
# setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao1ate4')

library(tidyverse)
library(GGally)
library(lattice)
library(foreign)
library(corrplot)
# Objetivo deste código é conseguir visualizar correlações entre as variáveis contínuas, e entre as variáveis 
# contínuas e variáveis binárias para as variáveis do questionário 1 e 2

# Juntar as bases de de binarios e ordinarios ------------------------------------------------
#Todas binárias até o momento
Bin_Gut_BPI_RMDQ = read.csv('outputdata/Bin_Got_BPI_RMDQ.csv')

#Ordinais até 3
Ord_Gut_BPI_Oswestry = read.csv('indata/Ordinal_Q1_Q2_Q3.csv', sep = ';')
listnames = c(names(Ord_Gut_BPI_Oswestry))

#Nova ordinária do RMDQ (a pontuação final do RMDQ)
dataRMDQ =  read.spss('indata/Banco.sav', to.data.frame = T)

# POR QUE HÁ LINHAS NO RMDQ QUE TODAS AS RESPOSTAS ESTÃO COM VALOR ZERO?!!!
# COM CERTEZA FOI PORQUE AS PESSOAS NÃO RESPONDERAM, ENTÃO DEVERIAM ESTAR VAZIAS NAN, NULAS
# E NÃO COM VALOR ZERO

dataRMDQ = dataRMDQ[,c(99:122,127)]

#Substituir 1 (não) por 0 e 2 (sim) por 1
dataRMDQ = data.frame(lapply(dataRMDQ, function(x) {gsub("N�o",0,x)}))
dataRMDQ = data.frame(lapply(dataRMDQ, function(x) {gsub("Sim",1,x)}))

#Converter para numérico
dataRMDQ <- mutate_all(dataRMDQ, function(x) as.numeric(as.character(x)))

dfsum = rowSums(dataRMDQ[c(1:24)])

#Se todos os valores na linha estão 0, provavelmente era nulo 
dumNA = function(dfin){
  df = data.frame(dfin)
  dfsum = rowSums(df[c(1:24)])
  for (i in 1:length(df)){
    if (dfsum[i] == 0) {
      df[i,] = NA
    }
  }
  return(df)
}

dataRMDQ = dumNA(dataRMDQ)

Ord_RMDQ = dataRMDQ[,c(25)]

Ord_Got_BPI_Oswestry_RMDQ = data.frame(cbind(Ord_Gut_BPI_Oswestry,Ord_RMDQ))

write.csv(Ord_Got_BPI_Oswestry_RMDQ, 'outputdata/Ordinal_Q1_Q2_Q3_Q4.csv')

Binaria_Ordinal_Q1_Q2_Q3_Q4 = data.frame(cbind(Bin_Gut_BPI_RMDQ,Ord_Got_BPI_Oswestry_RMDQ))

write.csv(Binaria_Ordinal_Q1_Q2_Q3_Q4, 'outputdata/Binaria_Ordinal_Q1_Q2_Q3_Q4.csv')

# Subset com as variáveis ordinais ----------------------------------------
ordinallist = names(Ord_Got_BPI_Oswestry_RMDQ)

ordinaldf = Ord_Got_BPI_Oswestry_RMDQ

#pairs(ordinaldf)
#splom(ordinaldf)

# Variáveis Binárias ----------------------------------------
bindata = Bin_Gut_BPI_RMDQ #(Oswestry não tem binária)

#Não há relação de ordinalidade, logo, todos os dados do dataset bindata são
#Se não adicionarmos [], R não mantém a estrutura de fatores
bindata[] = lapply(bindata[], factor)

dfbind = cbind(ordinaldf,bindata)

#write.csv(dfbind,'binaria_e_ordinal.csv')
library(reshape2)

meltdata = melt(data = dfbind, id.vars = names(bindata), measure.vars = names(ordinaldf))
names(meltdata)

#Dor na lombar
ggplot(data = meltdata, aes(Masc,value)) + 
  geom_boxplot(aes(fill = Masc)) +
  geom_jitter(size = 0.5) +  
  facet_wrap(.~variable, scales = "free")

# Coeficiente de correlação de ponto bisserial ----------------------------

library(polycor)

polyserial(as.numeric(dfbind$Ord_RMDQ),dfbind$D35)


# Matriz de correlação  ---------------------------------------------------
#Primeira coluna é indíce
bindata[1] = NULL
ordinaldf[1] = NULL
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

write.csv(pbmatrix, 'outputdata/biseralcorrelation_Q1_Q2_Q3_Q4.csv')

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

tiff('outputdata/Binaria_Ordinal_CorrelationMatrix_Q1234.tiff', units = 'in', width = 12, height = 20, res = 600)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black',
         title = "Matrix de correlação de ponto bisserial (correlograma) entre as variáveis binárias (linhas) 
         e variáveis ordinais (colunas) para os Questionários Gotemburgo, BPI, Oswestry e RMDQ",
         mar=c(0,0,3,0))
dev.off()


tiff('outputdata/Binaria_Ordinal_CorrelationMatrix_Number_q1234.tiff', units = 'in', width = 25, height = 35, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black',
         title = "Matrix de correlação de ponto bisserial (correlograma) entre as variáveis binárias (linhas) 
         e variáveis ordinais (colunas) para os Questionários Gotemburgo, BPI e Oswestry e RMDQ",
         mar=c(0,0,3,0), 
         method = 'number')
dev.off()





