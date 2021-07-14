rm(list = ls())
setwd('C:/GD/DS/Lefort/Segmentacao1ate4')

#Inicialmente diagnostico da primeira porção das 5 partes do questionário

library(tidyverse)
library(foreign)

data_Got_BPI = read.csv('indata/Binarias_Q1_Q2.csv')

dataRMDQ =  read.spss('indata/Banco.sav', to.data.frame = T)

# POR QUE HÁ LINHAS NO RMDQ QUE TODAS AS RESPOSTAS ESTÃO COM VALOR ZERO?!!!
# COM CERTEZA FOI PORQUE AS PESSOAS NÃO RESPONDERAM, ENTÃO DEVERIAM ESTAR VAZIAS NAN, NULAS
# E NÃO COM VALOR ZERO

dataRMDQ = dataRMDQ[,c(99:122)]

#Substituir 1 (não) por 0 e 2 (sim) por 1
dataRMDQ = data.frame(lapply(dataRMDQ, function(x) {gsub("Não",0,x)}))
dataRMDQ = data.frame(lapply(dataRMDQ, function(x) {gsub("Sim",1,x)}))

#Converter para numérico
dataRMDQ <- mutate_all(dataRMDQ, function(x) as.numeric(as.character(x)))

dfsum = rowSums(dataRMDQ)

#Se todos os valores na linha estão 0, provavelmente era nulo 
dumNA = function(dfin){
  df = data.frame(dfin)
  dfsum = rowSums(df)
  for (i in 1:length(df)){
    if (dfsum[i] == 0) {
      df[i,] = NA
    }
  }
  return(df)
}

dataRMDQ = dumNA(dataRMDQ)

Bin_Got_BPI_RMDQ = data.frame(cbind(data_Got_BPI,dataRMDQ))
Bin_Got_BPI_RMDQ[1] = NULL

write.csv(Bin_Got_BPI_RMDQ, file = 'outputdata/Bin_Got_BPI_RMDQ.csv')

datacorr = Bin_Got_BPI_RMDQ


# D30, Dor nas costas e Dor nas pernas com RMDQ ---------------------------
# Tabela de contingência --------------------------------------------------
library(gmodels)

datacorr = datacorr[c(9,10,20,21,41,68:91)]

freq22 = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
#Matrix nome das variavei
denominador = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
numerador = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
qte_denom = matrix(NA, nrow = length(datacorr)^2, ncol = 2)

nums = matrix(NA, nrow = length(datacorr)^2, ncol = 4)

cont = 1
for (i in 1:length(datacorr)){
  for (j in 1: length(datacorr)){
    
    cross = CrossTable(datacorr[[j]],datacorr[[i]], format = "SAS");
    
    freq22[cont,] = cross$prop.row[2,2]
    qte_denom[cont,c(1,2)] = c(cross$t[2,1] + cross$t[2,2],cross$t[1,1] + cross$t[1,2])
    
    denominador[cont,] = names(datacorr[j])
    numerador[cont,] = names(datacorr[i])
    
    nums[cont,c(1:4)] = c(cross$t[1,1], cross$t[1,2], cross$t[2,1], cross$t[2,2])
    cont = cont + 1
  }
}

crosstable = data.frame(cbind(denominador, numerador, freq22, qte_denom, nums))
names(crosstable) = c("demonimador", "numerador", "Frequência","Qtd_1_Denom", "Qtd_0_Demon",
                      "D0N0",'D0N1','D1N0','D1N1')

write.csv(x = crosstable, file = 'outputdata/Crosstable_D30_DorCostas_DorPenas_RMDQ.csv')


# DataViz com correlograma da tabela cruzada com apenas RMDQ e Dor --------
corrmatrix = matrix(NA, nrow = length(datacorr), ncol = length(datacorr))
#Criar matrix 
for (i in 1:length(datacorr)){ #Preencher as linhas
  for (j in 1:length(datacorr)){ #Preencher as colunas
    
    # A tabela cruzada
    cross = CrossTable(datacorr[[i]],datacorr[[j]], format = "SAS");
    
    #A frequência que desejamos coletar
    freq22 = cross$prop.row[2,2]
    
    #Preencher a matriz
    corrmatrix[i,j] = freq22
  }
}

#Para obter a matrix de correlação
corrdf = data.frame(corrmatrix)

rownames(corrdf) = c(names(datacorr))
colnames(corrdf) = names(datacorr)

#Save Imagens ****
# Number 
library(corrplot)
tiff('outputdata/Pernas_Costas_D30_RMDQ_Circle.tiff', units = "in", width = 30, height = 30, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'circle',
         title = 'Frequência da tabela de contingência para os pares de variáveis binárias',
         mar = c(0,0,3,0)) #Configuração da margem 
dev.off()


tiff('outputdata/Pernas_Costas_D30_RMDQ_Number.tiff', units = "in", width = 30, height = 30, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'number',
         title = 'Frequência da tabela de contingência para os pares de variáveis binárias',
         mar = c(0,0,3,0))
dev.off()

# Tabela de contingência com absolutamente todas binárias --------------------------------------------------
datacorr = Bin_Got_BPI_RMDQ
library(gmodels)
freq22 = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
#Matrix nome das variavei
denominador = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
numerador = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
qte_denom = matrix(NA, nrow = length(datacorr)^2, ncol = 2)

nums = matrix(NA, nrow = length(datacorr)^2, ncol = 4)

f = matrix(NA, nrow = length(datacorr)^2, ncol = 4)

cont = 1
for (i in 1:length(datacorr)){
  for (j in 1: length(datacorr)){
    
    cross = CrossTable(datacorr[[j]],datacorr[[i]], format = "SAS");
    
    freq22[cont,] = cross$prop.row[2,2]
    qte_denom[cont,c(1,2)] = c(cross$t[1,1] + cross$t[1,2],cross$t[2,1] + cross$t[2,2])
    
    denominador[cont,] = names(datacorr[j])
    numerador[cont,] = names(datacorr[i])
    
    nums[cont,c(1:4)] = c(cross$t[1,1], cross$t[1,2], cross$t[2,1], cross$t[2,2])
    
    cont = cont + 1
    
  }
}

crosstable = data.frame(cbind(denominador, numerador, freq22, qte_denom, nums))
names(crosstable) = c("demonimador", "numerador", "Frequência","Qtd_0_Denom", "Qtd_1_Denom",
                      "D0N0",'D0N1','D1N0','D1N1')

write.csv(x = crosstable, file = 'outputdata/Crosstable_Got_BPI_RMDQ.csv')




# DataViz com correlograma da tabela cruzada com apenas RMDQ e Dor --------
corrmatrix = matrix(NA, nrow = length(datacorr), ncol = length(datacorr))
#Criar matrix 
for (i in 1:length(datacorr)){ #Preencher as linhas
  for (j in 1:length(datacorr)){ #Preencher as colunas
    
    # A tabela cruzada
    cross = CrossTable(datacorr[[i]],datacorr[[j]], format = "SAS");
    
    #A frequência que desejamos coletar
    freq22 = cross$prop.row[2,2]
    
    #Preencher a matriz
    corrmatrix[i,j] = freq22
  }
}

#Para obter a matrix de correlação
corrdf = data.frame(corrmatrix)

rownames(corrdf) = c(names(datacorr))
colnames(corrdf) = names(datacorr)


library(corrplot)
tiff('outputdata/Got_BPI_RMDQ_Circle.tiff', units = "in", width = 30, height = 30, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'circle',
         title = 'Frequência da tabela de contingência para os pares de variáveis binárias',
         mar = c(0,0,3,0)) #Configuração da margem 
dev.off()


tiff('outputdata/Got_BPI_RMDQ_Number.tiff', units = "in", width = 40, height = 40, res = 200)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'number',
         title = 'Frequência da tabela de contingência para os pares de variáveis binárias',
         mar = c(0,0,3,0))
dev.off()



# Visualizar todas tabelas de contingência de uma vez só------------------------------------------------

datacorr = Bin_Got_BPI_RMDQ
library(gmodels)

#Essa será a principal que 
TableMatrix = matrix(NA, nrow = length(datacorr), ncol = length(datacorr))


cont = 1
for (i in 1:length(datacorr)){
  for (j in 1: length(datacorr)){
    
    cross = CrossTable(datacorr[[j]],datacorr[[i]], format = "SAS");
    
    #Exemplo licenca médica
    #Frequência: (15/16)|Total:119 pontos|Número de zeros: 103
    values = paste(cross$t[2,2],"/",sum(cross$t[2,]),"|T:",sum(cross$t),"|0:",sum(cross$t[1,]), sep = "")
    
    TableMatrix[i,j] = values
    
    cont = cont + 1
  }
}

#Para obter a matrix de correlação
corrdf_Contingencia = data.frame(TableMatrix)

rownames(corrdf_Contingencia) = c(names(datacorr))
colnames(corrdf_Contingencia) = names(datacorr)

library(gridExtra)
tiff("outputdata/img/Binary_Contingencia_0_1.png", units = "in", width=150 ,height=30 ,res = 100)
grid.table(corrdf_Contingencia)
dev.off()
