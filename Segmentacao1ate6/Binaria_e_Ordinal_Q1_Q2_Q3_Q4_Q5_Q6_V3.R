rm(list=ls())
# setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
# setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao1ate6')

library(tidyverse)
library(GGally)
library(lattice)

# Objetivo deste código é conseguir visualizar correlações entre as variáveis cont??nuas, e entre as variáveis 
# cont??nuas e variáveis binárias para as variáveis do questionário 1 e 2

# Juntar as bases de de binarios e ordinarios ------------------------------------------------
#Todas binárias até o momento
Bin_Gut_BPI_RMDQ_DN = read.csv('outputdata/Bin_Got_BPI_RMDQ_DN.csv')

#Ordinais até SF
Ord_Gut_BPI_Oswestry_RMDQ_SF = read.csv('indata/Ordinal_Gut_BPI_Oswestry_RMDQ_SF.csv', sep = ',')
Ord_Gut_BPI_Oswestry_RMDQ_SF[1] = NULL

#Binárias e ordinais até o momento
Binaria_Ordinal_Q1_Q2_Q3_Q4_Q5_Q6 = data.frame(cbind(Bin_Gut_BPI_RMDQ_DN,Ord_Gut_BPI_Oswestry_RMDQ_SF))

write.csv(Binaria_Ordinal_Q1_Q2_Q3_Q4_Q5_Q6, 'outputdata/Binaria_Ordinal_Q1_Q2_Q3_Q4_Q5_Q6.csv')

# Subset com as variáveis ordinais ----------------------------------------
ordinallist = names(Ord_Gut_BPI_Oswestry_RMDQ_SF)

ordinaldf = Ord_Gut_BPI_Oswestry_RMDQ_SF

# Variáveis Binárias ----------------------------------------
bindata = Bin_Gut_BPI_RMDQ_DN #(Oswestry e SF não tem binária)

#N?o há relação de ordinalidade, logo, todos os dados do dataset bindata são
#Se N?o adicionarmos [], R N?o mantém a estrutura de fatores
bindata[] = lapply(bindata[], factor)

#Primeira coluna é ind??ce, logo, podemos deletar
bindata[1] = NULL

dfbind = cbind(ordinaldf,bindata)

#write.csv(dfbind,'binaria_e_ordinal.csv')
library(reshape2)

meltdata = melt(data = dfbind, id.vars = names(bindata), measure.vars = names(ordinaldf))
names(meltdata)

write.csv(meltdata, 'outputdata/meltdataboxsplot.csv')

#Dor na lombar
ggplot(data = meltdata, aes(D30,value)) + 
  geom_boxplot(aes(fill = D30)) +
  geom_jitter(size = 0.5) +  
  facet_wrap(.~variable, scales = "free")

# Coeficiente de correlação de ponto bisserial ----------------------------

library(polycor)

var1 = as.numeric(dfbind$Ord_RMDQ)
var2 = as.numeric(dfbind$D35)
a = polyserial(var1,var2)

test = cor.test(var1, var2)

# Matriz de correlação  ---------------------------------------------------
rpb = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
xaxis = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
yaxis = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
cont_Bin_0 = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
cont_Bin_1 = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
missing = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
pvalue = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)

cont = 1
for (j in 1:length(bindata)){
  for (i in 1:length(ordinaldf)){
  
    var1 = ordinaldf[[i]]
    var2 = bindata[[j]]
    rpb[cont,] = polyserial(as.numeric(ordinaldf[[i]]), as.numeric(bindata[[j]]))
    pvalue[cont,] = cor.test(as.numeric(ordinaldf[[i]]),as.numeric(bindata[[j]]))$p.value
    xaxis[cont,] = names(ordinaldf[i])
    yaxis[cont,] = names(bindata[j])
    
    #Retornar contagem de valores zero e valores um da variável binária
    #Isto é importante para saber quantos dados temos na correlação
    #As vezes a correlação é grande só porque temos poucos dados
    aux = data.frame(bin = bindata[[j]], ord = ordinaldf[[i]])
    aux_ = aux[complete.cases(aux),]
    tbl = table(aux_[[1]]) #Contar apenas o número de zeros e ones do binário, que é a primeira coluna
    cont_Bin_0[cont,] = tbl[1] #Número de zeros
    cont_Bin_1[cont,] = tbl[2] #Número de ones
    
    #Ele retorna missing no boxplot se a pessoa respondeu a questão da ordinária, mas N?o respondeu a questão binária
    missingord = aux[!is.na(aux[[2]]),] #O ??ndice 2 se refere à variável ordinal
    missing[cont,] = sum(is.na(missingord[[1]])) #O ??ndice 1 se refere à variável binária, da qual deveremos contar os missing
    
    cont = cont + 1
  }
}

#Aqui finalmente é a matriz de correlação
pbmatrix = data.frame(cbind(yaxis, xaxis, rpb, pvalue, cont_Bin_0, cont_Bin_1, missing))
names(pbmatrix) = c("Binaria","Ordinal","rpb","pvalue","Bin_0","Bin_1","Missing")

write.csv(pbmatrix, 'outputdata/biseralcorrelation_Q1_Q2_Q3_Q4_Q5_Q6_bindata.csv')

#Checar se algumas variaveis binarias estao presentes:
#bypass para não precisar rodar o looping de novo já que o dataset já está gravado em csv
pbmatrix = read.csv('outputdata/biseralcorrelation_Q1_Q2_Q3_Q4_Q5_Q6_bindata.csv')
data = pbmatrix

data$Binaria = as.factor(data$Binaria)

levels(data$Binaria)

#Matrix de correlação 
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

library(corrplot)

tiff('outputdata/imgs/Binaria_Ordinal_CorrelationMatrix_Q123456.tiff', units = 'in', width = 18, height = 30, res = 450)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black',
         title = "Matrix de correlação de ponto bisserial (correlograma) entre as variáveis binárias (linhas) 
         e variáveis ordinais (colunas) para os Questionários Gotemburgo, BPI, Oswestry, RMDQ, SF e DN",
         mar=c(0,0,3,0))
dev.off()


tiff('outputdata/imgs/Binaria_Ordinal_CorrelationMatrix_Number_q123456.tiff', units = 'in', width = 25, height = 35, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black',
         title = "Matrix de correlação de ponto bisserial (correlograma) entre as variáveis binárias (linhas) 
         e variáveis ordinais (colunas) para os Questionários Gotemburgo, BPI e Oswestry, RMDQ, SF e DN",
         mar=c(0,0,3,0), 
         method = 'number')
dev.off()


# Mapa dos valores binários: ------------------------------------------------------

cont_Bin_0 = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
cont_Bin_1 = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)
missing = matrix(NA, nrow = length(ordinaldf)*length(bindata), ncol = 1)

#Matriz que irá receber os valores de missing
bincontmatrix = matrix(NA, nrow = length(bindata), ncol = length(ordinaldf))

cont = 1
for (j in 1:length(bindata)){
  for (i in 1:length(ordinaldf)){
    
    #Retornar contagem de valores zero e valores um da variável binária
    #Isto é importante para saber quantos dados temos na correlação
    #As vezes a correlação é grande só porque temos poucos dados
    aux = data.frame(bin = bindata[[j]], ord = ordinaldf[[i]])
    aux_ = aux[complete.cases(aux),]
    tbl = table(aux_[[1]]) #Contar apenas o número de zeros e ones do binário, que é a primeira coluna
    cont_Bin_0 = tbl[1] #Número de zeros
    cont_Bin_1 = tbl[2] #Número de ones
    
    # #Ele retorna missing no boxplot se a pessoa respondeu a questão da ordinária, mas N?o respondeu a questão binária
    # missingord = aux[!is.na(aux[[2]]),] #O ??ndice 2 se refere à variável ordinal
    # missing[cont,] = sum(is.na(missingord[[1]])) #O ??ndice 1 se refere à variável binária, da qual deveremos contar os missing
    # 
    
    values = paste(cont_Bin_0," | ",cont_Bin_1, sep = "")
    #Preencher a matriz
    
    bincontmatrix[j,i] = values
    
    cont = cont + 1
    
    
  }
}
corrdf_bin = data.frame(bincontmatrix)

rownames(corrdf_bin) = c(names(bindata))
colnames(corrdf_bin) = names(ordinaldf)

#NA ESQUERDA FICAM OS ZEROS E NA DIREITA OS ONES
#Save the dataframe as image 
library(gridExtra)
tiff("outputdata/imgs/Binary_Biserial_Cont_0_1.png", units = "in", width=80 ,height=35 ,res = 150)
grid.table(corrdf_bin)
dev.off()




