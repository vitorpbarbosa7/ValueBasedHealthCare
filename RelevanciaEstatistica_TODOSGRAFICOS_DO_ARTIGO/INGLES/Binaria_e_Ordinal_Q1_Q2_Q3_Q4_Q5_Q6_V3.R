rm(list=ls())
# setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
# setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/RelevanciaEstatistica')

library(tidyverse)
library(GGally)
library(lattice)

# Objetivo deste código é conseguir visualizar correlações entre as variáveis cont??nuas, e entre as variáveis 
# cont??nuas e variáveis binárias para as variáveis do questionário 1 e 2

# Juntar as bases de de binarios e ordinarios ------------------------------------------------
#Todas binárias até o momento
Bin_Gut_BPI_RMDQ_DN = read.csv('indata/Bin_Got_BPI_RMDQ_DN.csv')

#Ordinais até SF
Ord_Gut_BPI_Oswestry_RMDQ_SF = read.csv('indata/Ordinal_Gut_BPI_Oswestry_RMDQ_SF.csv', sep = ',')
Ord_Gut_BPI_Oswestry_RMDQ_SF[1] = NULL

#Binárias e ordinais até o momento
Binaria_Ordinal_Q1_Q2_Q3_Q4_Q5_Q6 = data.frame(cbind(Bin_Gut_BPI_RMDQ_DN,Ord_Gut_BPI_Oswestry_RMDQ_SF))

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

oldDN4 = c("DN4_1a_v1","DN4_1b_v1","DN4_1c_v1","DN4_2a_v1","DN4_2b_v1","DN4_2c_v1","DN4_2d_v1","DN4_3a_v1","DN4_3b_v1","DN4_4_v1")
newDN4 = c("DN4 - Queimação","DN4 - Sensação de frio dolorosa","DN4 - Choque elétrico","DN4 - Formigamento",
           "DN4 - Alfinetada e agulhada","DN4 - Adormecimento","DN4 - Coceira",
           "DH4 - Hipoestesia ao toque","DN4 - Hipoestesia a picada de agulha",
           "DN4 - Escovação")

oldnames = c("Dor_costas","Analg.e9.sicos","Casado_Amasiado","D30","Dor_pernas","Cirurgia_coluna")
newnames = c("Dor nas costas","Analgésicos","Casado/Amasiaado","Lombalgia","Dor nas pernas","Cirurgia na coluna")

oldord = c("Tempo_dor_costas","BPI_6_v1","Se_SP_haquantosanos",
           "QtCirColuna","Tempo_dor_pernas","Dor_pernas_VAS",
           
           "BPI_9a_v1","BPI_9b_v1","BPI_9c_v1","BPI_9d_v1","BPI_9e_v1","BPI_9f_v1","BPI_9g_v1")
neword = c("Tempo com a dor nas costas","Intensidade da dor no momento","Há quanto tempo em São Paulo se de outro local",
           "Quantidade de cirurgias na coluna","Tempo com dor que irradia p/ as pernas",
           "Intensidade da dor nas pernas",
           
           "BPI - Atividde geral","BPI - Humor","BPI - Habilidade de caminhar","BPI - Trabalho",
           "BPI - Relacionamento com outras pessoas","BPI - Sono","BPI - Habilidade para apreciar a vida")

#Substituir
for (i in 1:length(oldDN4)){
  pbmatrix$Binaria = sub(oldDN4[i],newDN4[i],pbmatrix$Binaria)
}

for (i in 1:length(newnames)){
  pbmatrix$Binaria = sub(oldnames[i],newnames[i],pbmatrix$Binaria)
}

for (i in 1:length(neword)){
  pbmatrix$Ordinal = sub(oldord[i],neword[i],pbmatrix$Ordinal)
}  

write.csv(pbmatrix, 'outputdata/biserial_nopvaluefilter.csv')

# P-Value Filter ----------------------------------------------------------
data = read.csv('outputdata/biserial_nopvaluefilter.csv')
data[1] = NULL

data_pvalue = data[data$pvalue < 0.05, ]
data_pvalue = subset(data_pvalue, !is.na(data_pvalue$Binaria))

write.csv(data_pvalue, file = 'outputdata/biseral_pvaluefilter.csv', row.names = FALSE)


# Mais filtros ------------------------------------------------------------
data_pvalue = data_pvalue[data_pvalue$rpb < 0.9999, ]
data_pvalue = data_pvalue[data_pvalue$rpb < -0.5 | data_pvalue$rpb > 0.5, ]

write.csv(data_pvalue, file = 'outputdata/biserial_pvaluefilter_03.csv', row.names = FALSE)

# Selecionando apenas algumas das binarias --------------------------------
library(tidyverse)
data_ = data_pvalue %>% filter(
  Binaria %in% c("Dor nas costas", "Dor nas pernas", "Lombalgia","Cirurgia na coluna","Desempregado")
)

#Remover a tanto tempo em são paulo 
data_ = data_[data_$Ordinal!= 'Há quanto tempo em São Paulo se de outro local',]

write.csv(data_, file = 'outputdata/biserial_pvalue_DoresPrincipais.csv', row.names = FALSE, fileEncoding = 'UTF-8')

#Selecionando as binarias para dor neuropatica 

# Selecionando apenas algumas das binarias para o DN--------------------------------
library(tidyverse)
data_ = data_pvalue %>% filter(
  Binaria %in% c("DN4 - Alfinetada e agulhada", 
                 "DN4 - Coceira",
                 "DH4 - Hipoestesia ao toque",
                 "DN4 - Hipoestesia a picada de agulha",
                 "DN4 - Escovação")
)

data_ = data_[data_$rpb > 0.60 | data_$rpb < -0.60, ]
data_ = data_[data_$Bin_1 > 30,]

write.csv(data_, file = 'outputdata/biserial_DN_pvaluefilter_03.csv', row.names = FALSE, fileEncoding = 'UTF-8')

