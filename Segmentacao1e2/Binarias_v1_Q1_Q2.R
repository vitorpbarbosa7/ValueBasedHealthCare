rm(list=ls())
# setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
# setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao1e2')

#Inicialmente diagnostico da primeira porção das 5 partes do questionário

library(tidyverse)

dataQ1 = read.csv('Q1bin.csv', encoding = 'UTF-8')
names(dataQ1)[1]= "ID"
dataQ1[1] = NULL #index do python
dataQ1[1] = NULL

dataQ2_dores = read.csv('dorescorpo.csv', encoding = 'UTF-8')
dataQ2_dores[1] = NULL

data = data.frame(cbind(dataQ1,dataQ2_dores))

names(data)[c(23:67)] = names(dataQ2_dores)
#write.csv(data, 'indata/binarias_Q1_e_Q2.csv')


datacorr = data

# Biblioteca que permite executar a correlação tetracórica ou o coeficiente de phi----------------
library(psych)

# # Utilizar o coeficiente de phi -------------------------------------------
# phimat<-function(x) {
#   xcol<-dim(x)[2]
#   newx<-matrix(NA,nrow=xcol,ncol=xcol)
#   for(i in 1:xcol) {
#     for(j in 1:xcol) newx[i,j]<-phi(table(x[,i],x[,j]))
#   }
#   rownames(newx)<-colnames(newx)<-colnames(x)
#   return(newx)
# }
# phimatrix = data.frame(phimat(datacorr))
# 
# #Dor na Lombar
# costas = data.frame(cbind(names(phimatrix),phimatrix$X30))
# names(costas)[1:2] = c('fatores','Dor_Lombar_30')
# write.csv(x = costas, file = 'outputdata/PHI_Lombar_Costas.csv')

# Tabela Cruzada ----------------------------------------------------------

library(gmodels)

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

write.csv(x = crosstable, file = 'outputdata/Crosstable_Q1_Q2.csv')

#Para inspecionar
library(gmodels)

a = CrossTable(datacorr$Dor_pernas,datacorr$Livrar_perna, format = "SAS")
a$prop.row[2,2]


# DataViz -----------------------------------------------------------------
rm(list=ls())

# setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
# setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao1e2')

#Inicialmente diagnostico da primeira porção das 5 partes do questionário

library(tidyverse)

dataQ1 = read.csv('Q1bin.csv', encoding = 'UTF-8')
names(dataQ1)[1]= "ID"
dataQ1[1] = NULL #index do python
dataQ1[1] = NULL

dataQ2_dores = read.csv('dorescorpo.csv', encoding = 'UTF-8')
dataQ2_dores[1] = NULL

data = data.frame(cbind(dataQ1,dataQ2_dores))

names(data)[c(23:67)] = names(dataQ2_dores)
#write.csv(data, 'indata/binarias_Q1_e_Q2.csv')


datacorr = data

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



# corrdf$Livrar_perda_sens = NULL


# Number -----------------------------------------------------------------
tiff('outputdata/Binarias_Q1_Q2_Number.tiff', units = "in", width = 30, height = 30, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'number')
dev.off()


tiff('outputdata/BPI_Corpo_Binarias_Number.tiff', units = "in", width = 20, height = 20, res = 300)
corrplot(as.matrix(corrdf[-c(1:22),-c(1:22)]),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'number')
dev.off()


# Square -----------------------------------------------------------------
tiff('outputdata/Binarias_Q1_Q2_Circle.tiff', units = "in", width = 30, height = 30, res = 300)
corrplot(as.matrix(corrdf),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'circle')
dev.off()


tiff('outputdata/BPI_Corpo_Binarias_Circle.tiff', units = "in", width = 20, height = 20, res = 300)
corrplot(as.matrix(corrdf[-c(1:22),-c(1:22)]),
         tl.srt = 45,
         tl.col = 'black', 
         type = 'upper', 
         method = 'circle')
dev.off()








