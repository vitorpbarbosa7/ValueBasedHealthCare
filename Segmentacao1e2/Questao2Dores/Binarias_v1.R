setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/Users/vpb/OneDrive - IPT/Lefort/Segmentacao1')
setwd('C:/GD/DS/Lefort/Segmentacao2')

#Inicialmente diagnostico da primeira porção das 5 partes do questionário

library(tidyverse)

dataQ1 = read.csv('Q1bin.csv', encoding = 'UTF-8')
names(data)[1]= "ID"
dataQ1[1] = NULL #index do python
dataQ1[1] = NULL

dataQ2_dores = read.csv('dorescorpo.csv', encoding = 'UTF-8')
dataQ2_dores[1] = NULL

data = cbind(dataQ1,dataQ2_dores)
datacorr = data

# Biblioteca que permite executar a correlação tetracórica ou o coeficiente de phi----------------
library(psych)

# Utilizar o coeficiente de phi -------------------------------------------
phimat<-function(x) {
  xcol<-dim(x)[2]
  newx<-matrix(NA,nrow=xcol,ncol=xcol)
  for(i in 1:xcol) {
    for(j in 1:xcol) newx[i,j]<-phi(table(x[,i],x[,j]))
  }
  rownames(newx)<-colnames(newx)<-colnames(x)
  return(newx)
}
phimatrix = data.frame(phimat(datacorr))

#Dor na Lombar
costas = data.frame(cbind(names(phimatrix),phimatrix$X30))
names(costas)[1:2] = c('fatores','Dor_Lombar_30')
write.csv(x = costas, file = 'outputdata/Lombar_Costas.csv')

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

write.csv(x = crosstable, file = 'outputdata/crosstable.csv')

#Para inspecionar
library(gmodels)
a = CrossTable(datacorr$Dor_pernas,datacorr$Livrar_costas, format = "SAS")

CrossTable(datacorr$Dor_pernas,datacorr$Livrar_perna, format = "SAS")




# Inspecção resultados ----------------------------------------------------
dor_ = data.frame(cbind(data$Dor_costas,data$Livrar_costas))
names(dor_) = c('Dor_costas','Livrar_costas')





# Correlacão Tetracórica --------------------------------------------------
a = tetrachoric(datacorr, correct = FALSE)

tetramatrix = data.frame(tetrachoric(datacorr, correct = FALSE)$rho)

#Analisar dor nas costas
costas_ = data.frame(cbind(names(tetramatrix),tetramatrix$Dor_costas))
names(costas_)[1:2] = c('fatores','costas')

