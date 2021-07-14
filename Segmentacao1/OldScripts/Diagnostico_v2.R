setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Segmentacao1')

#Inicialmente diagnostico da primeira porção das 5 partes do questionário

library(tidyverse)

data = read.csv('bindata7.csv', encoding = 'UTF-8')
names(data)[1]= "ID"
data[1] = NULL #index do python

# OneHotEncoder e renomear ------------------------------------------------
datacorr = data
datacorr[1] = NULL #Remover ID

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

costas = data.frame(cbind(names(phimatrix),phimatrix$Dor_costas))
names(costas)[1:2] = c('fatores','costas')





# Tabela Cruzada ----------------------------------------------------------

library(gmodels)

freq22 = matrix(NA, nrow = length(datacorr), ncol = 1)
#Matrix nome das variavei
denominador = matrix(NA, nrow = length(datacorr), ncol = 1)
numerador = matrix(NA, nrow = length(datacorr), ncol = 1)
qte_denom = matrix(NA, nrow = length(datacorr), ncol = 2)
i = 1
 for (i in 1:length(datacorr)){

  cross =   CrossTable(datacorr[[1]],datacorr[[i]], format = "SAS")
  
  freq22[i,] = cross$prop.row[2,2]
  qte_denom[i,c(1,2)] = c(cross$t[2,1] + cross$t[2,2],cross$t[1,1] + cross$t[1,2])
  
  denominador[i,] = names(datacorr[1])
  numerador[i,] = names(datacorr[i])
  
 }

crosstable = data.frame(cbind(denominador, numerador, freq22, qte_denom))
names(crosstable) = c("demonimador", "numerador", "Frequência","Qtd_1_Denom", "Qtd_0_Demon")

a = CrossTable(datacorr$Fuma,datacorr$Livrar_costas, format = "SAS")




# Inspecção resultados ----------------------------------------------------
dor_ = data.frame(cbind(data$Dor_costas,data$Livrar_costas))
names(dor_) = c('Dor_costas','Livrar_costas')





# Correlacão Tetracórica --------------------------------------------------
a = tetrachoric(datacorr, correct = FALSE)

tetramatrix = data.frame(tetrachoric(datacorr, correct = FALSE)$rho)

#Analisar dor nas costas
costas_ = data.frame(cbind(names(tetramatrix),tetramatrix$Dor_costas))
names(costas_)[1:2] = c('fatores','costas')

