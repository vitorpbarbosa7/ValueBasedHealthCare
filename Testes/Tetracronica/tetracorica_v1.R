setwd('C:/Users/vitor/OneDrive - IPT/Lefort/Testes/Tetracronica')

library(tidyverse)

data = read.csv('data.csv', encoding = 'UTF-8', sep = ';')
names(data)[1] = "A"

# Carregar biblioteca para aplicar a correlação tetracronica --------------

library(psych)

corr = tetrachoric(data)

corrmatrix = data.frame(corr$rho)



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
phimatrix = data.frame(phimat(data))


# Comparacao com o cálculo realizado no Excel -----------------------------

comparacao = read.csv('CalculoTeoricoComparacao.csv', encoding = 'UTF-8', sep = ',')
names(comparacao)[1] = "A"

compmatrix = data.frame(tetrachoric(comparacao)$rho)
