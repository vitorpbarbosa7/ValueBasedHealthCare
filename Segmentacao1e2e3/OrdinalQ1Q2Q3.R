rm(list=ls())
setwd('C:/GD/DS/Lefort/Segmentacao1e2e3')

data = read.csv('indata/OrdinalQ1Q2Q3.csv', encoding = 'UTF-8', sep = ',')
names(data)[1] = "Se_SP_haquantosanos"

plot(x = data$Dor_costas_VAS, data$BPI_3_v1)


# Correlações de Pearson --------------------------------------------------
library(corrplot)

#Função para realizar o plot
corr_fun = function(corr, method){
  corrplot(corr,
           method = method,
           type = 'upper',
           tl.col = "black",
           tl.srt = 45, 
           title = 'Matrix de correlação de coeficiente de Pearson entre variáveis ordinais
           dos questionários Gotemburgo, BPI e Oswestry', 
           mar = c(0,0,5,0))
}

cormatrix = cor(data,
                ,use = 'pairwise.complete.obs')

tiff('outputdata/OrdinalQ1Q2Q3_Number.tiff', units = "in", width = 18, height = 18, res = 450)
corr_fun(cormatrix, 'number')
dev.off()
