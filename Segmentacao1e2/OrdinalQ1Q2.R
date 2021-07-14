rm(list=ls())
setwd('C:/GD/DS/Lefort/Segmentacao1e2')
setwd('C:/GD_/USP/DS/Lefort/Segmentacao1e2')

data = read.csv('indata/OrdinalQ1Q2.csv', encoding = 'UTF-8', sep = ',')
names(data)[1] = "Se_SP_haquantosanos"


# Correlações de Pearson --------------------------------------------------
library(corrplot)

#Função para realizar o plot
corr_fun = function(corr, method){
  corrplot(corr,
           method = method,
           type = 'upper',
           tl.col = "black",
           tl.srt = 45)
}

cormatrix = cor(data, use = 'pairwise.complete.obs')
cormatrix
corr_fun(cormatrix, 'number')



tiff('outputdata/OrdinalQ1Q2.tiff', units = "in", width = 12, height = 14, res = 300)
corr_fun(cormatrix, 'circle')
dev.off()
