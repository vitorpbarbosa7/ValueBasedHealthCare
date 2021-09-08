setwd('C:/GD/DS/Lefort/PCA')

data = read.csv('C:/GD/DS/Lefort/PCA/indata/Binaria_Ordinal_Q123456_HALF_SEMMISSINGBPI.csv')
data[c(1,2)] = NULL

#Porcentagem de valores faltantes:
missing = sum(is.na(data))/(nrow(data)*ncol(data))*100

scaled_ord = scale(data[c(102:147)])

scaled_data = cbind(data[c(1:101)], scaled_ord)

#Aplicar o PCA
library(pcaMethods)

pca = bpca(as.matrix(scaled_data), nPcs = 10)

scores = pca@scores

loadings = pca@loadings

df_loadings = as.data.frame(pca@loadings)

Variaveis_Importantes = matrix(NA, nrow = length(df_loadings$V1), ncol=1)

for (i in 1:length(df_loadings$V1)) {  
  ordenado = loadings[order(abs(loadings[,i])),i]
  #dotchart(ordenado,
           #cex = 0.7, xlab = "loadings", main = "loadings")
  df_ordenado = as.data.frame(ordenado)
  df_ordenado$variaveis = rownames(df_ordenado)
  Variaveis_Importantes[i,] = c(df_ordenado$variaveis[length(df_ordenado$variaveis)])
  #Sys.sleep(5)
}

write.csv( Variaveis_Importantes, 'Variaveis_Principais.csv')

#Visualizar os dados em 2 dimensões
plot(loadings$V1, loadings$V2)

#Screeplot?
fviz_eig(pca)

plot(bpca(as.matrix(scaled_data)))

library(plsdepot)
nipals(as.matrix(data), comps = 2, scaled = TRUE)

pca_p = ppca(as.matrix(data), nPcs = 2)
