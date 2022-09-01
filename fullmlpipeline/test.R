setwd('C:/GD/DS/Lefort/ML')

data = read.csv('data/test.csv')
names(data)[1] = 'A'

library(randomForest)

data_imputed = rfImpute(A ~ ., data = data, iter = 6)

model = randomForest(D ~ ., data = data, proximity = T)
