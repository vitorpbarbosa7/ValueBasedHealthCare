setwd('C:\\GD\\DS\\Lefort\\ML')

#Fancy graphs
library(ggplot2)

#Improve some settings from ggplot
library(cowplot)

#For the machine learning algorithm 
library(randomForest)

url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data'

data = read.csv(url, header = F)

colnames(data) = c('age','sex','cp','trestbps','chol',
                   'fbs','restecg','thalach','exang',
                   'oldpeak','slope','ca','thal','hd')

str(data)


# #Cleaning to do : -------------------------------------------------------
data[data == '?'] = NA

data[data$sex == 0,]$sex = 'F'
data[data$sex == 1,]$sex = 'M'

data$sex = as.factor(data$sex)

#Outras colunas convertidas para fatores
data$cp = as.factor(data$cp)
data$fbs = as.factor(data$fbs)
data$restecg = as.factor(data$restecg)
data$exang = as.factor(data$exang)
data$slope = as.factor(data$slope)

#Como converter ? para NA e fatores? 
data$ca = as.integer(data$ca)
data$ca = as.factor(data$ca)

data$thal = as.integer(data$thal)
data$thal = as.factor(data$thal)

#Converter para palavras as condições de 0 e 1 (aliás pra que??)
data$hd = ifelse(test = data$hd == 0, yes = "Health", no = "Unhealthy")
data$hd = as.factor(data$hd)

#Checar as mudanças:
str(data)



# RandomForest ------------------------------------------------------------
set.seed(42) # To reproduce our results

#Random Forest que permite o Impute
# De 4 até 6 iterações geralmente é suficiente
data_imputed = rfImpute(hd ~ ., data = data, iter = 6)

model = randomForest(hd ~ ., data = data_imputed, proximity = TRUE)
model

