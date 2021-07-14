setwd('C:/GD/DS/Lefort/MissingVisualization')
getwd()

data = read.csv('dados.csv')

# Visualizar valores missing
library(Amelia)

map_missing = missmap(obj = data)
