setwd('C:/GD/DS/Lefort/PlotsML')

d30 = read.csv('indata/Circle_Lombalgia_Pernas_Costas.csv')
names(d30)[3] = 'Frequência'
names(d30)[1] = 'denominador'

library(tidyverse)

ggplot(d30, aes(x = as.factor(numerador), y = Frequência, fill = denominador)) + 
  geom_col(position = "dodge") + 
  coord_polar() + 
  theme_minimal()
