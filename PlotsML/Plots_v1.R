setwd('C:/GD/DS/Lefort/PlotsML')

d30 = read.csv('indata/Crosstable_D30_Plot.csv')
names(d30)[3] = 'Frequência'

library(tidyverse)

ggplot(d30, aes(x = as.factor(numerador), y = Frequência)) + 
  geom_col(position = "dodge") + 
  coord_polar()
