setwd('C:/GD/DS/Lefort/SPPS_R')

library(foreign)

data = read.spss('Banco.sav', to.data.frame = TRUE)
