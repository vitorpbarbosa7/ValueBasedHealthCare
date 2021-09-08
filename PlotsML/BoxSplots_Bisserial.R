rm(list=ls())
setwd('C:/GD/DS/Lefort/PlotsML')

library(tidyverse)
library(reshape2)

meltdata = read.csv(file = 'indata/meltdataboxsplot.csv')
meltdata[1] = NULL

meltdata[c(1:103)] = lapply(meltdata[c(1:103)], factor)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'none',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #PosiÃ§Ã£o da legenda
        plot.title = element_text(hjust =0.5), #PosiÃ§Ã£o do tÃ?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour ='black'),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos tÃ?tulos de cada face

ordvar = 'SF12_M1_2_v1'
plotdata = subset(meltdata, variable == ordvar)

plotdata$value = as.numeric(as.character(plotdata$value))

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico

meandata = plotdata[!is.na(plotdata$value),]

g = ggplot(data = plotdata, 
       aes(x = DN4_2c_v1, y = value)) + #################################################################
  geom_boxplot(aes(fill = DN4_2c_v1), alpha = 0.6) +
  geom_jitter(size = 0.5) + #################################################################
  ylab('Valor') + 
  xlab('Presença do sintoma de Adormecimento \n na mesma área da dor (Questão 2b do DN4) ') + 
  ggtitle('Short Form Health Survey (SF-12) \n Limitação da saúde atual em atividades moderadas 
          tais como deslocar uma mesa, aspirar a casa, andar de bicicleta, ou nadar') + 
  theme

g

dev.off()
#####################################
tiff('outputdata/imgs/boxplots/B.tiff', units = "in", width = 10, height = 8, res = 300)













