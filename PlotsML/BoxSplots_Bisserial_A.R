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

ordvar = 'BPI_9a_v1'
plotdata = subset(meltdata, variable == ordvar)

plotdata$value = as.numeric(as.character(plotdata$value))

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico

meandata = plotdata[!is.na(plotdata$value),]

g = ggplot(data = plotdata, 
       aes(x = DN4_2b_v1, y = value)) + #################################################################
  geom_boxplot(aes(fill = DN4_2b_v1), alpha = 0.6) +
  geom_jitter(size = 0.5) + #################################################################
  ylab('Valor') + 
  xlab('Presença do sintoma de \n Alfinetada e Agulhada na mesma área da dor (Questão 2b do questionário DN4)') + 
  ggtitle('Inventário Breve de dor - Atividade geral:
          {0: Não inteferiu ; 10: Interferiu completamente}') + 
  theme
g

tiff('outputdata/imgs/boxplots/A.tiff', units = "in", width = 10, height = 8, res = 300)
g
dev.off()












