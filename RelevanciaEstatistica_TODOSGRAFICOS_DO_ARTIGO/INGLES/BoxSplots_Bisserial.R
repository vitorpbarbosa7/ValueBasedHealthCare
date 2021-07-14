rm(list=ls())
setwd('C:/GD/DS/Lefort/RelevanciaEstatistica/INGLES')

library(tidyverse)
library(reshape2)

meltdata = read.csv(file = 'outputdata/meltdataboxsplot.csv')
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

ordvar = 'BPI_9e_v1'
plotdata = subset(meltdata, variable == ordvar)

plotdata$value = as.numeric(as.character(plotdata$value))

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico

meandata = plotdata[!is.na(plotdata$value),]

library(RColorBrewer)

g = ggplot(data = plotdata, 
       aes(x = DN4_2b_v1, y = value)) + #################################################################
  geom_boxplot(aes(fill = DN4_2b_v1), alpha = 0.7) +
  scale_fill_brewer(palette = 'Set3') + 
  geom_jitter(size = 1, color = 'red') + #################################################################
  ylab('Value') + 
  xlab("Patient indicates wether or not the pain's region presents the
       symptom of paresthesia - feeling pins and needles") +  
  ggtitle("Pain's interfence with the relationship with other people") + 
  theme
g

#####################################
tiff('outputdata/imgs/boxplots/B.tiff', units = "in", width = 8, height = 6, res = 300)
g
dev.off()













