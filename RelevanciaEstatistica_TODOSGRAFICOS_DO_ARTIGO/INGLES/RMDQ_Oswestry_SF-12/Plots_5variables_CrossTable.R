# library
rm(list=ls())
library(tidyverse)

setwd('C:/GD/DS/Lefort/RelevanciaEstatistica/RMDQ_Oswestry_SF-12')

d30 = read.csv('outputdata/datachi_3variables.csv')

names(d30)[3] = 'value'
names(d30)[2] = 'individual'
names(d30)[1] = 'Variable'

data = d30
data[c(4:10)] = NULL
data$value = 100*data$value

#Change factor level order 
data$Variable = fct_rev(data$Variable)

# Plot com barras ---------------------------------------------------------
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posição da legenda
        plot.title = element_text(hjust =0.5), #Posição do título
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour ='black'),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos títulos de cada face

gbar = ggplot(data, aes(x = reorder(individual,value), y = value, fill = Variable)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  coord_flip() + 
  xlab('Variáveis binárias') +
  ylab('Frequência') + 
  theme
gbar

tiff('outputdata/imgs/Lombar_Costas_Pernas_Crosstable.tiff', width = 6, height = 4, units ='in', res = 150)
gbar
dev.off()

