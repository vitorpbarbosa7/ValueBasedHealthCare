# library
rm(list=ls())
library(tidyverse)

setwd('C:/GD/DS/Lefort/RelevanciaEstatistica/RMDQ_Oswestry_SF-12')

d30 = read.csv('outputdata/biserial_pvalue_DoresPrincipais.csv', encoding = 'UTF-8')

names(d30)[3] = 'value'
names(d30)[2] = 'individual'
names(d30)[1] = 'Variável'

data = d30
data[c(4:6)] = NULL
data$value = 100*data$value

#Change factor level order 
data$Variável = fct_rev(data$Variável)

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

gbar = ggplot(data, aes(x = reorder(individual,value), y = value, fill = Variável)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  coord_flip() + 
  xlab('Variáveis ordinais') +
  ylab('Coeficiente de correlação de ponto bisserial') + 
  theme
gbar


gseg = ggplot(data, aes(x = reorder(individual, value), y = value)) +
  geom_point(aes(color = Variável, fill = Variável), size = 5, 
             stat = "identity", position = position_dodge(0.8)) +
  geom_segment(aes(x = individual, xend = individual, y = 0, yend = value,
                   color = Variável, fill = Variável), alpha = 0.5, size = 2,
               stat = "identity", position = position_dodge(0.8)) +
  coord_flip() +
  # geom_text(aes(label = value), hjust = 1.2, vjust = -0.09, size = 6) + 
  xlab("Variáveis ordinais") +
  ylab("Coeficiente de correlação de ponto bisserial") +
  # scale_color_manual(name = 'Família', values =  cores) +
  # scale_fill_manual(name = 'Família', values =  cores) +
  theme
gseg

  tiff('outputdata/imgs/Lombar_Costas_Pernas_Bisserial.tiff', width = 12, height = 10, units ='in', res = 300)
gbar
dev.off()

