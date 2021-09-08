# GGplot
rm(list=ls())

setwd('C:/GD/DS/Lefort/RelevanciaEstatistica/INGLES/ML')

data = read.csv('outputdata/pca.csv')
data[1] = NULL

# Tratamento da classe lombalgia para plot
data$classe = sub('False','No',data$classe)
data$classe = sub('True','Yes',data$classe)
data$classe = as.factor(data$classe)

#Plot ggplot 2D
library(tidyverse)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posição da legenda
        plot.title = element_text(hjust =0.5), #Posição do título
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos títulos de cada 

names(data)[4] = 'Lombalgia'

names(data)

g = ggplot(data, aes(x = PC1, y = PC2)) + 
  geom_point(aes(fill = Lombalgia, color = Lombalgia, shape = Lombalgia), size = 3) + 
  xlab('Principal Component 1') + 
  ylab('Principal Component 2') + 
  scale_fill_manual(values = c('#FF0000','#0000FF')) + 
  scale_color_manual(values = c('#FF0000','#0000FF')) + 
  labs(fill = 'Low back pain', color = 'Low back pain', shape = "Low back pain") + 
theme
g

tiff('outputdata/img/PCA2D.tiff', units = 'in', width = 8, height = 6, res = 300)
g
dev.off()
