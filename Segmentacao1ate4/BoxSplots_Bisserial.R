rm(list=ls())
setwd('C:/GD/DS/Lefort/Segmentacao1ate4')

library(tidyverse)
library(reshape2)

meltdata = read.csv(file = 'outputdata/meltdataboxsplot.csv')
meltdata[1] = NULL

meltdata[c(1:92)] = lapply(meltdata[c(1:92)], factor)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'none',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #PosiÃ§Ã£o da legenda
        plot.title = element_text(hjust =0.5), #PosiÃ§Ã£o do tÃ�tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour ='black'),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos tÃ�tulos de cada face

ordvar = 'Quantos.metros.consegue.andar.em.velocidade.normal'
plotdata = subset(meltdata, variable == ordvar)

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico

meandata = plotdata[!is.na(plotdata$value),]

medias = meandata %>%
  group_by(D30) %>% #########################################################################
  summarise_at(vars(value), list(name = mean))
nft = length(medias)
names(medias)[nft] = 'media'
medias[nft] = round(medias[nft], 2)

g = ggplot(data = subset(meltdata, variable == ordvar), 
       aes(x = D30, y = value)) + #################################################################
  geom_boxplot(aes(fill = D30)) + #################################################################
  geom_jitter(size = 0.5) + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = medias, aes(label = media, y = media + 0.1),size =5) +
  facet_wrap(.~variable, scales = 'free') + 
  ylab('Valor') + 
  theme

#####################################
tiff('outputdata/img/boxsplots/D30_AndarMetros.tiff', units = "in", width = 8, height = 6, res = 150)
g
dev.off()


ggplot(data = meltdata, aes(x = Dor_costas, y = value)) +
  geom_boxplot(aes(fill = Dor_costas)) +
  geom_jitter(size = 0.5) + 
  facet_wrap(.~variable, scales = 'free') + 
  ylab('Valor') + 
  theme
dev.off()















