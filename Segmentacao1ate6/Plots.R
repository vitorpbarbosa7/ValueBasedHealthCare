rm(list=ls())
setwd('C:/GD/DS/Lefort/Segmentacao1ate6')

biserial = read.csv('outputdata/biseralcorrelation_Q1_Q2_Q3_Q4_Q5_Q6_V2.csv', sep = ',')
biserial$rpb = round(as.numeric(biserial$rpb), 2)

library(tidyverse)

# Plots -------------------------------------------------------------------
Dor_Lombar = subset(biserial, Binaria == 'D30' & (rpb >= +0.4 | rpb <= -0.4))

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'none',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posição da legenda
        plot.title = element_text(hjust =0.5), #Posição do título
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour ='black'),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos títulos de cada face
  
g = ggplot(Dor_Lombar, aes(x = reorder(Ordinal,rpb) ,y = rpb, fill = rpb)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(
    low = 'red', high = 'blue'
  ) + 
  geom_text(aes(label = rpb, y = rpb + .05*sign(rpb) ) ) + 
  coord_flip() + 
  xlab("Variáveis contínuas ou ordinais") + 
  ylab("Coeficiente de correlação de ponto bisserial") + 
  ggtitle("Principais correlações de variáveis de caráter \n ordinal com a dor na lombar") + 
  theme

tiff('outputdata/imgs/Dor_Lombar_MainCorrelations.tiff', width = 14, height = 10, units = "in", res = 300)
g
dev.off()


# Tentar um facet ---------------------------------------------------------

listarpb = subset(biserial, (Binaria == 'D30') & (rpb >= +0.45 | rpb <= -0.45))$Ordinal

Binarias = subset(biserial, (Binaria == 'Dor_costas' | Binaria == 'D30' | Binaria == 'Dor_pernas'))

Binarias = subset(Binarias, Ordinal %in% listarpb)



# Para mudar o nome na vertical
labels = c(D30 = "Lombalgia (BPI - 30)", Dor_costas = "Dor nas costas", Dor_pernas = "Dor nas pernas")

g = ggplot(Binarias, aes(x = reorder(Ordinal, rpb), y = rpb, fill = rpb)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_gradient2(
    low = 'red', high = 'blue'
  ) + 
  geom_text(aes(label = rpb, y = rpb + .05*sign(rpb) ) ) + 
  coord_flip() + 
  facet_wrap(.~Binaria, labeller = labeller(Binaria = labels)) + 
  xlab("Variáveis continuas ou ordinais") + 
  ylab("Coeficiente de correlação de ponto bisserial") + 
  theme

tiff('outputdata/imgs/Facet_Biseral_Correlation_D30_Costas_Pernas.tiff', width = 15, height = 6, units = "in", res = 150)
g
dev.off()


# Da Tabela de Contingência 

cruzada = read.csv('outputdata/Crosstable_Got_BPI_RMDQ_DN.csv', sep = ',', encoding = 'UTF-8')
names(cruzada)[4] = 'Frequência'
names(cruzada)[2] = 'Denominador'

cruzada$Frequência = round(cruzada$Frequência, 2)

listafreq = subset(cruzada, Denominador == 'D30' & Frequência >= +0.5)$numerador

Set = subset(cruzada, (Denominador == 'Dor_costas' | Denominador == 'D30' | Denominador == 'Dor_pernas'))

mydata = subset(Set, numerador %in% listafreq)

labels = c(D30 = "Lombalgia (BPI - 30)", Dor_costas = "Dor nas costas", Dor_pernas = "Dor nas pernas")

g = ggplot(mydata, aes(x = reorder(numerador, Frequência), y = Frequência, fill = Frequência)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_gradient2(
    low = 'yellow', high = 'blue'
  ) + 
  geom_text(aes(label = Frequência, y = Frequência + .05*sign(Frequência) ) ) + 
  coord_flip() + 
  facet_wrap(.~Denominador, labeller = labeller(Denominador = labels)) + 
  xlab('Variáveis binárias') + 
  ylab("Frequência da tabela de contingência") + 
  theme

tiff('outputdata/imgs/Facet_Contingencia_D30_Costas_Pernas.tiff', width = 10, height = 6, units = "in", res = 150)
g
dev.off()


# Bubbles -----------------------------------------------------------------

# Plots -------------------------------------------------------------------
Dor_Lombar = subset(biserial, Binaria == 'D30' & (rpb >= +0.4 | rpb <= -0.4))

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'none',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posição da legenda
        plot.title = element_text(hjust =0.5), #Posição do título
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour ='black'),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos títulos de cada face

ggplot(Dor_Lombar, aes(x = reorder(Ordinal,rpb),
                       y = rpb,
                       size = rpb)) + 
  geom_point(shape = 21, aes(fill = as.factor(Ordinal)))

