rm(list=ls())
setwd('C:/GD/DS/Lefort/Segmentacao1ate4')

biserial = read.csv('outputdata/biseralcorrelation_Q1_Q2_Q3_Q4.csv', sep = ',')
biserial$rpb = round(as.numeric(biserial$rpb), 2)

library(tidyverse)

# Plots -------------------------------------------------------------------
Dor_Lombar = subset(biserial, Binaria == 'D30' & (rpb >= +0.4 | rpb <= -0.4))

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'none',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #PosiÃ§Ã£o da legenda
        plot.title = element_text(hjust =0.5), #PosiÃ§Ã£o do tÃ­tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour ='black'),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos tÃ­tulos de cada face
  
ggplot(Dor_Lombar, aes(x = reorder(Ordinal,rpb) ,y = rpb, fill = rpb)) + 
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
  scale_y_discrete(labels = c("AAIntensidade da dor nas costas","BPI_6_v1",
                              "Intensidade da dor nas pernas","Tempo com a dor nas costas",
                              "Tempo com a dor que irradia para as pernas",
                              "Quantidade de cirurgias na coluna",
                              "Há quanto tempo estão em São Paulo se de outro local",
                              "Quantos metros consegue andar em velocidade normal")) +
  theme

tiff('outputdata/img/Facet_Biseral_Correlation_D30_Costas_Pernas.tiff', width = 10, height = 6, units = "in", res = 150)
g
dev.off()


# Da Tabela de ContingÃªncia: ----------------------------------------------

cruzada = read.csv('outputdata/Crosstable_Got_BPI_RMDQ.csv', sep = ';', encoding = 'UTF-8')
names(cruzada)[4] = 'Frequência'
names(cruzada)[2] = 'Denominador'

cruzada$Frequência = round(cruzada$Frequência, 2)

listafreq = subset(cruzada, Denominador == 'D30' & Frequência >= +0.6)$numerador

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

tiff('outputdata/img/Facet_Contingencia_D30_Costas_Pernas.tiff', width = 10, height = 6, units = "in", res = 150)
g
dev.off()



