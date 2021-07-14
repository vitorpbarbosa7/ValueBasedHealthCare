# library
rm(list=ls())
library(tidyverse)

setwd('C:/GD/DS/Lefort/RelevanciaEstatistica_TODOSGRAFICOS_DO_ARTIGO/INGLES')

d30 = read.csv('outputdata/datachi_5variables.csv')
d30[1] =  NULL

names(d30)[3] = 'value'
names(d30)[2] = 'individual'
names(d30)[1] = 'Variable'

data = d30
data[c(4:10)] = NULL
data$value = 100*data$value

#Change factor level order 
data$Variable = fct_rev(data$Variable)

# Set a number of 'empty bar' to add at the end of each Variable
empty_bar <- 0
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Variable), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Variable <- rep(levels(data$Variable), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(Variable)
data$id <- seq(1, nrow(data))


# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

#Criar uma label customizada
label_data$value = round(label_data$value, 2)
label_data$individual = paste(label_data$individual," - (",label_data$value,")",sep = "")

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=Variable)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha = 0.6) +
  ylim(0,100) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_blank(), 
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 25),
    #Tamanho da legenda
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 28), 
    axis.text.y = element_blank()
  ) + 
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value-40, label=individual, hjust=hjust), 
            color="black", fontface="bold",alpha=0.9, size=8, angle= label_data$angle, inherit.aes = FALSE ) + 
  xlab('') + 
  ylab('')
  # ggtitle("Correlação entre variáveis binárias \n a partir da tabela cruzada")
p

library(Cairo)

CairoPNG(filename = "outputdata/imgs/Lombar_Costas_Pernas_Desemprego_Cirurgia_APRESENTACAO.png", bg = "transparent",
         width = 13, height = 13, units = 'in', dpi = 300)
p
dev.off()



ggsave(p, filename = "outputdata/imgs/Lombar_Costas_Pernas_Desemprego_Cirurgia.tiff",  bg = "transparent",
       width = 13, height = 13, units = 'in')

tiff('outputdata/imgs/Lombar_Costas_Pernas_Desemprego_Cirurgia.tiff', 
     width = 13, height = 13, units ='in', res = 300)
p
dev.off()

