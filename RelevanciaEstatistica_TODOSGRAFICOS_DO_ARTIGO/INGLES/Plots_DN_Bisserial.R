# library
library(tidyverse)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  Variable=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

setwd('C:/GD/DS/Lefort/RelevanciaEstatistica/INGLES')

d30 = read.csv('outputdata/biserial_DN_pvaluefilter_03.csv', encoding = 'UTF-8')

names(d30)[3] = 'value'
names(d30)[2] = 'individual'
names(d30)[1] = 'Variable'

data = d30
data[c(4:6)] = NULL
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

#Para os casos de valores negativos
label_data$position = ifelse(label_data$value>0,label_data$value,50)

# Mudar a legenda
str(label_data)
label_data$individual = c(label_data$individual[1:3],'BPI - Relationship with \n other people - (64.42)')
label_data$individual

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=individual)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha = 0.6) +
  ylim(-70,102) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(), 
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 25),
    #Tamanho da legenda
    legend.title = element_text(size = 35),
    legend.text = element_text(size = 32), 
    axis.text.y = element_blank()
  ) + 
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=position-70, label=individual, hjust=hjust), 
            color="black", fontface="bold",alpha=0.9, size=8, angle= label_data$angle, inherit.aes = FALSE ) + 
  xlab('') + 
  ylab('')
  # ggtitle("Correlação entre a questão do questionário DN4 - Alfinetada e agulhada 
  #         com variáveis de caráter ordinal")
p
tiff('outputdata/imgs/DN_Bisserial.tiff', width = 16, height = 16, units ='in', res = 200)
p
dev.off()

