# DN

# library
library(tidyverse)

setwd('C:/GD/DS/Lefort/PlotsML')

data_sur_job = read.csv('outputdata/DN/Dn_Crosstable_Plot.csv', encoding = 'UTF-8')

names(data_sur_job)[3] = 'value'
names(data_sur_job)[2] = 'individual'
names(data_sur_job)[1] = 'Variável'

data = data_sur_job
data[c(4:9)] = NULL
data$value = 100*data$value

#Change factor level order 
data$Variável = fct_rev(data$Variável)

# Set a number of 'empty bar' to add at the end of each Variável
empty_bar <- 0
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Variável), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Variável <- rep(levels(data$Variável), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(Variável)
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
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=Variável)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha = 0.6) +
  #scale_fill_manual(values = c("#FC4E07","#9400D3")) + 
  ylim(0,100) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_blank(), 
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 25),
    #Tamanho da legenda
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14), 
    axis.text.y = element_blank()
  ) + 
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value-10, label=individual, hjust=hjust), 
            color="black", fontface="bold",alpha=0.9, size=6, angle= label_data$angle, inherit.aes = FALSE ) + 
  xlab('') + 
  ylab('') + 
  ggtitle("Correlação entre variáveis binárias \n a partir da tabela cruzada")

tiff('outputdata/imgs/DN_Crosstable.tiff', width = 15, height = 15, units ='in', res = 300)
p
dev.off()
