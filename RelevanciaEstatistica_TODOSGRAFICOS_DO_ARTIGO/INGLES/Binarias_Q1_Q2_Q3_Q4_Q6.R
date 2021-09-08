rm(list = ls())
setwd('C:/GD/DS/Lefort/RelevanciaEstatistica/INGLES')

library(tidyverse)
library(foreign)
library(psych)
library(gmodels)

Bin_Got_BPI_RMDQ_DN = read.csv('indata/Bin_Got_BPI_RMDQ_DN.csv', encoding = 'UTF-8')

datacorr = Bin_Got_BPI_RMDQ_DN
datacorr[1] = NULL

# Buscar pela relevância estatística destes dados -------------------------
cross = CrossTable(datacorr$Dor_pernas, datacorr$Livrar_costas, chisq = TRUE)

S# Tabela de contingência --------------------------------------------------
library(gmodels)

freq22 = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
#Matrix nome das variavei
denominador = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
numerador = matrix(NA, nrow = length(datacorr)^2, ncol = 1)
qte_denom = matrix(NA, nrow = length(datacorr)^2, ncol = 2)

nums = matrix(NA, nrow = length(datacorr)^2, ncol = 4)
chi = matrix(NA, nrow = length(datacorr)^2, ncol = 1)

cont = 1
for (i in 1:length(datacorr)){
  for (j in 1: length(datacorr)){
    
    cross = CrossTable(datacorr[[j]],datacorr[[i]], format = "SAS", chisq = TRUE);
    
    freq22[cont,] = as.numeric(cross$prop.row[2,2])
    qte_denom[cont,c(1,2)] = c(as.numeric(cross$t[2,1]) + as.numeric(cross$t[2,2]),as.numeric(cross$t[1,1]) + as.numeric(cross$t[1,2]))
    
    denominador[cont,] = names(datacorr[j])
    numerador[cont,] = names(datacorr[i])
    
    nums[cont,c(1:4)] = c(as.numeric(cross$t[1,1]), as.numeric(cross$t[1,2]), as.numeric(cross$t[2,1]), as.numeric(cross$t[2,2]))
    
    chi[cont,] = as.numeric(cross$chisq[3])
    
    cont = cont + 1
  }
}

crosstable = data.frame(cbind(denominador, numerador, freq22, chi, qte_denom, nums))
#Para lidar com perda de variaveis de outras abas
df_crosstable = crosstable

crosstable = df_crosstable
names(crosstable) = c("denominador", "numerador", "Frequência","P-value","Qtd_1_Denom", "Qtd_0_Demon",
                      "D0N0",'D0N1','D1N0','D1N1')
#Deletar as variaveis que podem confundir com nome de colunas do dataframe
#rm(denominador,numerador, freq22,chi,qte_denom,nums)

crosstable$Frequência = as.numeric(crosstable$Frequência)
crosstable$`P-value` = as.numeric(crosstable$`P-value`)
crosstable$Qtd_1_Denom = as.numeric(crosstable$Qtd_1_Denom)
crosstable$Qtd_0_Demon = as.numeric(crosstable$Qtd_0_Demon)
crosstable$D0N0 = as.numeric(crosstable$D0N0)
crosstable$D0N1 = as.numeric(crosstable$D0N1)
crosstable$D1N0 = as.numeric(crosstable$D1N0)
crosstable$D1N1 = as.numeric(crosstable$D1N1)


#Substituir os nomes das variaveis de Dor Neuropatica
oldDN4 = c("DN4_1a_v1","DN4_1b_v1","DN4_1c_v1","DN4_2a_v1","DN4_2b_v1","DN4_2c_v1","DN4_2d_v1","DN4_3a_v1","DN4_3b_v1","DN4_4_v1")
newDN4 = c("DN4 - Queimação","DN4 - Sensação de frio dolorosa","DN4 - Choque elétrico","DN4 - Formigamento",
             "DN4 - Alfinetada e agulhada","DN4 - Adormecimento","DN4 - Coceira",
             "DH4 - Hipoestesia ao toque","DN4 - Hipoestesia a picada de agulha",
             "DN4 - Escovação")

oldnames = c("Dor_costas","Analg.e9.sicos","Casado_Amasiado","D30","Dor_pernas","Cirurgia_coluna")
newnames = c("Dor nas costas","Analgésicos","Casado/Amasiaado","Lombalgia","Dor nas pernas","Cirurgia na coluna")

#Substituir
for (i in 1:length(oldDN4)){
crosstable$denominador = sub(oldDN4[i],newDN4[i],crosstable$denominador)
crosstable$numerador = sub(oldDN4[i],newDN4[i],crosstable$numerador)
}

for (i in 1:length(newnames)){
crosstable$denominador = sub(oldnames[i],newnames[i],crosstable$denominador)
crosstable$numerador = sub(oldnames[i],newnames[i],crosstable$numerador)
}

#Gravar sem filtro de p-value
write.csv(x = crosstable, file = 'outputdata/crosstable_semfiltropvalue.csv')

#Gravar com filtro de p-value mas não filtro de frequência
data_general = crosstable
data_general = data_general[data_general$`P-value` < 0.05,]
write.csv(x = data_general, file = 'outputdata/crosstable_general.csv')

data = crosstable
datachi = data[data$`P-value` < 0.05, ]
datachi = datachi[datachi$Frequência > 0.5, ]
datachi = datachi[datachi$D0N0 > 10, ]
datachi = datachi[datachi$D1N0 > 10, ]
datachi = datachi[datachi$D0N1 > 10, ]
datachi = datachi[datachi$D1N1 > 10, ]

datachi2 = subset(datachi, !is.na(datachi$D1N1))

write.csv(x = datachi2, file = 'outputdata/datachi.csv')

#Apenas as variaveis de dor neuropatica no denominador
newDN4 = c("DN4 - Queimação","DN4 - Sensação de frio dolorosa","DN4 - Choque elétrico","DN4 - Formigamento",
           "DN4 - Alfinetada e agulhada","DN4 - Adormecimento","DN4 - Coceira",
           "DH4 - Hipoestesia ao toque","DN4 - Hipoestesia a picada de agulha",
           "DN4 - Escovação")
data_DN_read = read.csv('outputdata/datachi.csv')
data_DN_read[1] = NULL

library(tidyverse) #Inclui o dplyr que permite utilizar %>% e %in%
data_DN = data_DN_read %>% filter(
  denominador %in% newDN4
)
data_DN = data_DN[data_DN$Frequência > 0.6, ]

write.csv(x = data_DN, file = 'outputdata/datachi_DN.csv')

#Desemprego e cirurgia
data_read = read.csv('outputdata/crosstable_general.csv')
data_read[1] = NULL
datachi = data_read
datachi = datachi[datachi$D0N0 > 10, ]
datachi = datachi[datachi$D1N1 > 10, ]

#Filtro de frequencia, porque menos que isso nao eh relevante, nao seria uma associacao, uma correlacao
datachi = datachi[datachi$Frequência < 1, ]
datachi = datachi[datachi$Frequência > 0.6, ]

library(tidyverse) #Inclui o dplyr que permite utilizar %>% e %in%
names = c("Desempregado","Cirurgia na coluna","Dor nas pernas","Dor nas costas","Lombalgia")

data_ = datachi %>% filter(
  denominador %in% names
)

data_ = data_[with(data_, order(-Frequência)), ]
write.csv(x = data_, file = 'outputdata/datachi_5variables.csv')

