rm(list = ls())

#Para fazer esse projeto vou usar os dados disponibilizados neste post no Kaggle:
#https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset

#definindo as librarys e importando o dataSet
library(dplyr)
library(Hmisc)

COVID19_data <- read.csv("~/Covid_R_enviar/Covid_r/Data/COVID19_line_list_data.csv")

#arrumando os dados

str(COVID19_data)
describe(COVID19_data)

#apagando partes vazias/não-uteis

COVID19_data$id <- NULL
COVID19_data$X <- NULL
COVID19_data$X.1 <- NULL
COVID19_data$X.2 <- NULL
COVID19_data$X.3 <- NULL
COVID19_data$X.4 <- NULL
COVID19_data$X.5 <- NULL
COVID19_data$X.6 <- NULL
COVID19_data$If_onset_approximated <- NULL

COVID19_data$death <- as.integer(COVID19_data$death != 0)
describe(COVID19_data$death)

#mudando os nomes dos headers

names(COVID19_data) <- c("Casos_sendo_estudados","Data_do_relato","Resumo",
                         "Local","País","Genero","Idade","Aparecimento_dos_sintomas",
                         "Data_de_visita_ao_Hospital","Começo_da_exposição",
                         "Final_da_exposição","Visitou_Wuhan","Mora_em_Wuhan",
                         "morte","sobreviveu","Sintomas","Fonte","Link")

#qual foi o pais com o maior número de ocorrencia?

x <- sort(table(COVID19_data$País), decreasing=TRUE)
write.table(x ,file ="casosporpaís.csv", row.names = FALSE) #salvando parar usar em mapa

  #China com 197 casos

#calculando a taxa de mortalidade:

taxa_de_mortalidade <- sum(COVID19_data$morte) / nrow(COVID19_data)

#qual a porcentagem de pessoas que morava ou visitou wuhan?

Total_Visitou <-sum(COVID19_data$Visitou_Wuhan) 
Total_Mora <- sum(COVID19_data$Mora_em_Wuhan, na.rm = TRUE)
Envolvido_Wuhan <- (Total_Mora+Total_Visitou)

  percentagem_wuhan <- Envolvido_Wuhan / nrow(COVID19_data)
  # são 32% dos casos envolvidos de alguma maneira com Wuhan

#pessoas mais velhas são mais fáceis de morrer?

mortos <- subset(COVID19_data, morte == 1)
  mean(mortos$Idade, na.rm = TRUE)

vivos <- subset(COVID19_data, morte == 0)
  mean(vivos$Idade, na.rm = TRUE)

  #fazendo o t test
  
  t.test(mortos$Idade, vivos$Idade, alternative = "two.sided", conf.level = 0.99)
  #t = 10.839, df = 72.234, p-value < 2.2e-16 / Sim, pessoas mais velhas tem taxas
  #de letalidade maior

#genero influenci no número de mortes?
  
homens <- subset(COVID19_data, Genero == "male")
  mean(homens$morte, na.rm = TRUE)
  
mulheres <- subset(COVID19_data, Genero == "female")
  mean(mulheres$morte, na.rm = TRUE)
  
  #fazendo o t test
  t.test(mulheres$morte, homens$morte, alternative = "two.sided", conf.level = 0.99)
  #t = -3.084, df = 894.06, p-value = 0.002105 / Sim, idade influencia Homens são mais afetados.
  
  