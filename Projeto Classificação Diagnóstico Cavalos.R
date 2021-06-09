#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#                                                 x
#                                                 x
#     Script - Trabalho Final Data Mining         x
#                                                 x
#                                                 x 
# Grupo:                                          x
#                                                 x
#      Letícia Aranha                             x
#         - lsf.aranha@gmail.com                  x
#                                                 x
#                                                 x
#      Matheus Rangel Cardoso                     x
#         - matheus.rangel.cardoso@gmail.com      x
#                                                 x
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Preparando ambiente de trabalho ####

# Limpar a memória do workspace:
rm(list = ls())

# Limpar o console:
cat("\14")

# Limpar os GrÃ¡ficos:
# dev.off()

# Colocar seed = 1 para teste
 set.seed(1)


# Importando bibliotecas ####

#install.packages("ggplot2")     #criação de gráficos
#install.packages("scales")
#install.packages("dplyr") 
#install.packages("e1071)
#install.packages("DMwR")
#install.packages("caret")        #pre-processamento
#install.packages("psych")        #dummy coding
#install.packages("tree")         #arvore de decisÃ£o
#install.packages("randomForest") #random forest
#install.packages('fastDummies')  #dummy variables
#install.packages('magrittr)      #pipe operator

library(ggplot2)
library(scales)
library(dplyr)
library(e1071)
library(DMwR)
library(caret)
library(data.table)
library(psych)
library(tree)
library(randomForest)
library(fastDummies)
library(magrittr)



# Carregando base de treino e teste ####

setwd('C:\\Users\\mathe\\Documents\\Pos\\04 - DM\\Projeto\\TrabalhoDM_2019.1\\Trabalho')
treino <-  read.csv('Horse.csv')
teste <-  read.csv('HorseTest.csv')


# AcurÃ¡cia do modelo original ####

# Treinando o modelo
tree_model1 <- tree(outcome~., treino)

# PrevisÃ£o utilizando a base de teste
predictionsDtree1 <- predict(tree_model1,
                             teste,
                             type = 'class')

# Matriz de ConfusÃ£o
table1 <- table(predictionsDtree1,
                teste$outcome); table1

# Medindo acurÃ¡cia
acuracy1 <- 1 - mean(predictionsDtree1 
                     != teste$outcome); acuracy1 #60%



# Analise exploratÃ³ria ####

# Valores nulos na base de treino tabela: ####

# Contagem de valores nulos na base de treino:
contagem_nulos_treino = sapply(treino,
                               function(x) 
                                 sum(is.na(x)))

# % dos valores nulos na base de treino
nulos_treino_porcentagem = sapply(treino,
                                  function(y) 
                                    ((sum(is.na(y))*100)/nrow(treino)))

# Criando uma tabela com o a contagem de valores nulos na base de treino
# e a % de valores nulos na base de treino:
dados_treino_analise = data.frame(contagem_nulos_treino,
                                  nulos_treino_porcentagem ); dados_treino_analise

# Valores nulos na base de teste tabela: ####

# Contagem de valores nulos na base de teste:
contagem_nulos_teste = sapply(teste,
                              function(x)
                                sum(is.na(x)))

# % dos valores nulos na base de teste
nulos_teste_porcentagem = sapply(teste,
                                 function(y)
                                   ((sum(is.na(y))*100)/nrow(teste)))

# Criando uma tabela com o a contagem de valores nulos na base de teste
# e a % de valores nulos na base de teste
dados_teste_analise = data.frame(contagem_nulos_teste,
                                 nulos_teste_porcentagem); dados_teste_analise


# Analises grÃ¡ficas dos atributos categoricos ####

#temp_of_extremities
grafico_temp_of_extremities = ggplot(data = treino)
grafico_temp_of_extremities + geom_bar(aes(x = temp_of_extremities, 
                                           fill = temp_of_extremities))

#peripheral_pulse
grafico_pulse = ggplot(data = treino)
grafico_pulse + geom_bar(aes(x = peripheral_pulse,
                             fill = peripheral_pulse))

#mucous_membrane
grafico_mucous_membrane = ggplot(data = treino)
grafico_mucous_membrane + geom_bar(aes(x = mucous_membrane,
                                       fill = mucous_membrane))

#capillary_refill_time
grafico_capillary_refill_time = ggplot(data = treino)
grafico_capillary_refill_time + geom_bar(aes(x = capillary_refill_time,
                                             fill = capillary_refill_time))

#pain
grafico_pain = ggplot(data = treino)
grafico_pain + geom_bar(aes(x = pain,
                            fill = pain))

#peristalsis
grafico_peristalsis = ggplot(data = treino)
grafico_peristalsis + geom_bar(aes(x = peristalsis,
                                   fill = peristalsis))

#abdominal_distention
grafico_abdominal_distention = ggplot(data = treino)
grafico_abdominal_distention + geom_bar(aes(x = abdominal_distention,
                                            fill = abdominal_distention))

# nasogastric_tube
grafico_nasogastric_tube = ggplot(data = treino)
grafico_nasogastric_tube + geom_bar(aes(x = nasogastric_tube,
                                        fill = nasogastric_tube))

# nasogastric_reflux
grafico_nasogastric_reflux = ggplot(data = treino)
grafico_nasogastric_reflux + geom_bar(aes(x = nasogastric_reflux,
                                          fill = nasogastric_reflux))

# rectal_exam_feces
grafico_rectal_exam_feces = ggplot(data = treino)
grafico_rectal_exam_feces + geom_bar(aes(x = rectal_exam_feces,
                                         fill = rectal_exam_feces))

# abdomen
grafico_abdomen = ggplot(data = treino)
grafico_abdomen + geom_bar(aes(x = abdomen,
                               fill = abdomen))



# Identificando atributos com variância zero ####
nearZeroVarianceIndexes = nearZeroVar(treino)



# Descarte de atributos ####

# Foram descartados os atributos:
# 1. Com valores nulos superiores a maioria absoluta:
#         nasogastric_reflux_ph (atributo 16)
#         abdomo_appearance (atributo 21)
#         abdomo_protein (atributo 22)
# 2. Com variÃ¢ncia zero:
#         lesion_2 (atributo 26)
#         lesion_3 (atributo 27)
# 3. Que representam IDs:
#         lesion_1 (atributo 25)
#         hospital number (atributo 3)
# 4. Que foram ditos como irrelevantes pela documentação:
#         cp_dat (atributo 28)


treino <- treino[,-c(16,21,22,26,27,25,3,28)]
teste <- teste[,-c(16,21,22,26,27,25,3,28)]



# Acurácia do modelo após descarte de atributos ####

# Treinando modelo
tree_model2 <- tree(outcome~., treino)

# PrevisÃ£o utilizando a base de teste
predictionsDtree2 <- predict(tree_model2,
                             teste,
                             type = 'class')

# Matriz de confusão
table2 <- table(predictionsDtree2,
                teste$outcome); table2

# Medindo Acurácia
acuracy2 <- 1 - mean(predictionsDtree2
                     != teste$outcome); acuracy2 #67%

# Plotando a Árvore de decisão
summary(tree_model2)
plot(tree_model2)
text(tree_model2)



# Substituindo Missing Values ####

# Pela Mediana, se a variável for contínua ou discreta
# Pela Moda, se a variável for categórica


# Imputando Mediana em treino e teste ####

missingModel <- preProcess(treino, 'medianImpute')

treino <- predict(missingModel, treino)
teste <- predict(missingModel, teste)


# Imputando Moda em treino e teste ####

# Função que calcula a moda:

func_moda<- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

# Aplicando na base de treino:
for (coluna in 1:ncol(treino)){
  moda = func_moda(treino[,coluna])
  idx = which(is.na(treino[,coluna]))
  treino[idx,coluna] = moda
}

# Aplicando na base de teste:
for (coluna in 1:ncol(teste)){
  moda = func_moda(treino[,coluna])
  idx = which(is.na(teste[,coluna]))
  teste[idx,coluna] = moda
}



# Acurácia do modelo após tratamento de Missing Values ####

# Treinando o modelo
tree_model3 <- tree(outcome~., treino)

# PrevisÃ£o utilizando a base de teste
predictionsDtree3 <- predict(tree_model3,
                             teste,
                             type = 'class')

# Matriz de Confusão
table3 <- table(predictionsDtree3,
                teste$outcome); table3

# Medindo Acurácia
acuracy3 <- 1 - mean(predictionsDtree3
                     != teste$outcome); acuracy3 #79%

# Plotando Ã¡rvore de decisÃ£o
summary(tree_model3)
plot(tree_model3)
text(tree_model3)

# Outliers ####

# Para identificar se um valor é ou não um outlier, calcula-se a Amplitude Inter
# Quartil (IRQ)
# IQR = 3º Quartil (Q3) - 1º Quartil (Q1)
# O limite máximo será definido como Q3 + (IRQ * 1,5)
# O limite mínimo será definido como Q1 - (IRQ * 1,5)
# Qualquer valor que se encontre fora dos limites máximo e mínimo será considerado
# um outlier


# Substituindo em treino:

# rectal_temp
rectal_temp_box = ggplot(data=treino, aes( y = rectal_temp))
rectal_temp_box + geom_boxplot()


summary(treino$rectal_temp)

treino[,3]

# 3º Quartil
Q3_rectal_temp = unname(quantile(treino$rectal_temp,0.75))
Q3_rectal_temp

# 1º Quartil
Q1_rectal_temp = unname(quantile(treino$rectal_temp,0.25))
Q1_rectal_temp

# IQR
IRQ_rectal_temp = Q3_rectal_temp - Q1_rectal_temp

# Limite Máximo
Lmax_rectal_temp = Q3_rectal_temp + (1.5*IRQ_rectal_temp)

# Limite Mínimo
Lmin_rectal_temp = Q1_rectal_temp - (1.5*IRQ_rectal_temp)



for (linha in 1:nrow(treino)){
  if(treino[linha,3] > Lmax_rectal_temp){
    treino[linha,3] = Lmax_rectal_temp
  }else if (treino[linha,3] < Lmin_rectal_temp){
    treino[linha,3] = Lmin_rectal_temp
  } else {
    treino[linha,3] = treino[linha,3]
  }
}

treino[,3]


# pulse
pulse_box = ggplot(data=treino, aes( y = pulse))
pulse_box + geom_boxplot()


summary(treino$pulse)

treino[,4]

# 3º Quartil
Q3_pulse = unname(quantile(treino$pulse,0.75))
Q3_pulse

# 1º Quartil
Q1_pulse = unname(quantile(treino$pulse,0.25))
Q1_pulse

# IQR
IRQ_pulse = Q3_pulse - Q1_pulse

# Limite Máximo
Lmax_pulse = Q3_pulse + (1.5*IRQ_pulse)

# Limite Mínimo
Lmin_pulse = Q1_pulse - (1.5*IRQ_pulse)



for (linha in 1:nrow(treino)){
  if(treino[linha,4] > Lmax_pulse){
    treino[linha,4] = Lmax_pulse
  }else if (treino[linha,4] < Lmin_pulse){
    treino[linha,4] = Lmin_pulse
  } else {
    treino[linha,4] = treino[linha,4]
  }
}

treino[,4]

# respiratory_rate
respiratory_rate_box = ggplot(data=treino, aes( y = respiratory_rate))
respiratory_rate_box + geom_boxplot()


summary(treino$respiratory_rate)

treino[,5]

# 3º Quartil
Q3_respiratory_rate = unname(quantile(treino$respiratory_rate,0.75))
Q3_respiratory_rate

# 1º Quartil
Q1_respiratory_rate = unname(quantile(treino$respiratory_rate,0.25))
Q1_respiratory_rate

# IQR
IRQ_respiratory_rate = Q3_respiratory_rate - Q1_respiratory_rate

# Limite Máximo
Lmax_respiratory_rate = Q3_respiratory_rate + (1.5*IRQ_respiratory_rate)

# Limite Mínimo
Lmin_respiratory_rate = Q1_respiratory_rate - (1.5*IRQ_respiratory_rate)



for (linha in 1:nrow(treino)){
  if(treino[linha,5] > Lmax_respiratory_rate){
    treino[linha,5] = Lmax_respiratory_rate
  }else if (treino[linha,5] < Lmin_respiratory_rate){
    treino[linha,5] = Lmin_respiratory_rate
  } else {
    treino[linha,5] = treino[linha,5]
  }
}

treino[,5]


# packed_cell_volume
packed_cell_volume_box = ggplot(data=treino, aes( y = packed_cell_volume))
packed_cell_volume_box + geom_boxplot()


summary(treino$packed_cell_volume)

treino[,17]

# 3º Quartil
Q3_packed_cell_volume = unname(quantile(treino$packed_cell_volume,0.75))
Q3_packed_cell_volume

# 1º Quartil
Q1_packed_cell_volume = unname(quantile(treino$packed_cell_volume,0.25))
Q1_packed_cell_volume

# IRQ
IRQ_packed_cell_volume = Q3_packed_cell_volume - Q1_packed_cell_volume

# Limite Máximo
Lmax_packed_cell_volume = Q3_packed_cell_volume + (1.5*IRQ_packed_cell_volume)

# Limite Mínimo
Lmin_packed_cell_volume = Q1_packed_cell_volume - (1.5*IRQ_packed_cell_volume)



for (linha in 1:nrow(treino)){
  if(treino[linha,17] > Lmax_packed_cell_volume){
    treino[linha,17] = Lmax_packed_cell_volume
  }else if (treino[linha,17] < Lmin_packed_cell_volume){
    treino[linha,17] = Lmin_packed_cell_volume
  } else {
    treino[linha,17] = treino[linha,17]
  }
}

treino[,17]


# total_protein
total_protein_box = ggplot(data=treino, aes( y = total_protein))
total_protein_box + geom_boxplot()


summary(treino$total_protein)

treino[,18]

# 3º Quartil
Q3_total_protein = unname(quantile(treino$total_protein,0.75))
Q3_total_protein

# 1º Quartil
Q1_total_protein = unname(quantile(treino$total_protein,0.25))
Q1_total_protein

# IQR
IRQ_total_protein = Q3_total_protein - Q1_total_protein

# Limite Máximo
Lmax_total_protein = Q3_total_protein + (1.5*IRQ_total_protein)

# Limite Mínimo
Lmin_total_protein = Q1_total_protein - (1.5*IRQ_total_protein)



for (linha in 1:nrow(treino)){
  if(treino[linha,18] > Lmax_total_protein){
    treino[linha,18] = Lmax_total_protein
  }else if (treino[linha,18] < Lmin_total_protein){
    treino[linha,18] = Lmin_total_protein
  } else {
    treino[linha,18] = treino[linha,18]
  }
}

treino[,18]


# Substituindo em teste:

# rectal_temp
rectal_temp_box = ggplot(data=teste, aes( y = rectal_temp))
rectal_temp_box + geom_boxplot()

for (linha in 1:nrow(teste)){
  if(teste[linha,3] > Lmax_rectal_temp){
    teste[linha,3] = Lmax_rectal_temp
  }else if (teste[linha,3] < Lmin_rectal_temp){
    teste[linha,3] = Lmin_rectal_temp
  } else {
    teste[linha,3] = teste[linha,3]
  }
}


# pulse
pulse_box = ggplot(data=teste, aes( y = pulse))
pulse_box + geom_boxplot()

for (linha in 1:nrow(teste)){
  if(teste[linha,4] > Lmax_pulse){
    teste[linha,4] = Lmax_pulse
  }else if (teste[linha,4] < Lmin_pulse){
    teste[linha,4] = Lmin_pulse
  } else {
    teste[linha,4] = teste[linha,4]
  }
}

teste[,4]


# respiratory_rate
respiratory_rate_box = ggplot(data=teste, aes( y = respiratory_rate))
respiratory_rate_box + geom_boxplot()

for (linha in 1:nrow(teste)){
  if(teste[linha,5] > Lmax_respiratory_rate){
    teste[linha,5] = Lmax_respiratory_rate
  }else if (teste[linha,5] < Lmin_respiratory_rate){
    teste[linha,5] = Lmin_respiratory_rate
  } else {
    teste[linha,5] = teste[linha,5]
  }
}

teste[,5]


# packed_cell_volume
packed_cell_volume_box = ggplot(data=teste, aes( y = packed_cell_volume))
packed_cell_volume_box + geom_boxplot()

for (linha in 1:nrow(teste)){
  if(teste[linha,17] > Lmax_packed_cell_volume){
    teste[linha,17] = Lmax_packed_cell_volume
  }else if (teste[linha,17] < Lmin_packed_cell_volume){
    teste[linha,17] = Lmin_packed_cell_volume
  } else {
    teste[linha,17] = teste[linha,17]
  }
}

teste[,17]


# total_protein #
total_protein_box = ggplot(data=teste, aes( y = total_protein))
total_protein_box + geom_boxplot()

for (linha in 1:nrow(teste)){
  if(teste[linha,18] > Lmax_total_protein){
    teste[linha,18] = Lmax_total_protein
  }else if (teste[linha,18] < Lmin_total_protein){
    teste[linha,18] = Lmin_total_protein
  } else {
    teste[linha,18] = teste[linha,18]
  }
}

teste[,18]



# Acurácia do modelo após o tratamento de Outliers ####

# Treinando o modelo
tree_model4 <- tree(outcome~., treino)

# Previsão utilizando a base de teste
predictionsDtree4 <- predict(tree_model4,
                             teste,
                             type = 'class')

# Matriz de confusão
table4 <- table(predictionsDtree4,
                teste$outcome); table4

# Medindo acurácia
acuracy4 <- 1 - mean(predictionsDtree4
                     != teste$outcome); acuracy4 #79%

# Plotando Árvore de decisão
summary(tree_model4)
plot(tree_model4)
text(tree_model4)



# Normalização ####

scaler <- preProcess(treino, 'range') #normalização entre 0 e 1

# Aplicação de parametros de normalização

# Base de treino
treino <- predict(scaler, treino)

# Base de teste
teste <- predict(scaler, teste)


# Acurácia do modelo após anormalização ####

# Treinando o modelo
tree_model5 <- tree(outcome~., treino)

# PrevisÃ£o utilizando a base de teste
predictionsDtree5 <- predict(tree_model5,
                             teste,
                             type = 'class')

# Matriz de confusão
table5 <- table(predictionsDtree5,
                teste$outcome); table5

# Medindo acurácia
acuracy5 <- 1 - mean(predictionsDtree5
                     != teste$outcome); acuracy5 #80%

# Plotando Árvore de decisão
summary(tree_model5)
plot(tree_model5)
text(tree_model5)



# Balanceamento das bases ####

treino_sem_balanceamento = treino

table(treino$outcome)
table(treino_sem_balanceamento$outcome)



# Calculando a % dos valores de cada outcome e fazendo um gráfico de Pizza

# Calculando a % dos valores do outcome:

# Contador
q_died = 0
q_lived = 0
q_euthanized = 0

# Loop para a contagem
for (linha in 1:nrow(treino))
{
  if(treino[linha,19] == 'died')
  {
    q_died = q_died + +1
  }
  if(treino[linha,19] == 'euthanized')
  {
    q_euthanized = q_euthanized +1
  }
  if (treino[linha,19] == 'lived')
  {
    q_lived = q_lived +1
  }
}

# % dos valores da contagem acima
p_died = (q_died/nrow(treino))*100
p_lived = (q_lived/nrow(treino))*100
p_euthanized = (q_euthanized/nrow(treino))*100

# Prova real:
prova_real = p_died + p_lived + p_euthanized
prova_real == 100

porcentagem_outcome_treino = data.frame(tipo = c('died', 'lived', 'euthanized'),
                                        valor = c(p_died, p_lived, p_euthanized))

# Fazendo um gráfico de Pizza:

porc = data.frame(valor = c(p_died, p_lived, p_euthanized),
                  tipo = c('died','lived', 'euthanized')) %>% 
  mutate(tipo = factor(tipo, levels = c("euthanized", "lived", "died")),
         cumulative = cumsum(valor),
         midpoint = cumulative - valor / 2,
         label = paste0(tipo, " ", round(valor / sum(valor) * 100, 1), "%"))

ggplot(porc, aes(x = 1, weight = valor, fill = tipo)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label))

# Fazendo gráfico de barras:

balancemaento = ggplot(data = treino) 
balancemaento + geom_bar(aes(x = outcome, fill = outcome)) 


# Temos mais dados com cavalos vivos ("lived" = 178), do que mortos ("died" = 77)
# e que foram submetidos a eutásia ("euthanized" = 44)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#
#                                                                                  #
# A biblioteca que faz SMOTE não trabalha com duas classes (Ex: vivo e morto)      #
# A base de dados tem 3 classes:                                                   #
#                               Vivo                                               #
#                               Morto                                              #
#                               Eutanásia                                          #
#                                                                                  #
# Protanto, para fazer o SMOTE, será preciso dividir a base de acordo com as       #
# classes de outcome.                                                              #
# Teremos então as bases:                                                          #
#                   df_Vivo_(1) [Vivo "original"]                                  #
#                   df_Morto                                                       #
#                   df_Eutanasia                                                   #
#                                                                                  #
# Posteriormente, as bases serão correlacionadas em duplas, juntando a classe      #
# dominante (vivo) com as classes minotitárias (morto e eutanásia).                #
# Para o tratamento do SMOTE teremos:                                              #
#                  Vivo + Morto_SOTE                                               #
#                  Vivo + Eutanásia_SMOTE                                          #
#                                                                                  #
# Depois, os resultados (valores gerados para Morto_SMOTE e Eutanásia_SMOTE)       #
# serão separados da classe "Vivo".Teremos:                                        #
#                  Morto-SMOTE                                                     #
#                  Eutanásia_SMOTE                                                 #
#                                                                                  #
# Por fim, as novas bases (morto_SMOTE e eutanÃ¡sia_SMOTE) serão adicionadas com a # 
# base "Vivo_(1)" original.                                                        #
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#

# Separando dados

# "Vivo" - Pega os dados em que a coluna outcome é igual a vivo
df_vivo <- data.frame(subset(treino,
                             outcome == "lived"))

# Dropando os níveis de fatores inexistentes (ao executar: df_vivo[,19])
# os níveis morto e euthanized ficam como 0)
df_vivo[,19] <- droplevels(df_vivo[,19]) 

# "Morto"
df_morto <- data.frame(subset(treino,
                              outcome == "died"))

# Dropando os níveis de fatores inexistentes
df_morto[,19] <- droplevels(df_morto[,19]) 

# "Eutanásia"
df_euthanized <- data.frame(subset(treino,
                                   outcome == "euthanized"))

# Dropando os níveis de fatores inexistentes
df_euthanized[,19] <- droplevels(df_euthanized[,19]) 


# Juntando dados:

# Vivo + Morto
vivo_morto <- merge(df_vivo,
                    df_morto, all = TRUE)

# verificando os níveis dos fatores
vivo_morto[,19]

# Vivo + euthanized
vivo_euthanized <- merge(df_vivo,
                         df_euthanized, all = TRUE)

# verificando os níveis dos fatores
vivo_euthanized[,19]


# SMOTE ####

# Level Died

# Pegando a quantidade de vivo e morto
registros_por_classe_morto <- table(vivo_morto[,19])
vivo_m <- registros_por_classe_morto[1]
morto <- registros_por_classe_morto[2]

# Calculando o SMOTE (dobrando o nÃºmero de morto)
percent_over <- 100 # 200 ou 1306
percent_under <- vivo_m*100 / ((percent_over/100)*(morto))

vivo_morto <- SMOTE(outcome~.,
                    vivo_morto,
                    perc.over = percent_over,
                    perc.under = percent_under)


# Level Euthanized

# Pegando a quantidade de vivo e euthanized
registros_por_classe_eutanasia <- table(vivo_euthanized[,19])
vivo_e <- registros_por_classe_eutanasia[1]
eutanasia <- registros_por_classe_eutanasia[2]

# Fazendo o SMOTE triplicando o euthanized
percent_over <- 200 # 200 ou 1306
percent_under <- vivo_e*100 / ((percent_over/100)*eutanasia)

vivo_euthanized <- SMOTE(outcome~.,
                         vivo_euthanized,
                         perc.over = percent_over,
                         perc.under = percent_under)

# Separando os valores criados pelo SMOTE

# Separar morto balanceado
df_morto_bal <- data.frame(subset(vivo_morto,
                                  outcome == "died"))

# dropando os fatores inexistentes
df_morto_bal[,19] <- droplevels(df_morto_bal[,19]) 


# Separar eutanásia balanceado
df_euthanized_bal <- data.frame(subset(vivo_euthanized,
                                       outcome == "euthanized"))
# dropando os fatores inexistentes
df_euthanized_bal[,19] <- droplevels(df_euthanized_bal[,19]) 

# Juntando as bases
  # Obs: Precisa juntar as bases nessa ordem (morto -> eutanasia -> vivo) para manter a
# configuração original da ordem dos fatores anteriores, caso não esteja nessa ordem, na
# hora de treinar o modelo com a base de teste, irá cusar erro nos resultados (inclusive
# não será possível calcular o kappa), após juntar nessa ordem, a base fica coreta.
treino <- rbind(df_morto_bal,
                df_euthanized_bal,
                df_vivo)


# Bases

# Treino com balancemaneto:
table(treino$outcome)

# Treino sem balancemaneto:
table(treino_sem_balanceamento$outcome)

# Gráfico de Pizza com as bases balanceadas ####

q_died = 0
q_lived = 0
q_euthanized = 0

# Loop para a contagem
for (linha in 1:nrow(treino))
{
  if(treino[linha,19] == 'died')
  {
    q_died = q_died + +1
  }
  if(treino[linha,19] == 'euthanized')
  {
    q_euthanized = q_euthanized +1
  }
  if (treino[linha,19] == 'lived')
  {
    q_lived = q_lived +1
  }
}

# % dos valores da contagem acima
p_died = (q_died/nrow(treino))*100
p_lived = (q_lived/nrow(treino))*100
p_euthanized = (q_euthanized/nrow(treino))*100

# Prova real:
prova_real = p_died + p_lived + p_euthanized
prova_real == 100

porcentagem_outcome_treino = data.frame(tipo = c('died', 'lived', 'euthanized'),
                                        valor = c(p_died, p_lived, p_euthanized))

# Fazendo um gráfico de Pizza:

porc = data.frame(valor = c(p_died, p_lived, p_euthanized),
                  tipo = c('died','lived', 'euthanized')) %>% 
  mutate(tipo = factor(tipo, levels = c("euthanized", "lived", "died")),
         cumulative = cumsum(valor),
         midpoint = cumulative - valor / 2,
         label = paste0(tipo, " ", round(valor / sum(valor) * 100, 1), "%"))

ggplot(porc, aes(x = 1, weight = valor, fill = tipo)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label))

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#  Treinando modelo SEM balanceamento ####
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Níveis base de teste:
table(teste$outcome)

# Árvore de decisão SEM balanceamento ####

# Treinando o modelo
tree_model_normal <- tree(outcome~.,
                          treino_sem_balanceamento)

# Previsão na base de teste
predictionsDtree_normal <- predict(tree_model_normal,
                                   teste, type = 'class')


# Matriz de confusão
table_tree_normal <- table(predictionsDtree_normal,
                      teste$outcome); table_tree_normal



# Medindo acurácia do modelo
acuracy_tree_normal <- 1 - mean(predictionsDtree_normal
                           != teste$outcome); acuracy_tree_normal #77%

# Kappa:
Kappa_Tree_normal = confusionMatrix(table_tree_normal)$overall[2]; Kappa_Tree_normal

# Info
dados_tree_normal = data.frame(acuracy_tree_normal,
                    unname(Kappa_Tree_normal)); dados_tree_normal

# Tabela Info
colnames(dados_tree_normal) <- c("Acuracia Tree Normal",
                                "Kappa Tree Normal"); dados_tree_normal

# Plotando árvore de decisão
summary(tree_model_normal)
plot(tree_model_normal)
text(tree_model_normal)


# SVM do modelo SEM balanceamento ####

# Treinando modelo
svm_model_normal <- svm(outcome ~.,
                        treino_sem_balanceamento, probability =T)   #o ~. define como imput todas as outras colunas alÃ©m de class

# Previsão na base de teste
predictionsSVM_normal <- predict(svm_model_normal,
                                 teste, probability =T)

# Matriz de confusão
tableSVM_normal <- table(predictionsSVM_normal,
                         teste$outcome); tableSVM_normal

# Medindo acurácia do modelo
acuracySVM_normal <-  1 - mean(predictionsSVM_normal
                               != teste$outcome); acuracySVM_normal #78%

# Kappa:
Kappa_SVM_normal = confusionMatrix(tableSVM_normal)$overall[2]; Kappa_SVM_normal

# Info
dados_SVM_normal = data.frame(acuracySVM_normal,
                              unname(Kappa_SVM_normal)); dados_SVM_normal

# Tabela Info
colnames(dados_SVM_normal) <- c("Acuracia SVM Normal",
                                "Kappa SVM Normal"); dados_SVM_normal

# Plotando sumário
summary(svm_model_normal)

# Plotando tabela de probabilidades
probabilidades_normal <- attr(predictionsSVM_normal,
                              "probabilities")

predictionsAndProbabilities_normal <- cbind(teste$outcome,
                                            predictionsSVM_normal,
                                            probabilidades_normal)
View(predictionsAndProbabilities_normal)


# Random Forest do modelo SEM balanceamento ####

# Treinando o modelo
forest_model_normal <- randomForest(outcome ~.,
                                    data = treino_sem_balanceamento,
                                    importance = TRUE,
                                    do.trace = 100)

# Previsão na base de teste
predictionsForest_normal <- predict(forest_model_normal,
                                    teste)

# Matriz de confusão
tableRForest_normal <- table(predictionsForest_normal,
                             teste$outcome); tableRForest_normal

# Medindo acurácia do modelo
acuracyRForest_normal <- 1 - mean(predictionsForest_normal
                                  != teste$outcome); acuracyRForest_normal #100%

# Kappa:
Kappa_Forest_normal = confusionMatrix(tableRForest_normal)$overall[2]; Kappa_Forest_normal

# Info
dados_Forest_normal = data.frame(acuracyRForest_normal,
                                 unname(Kappa_Forest_normal)); dados_Forest_normal

# Tabela Info
colnames(dados_Forest_normal) <- c("Acuracia FOREST Normal",
                                   "Kappa FOREST Normal"); dados_Forest_normal

# Plotando gráfico OOB
plot(forest_model_normal)
legend("topright",
       legend=c("OOB",
                "0",
                "1"),
       col=c("black",
             "red",
             "green"),
       lty=1:1,
       cex=0.8)

# lty = line type, cex = character expansion factor

# Duas medidas de importância para ranquear os atributos
varImpPlot(forest_model_normal)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Treinando modelo COM balanceamento ####
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Árvore de Decisão COM balanceamento ####

# Treinando o modelo
tree_model_smote <- tree(outcome~.,
                         treino)

# Previsão com a base de treino
predictionsDtree_smote <- predict(tree_model_smote,
                                  teste,
                                  type = 'class')

# Matriz de confusão
table_tree_smote <- table(predictionsDtree_smote,
                           teste$outcome); table_tree_smote

# Medindo acurácia
acuracy_tree_smote <- 1 - mean(predictionsDtree_smote
                               != teste$outcome); acuracy_tree_smote #80%


# Kappa:
Kappa_Tree_Smote = confusionMatrix(table_tree_smote)$overall[2]; Kappa_Tree_Smote

# Info
dados_Tree_Smote = data.frame(acuracy_tree_smote,
                              unname(Kappa_Tree_Smote)); dados_Tree_Smote

# Tabela Info
colnames(dados_Tree_Smote) <- c("Acuracia TREE Smote",
                                "Kappa TREE Smote"); dados_Tree_Smote

# Plotando Árvore de decisão
summary(tree_model_smote)
plot(tree_model_smote)
text(tree_model_smote)



# SVM no modelo COM balanceamento ####

# Treinando o modelo
svm_model_smote <- svm(outcome ~.,
                       treino,
                       probability =T)   #o ~. define como imput todas as outras colunas além de class

# Previsão com a base de teste
predictionsSVM_smote <- predict(svm_model_smote,
                                teste,
                                probability =T)

# Matriz de confusão
tableSVM_smote <- table(predictionsSVM_smote,
                        teste$outcome); tableSVM_smote

# Medindo acurÃ¡cia do modelo
acuracySVM_smote <-  1 - mean(predictionsSVM_smote
                              != teste$outcome); acuracySVM_smote #82%

# Kappa:
Kappa_SVM_Smote = confusionMatrix(tableSVM_smote)$overall[2]; Kappa_SVM_Smote

# Info
dados_SVM_Smote = data.frame(acuracySVM_smote,
                             unname(Kappa_SVM_Smote)); dados_SVM_Smote

# Tabela Info
colnames(dados_SVM_Smote) <- c("Acuracia SVM Smote",
                               "Kappa SVM Smote"); dados_SVM_Smote

# Plotando sumário
summary(svm_model_smote)

# Plotando tabela de probabilidades
probabilidades_smote <- attr(predictionsSVM_smote,
                             "probabilities")

predictionsAndProbabilities_smote <- cbind(teste$outcome,
                                           predictionsSVM_smote,
                                           probabilidades_smote)
View(predictionsAndProbabilities_smote)


# Random Forest no modelo COM balanceamento ####

#Treinando modelo
forest_model_smote <- randomForest(outcome ~.,
                                   data = treino,
                                   importance = TRUE,
                                   do.trace = 100)

# Previsão na base de teste
predictionsForest_smote <- predict(forest_model_smote,
                                   teste)

# Matriz de confusão
tableRForest_smote <- table(predictionsForest_smote,
                            teste$outcome); tableRForest_smote

# Medindo acurácia
acuracyRForest_smote <- 1 - mean(predictionsForest_smote
                                 != teste$outcome); acuracyRForest_smote #100%

# Kappa:
Kappa_FOREST_Smote = confusionMatrix(tableRForest_smote)$overall[2]; Kappa_FOREST_Smote

# Info
dados_FOREST_Smote = data.frame(acuracyRForest_smote,
                             unname(Kappa_FOREST_Smote)); dados_FOREST_Smote

# Tabela Info
colnames(dados_FOREST_Smote) <- c("Acuracia Forest Smote",
                                  "Kappa Forest Smote"); dados_FOREST_Smote

# Plotando gráfico para OOB
plot(forest_model_smote)

legend("topright",
       legend=c("OOB",
                "0",
                "1"),
       col=c("black",
             "red",
             "green"),
       lty=1:1,
       cex=0.8)

#lty = line type, cex = character expansion factor

# Duas medidas de importância para ranquear os atributos
varImpPlot(forest_model_smote)


# OBS: O modelo sem a aplicação do SMOTE performou melhor ####



# Aplicando Dummy Variables em treino ####


treino_dummy <- treino[,-19] %>%
  dummy_cols(select_columns = NULL,
             remove_first_dummy = TRUE) %>%
  select(-age,
         -surgery,
         -temp_of_extremities,
         -peripheral_pulse,
         -mucous_membrane,
         -capillary_refill_time,
         -pain, -peristalsis,
         -abdominal_distention,
         -nasogastric_tube,
         -nasogastric_reflux,
         -rectal_exam_feces,
         -abdomen,
         -surgical_lesion) %>%
  cbind(treino['outcome'])



# Aplicando Dummy Variables em teste ####


teste_dummy <- teste[,-19] %>%
  dummy_cols(select_columns = NULL,
             remove_first_dummy = TRUE) %>%
  select(-age,
         -surgery,
         -temp_of_extremities,
         -peripheral_pulse,
         -mucous_membrane,
         -capillary_refill_time,
         -pain, -peristalsis,
         -abdominal_distention,
         -nasogastric_tube,
         -nasogastric_reflux,
         -rectal_exam_feces,
         -abdomen,
         -surgical_lesion) %>%
  cbind(teste['outcome'])

# Número de Colunas da base de treino após a palicação da dummy_variables
ncol(teste_dummy) # 43 variáveis, sendo 42 numéricas  e 1 categórica (outcome)

# PCA ####

pcatreino <- treino_dummy[,-43]
pcateste <- teste_dummy[,-43]

# Aplicação do PCA
pcaParams <- preProcess(pcatreino,                                                 
                        method="pca",                                              
                        thresh = 0.95)                                             



print(pcaParams) # dos 42 componentes principais, apenas 32 explicam 95% da variância dos dados

pcatreino <- predict(pcaParams,
                     pcatreino)

pcateste <- predict(pcaParams,
                    pcateste)

# Observando a vaiância acumulada:
vars = pcaParams$std^2
vars = vars/sum(vars)
cumulativeVariance = cumsum(vars)
View(cumulativeVariance)


#adicionar a coluna de classe
pcatreino <- cbind(pcatreino,
                   treino['outcome'])

pcateste <- cbind(pcateste,
                  teste['outcome'])

# Plotando sumário
summary(pcatreino)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Treinando modelos após PCA ####
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Árvore de decisão com PCA ####

# Treinando modelo
tree_model_PCA <- tree(outcome~.,
                       pcatreino)

# Previsão na base pcateste
predictionsDtree_PCA <- predict(tree_model_PCA,
                                pcateste,
                                type = 'class')

# Matriz de confusão
table_TREE_PCA <- table(predictionsDtree_PCA,
                        pcateste$outcome); table_TREE_PCA

# Medindo acurácia do modelo

acuracy_TREE_PCA <- 1 - mean(predictionsDtree_PCA != pcateste$outcome); acuracy_TREE_PCA #86% (deu 79 com o meu código!)

# Kappa:
Kappa_TREE_PCA = confusionMatrix(table_TREE_PCA)$overall[2]; Kappa_TREE_PCA

# Info
dados_TREE_PCA = data.frame(acuracy_TREE_PCA,
                            unname(Kappa_TREE_PCA)); dados_TREE_PCA

# Tabela Info
colnames(dados_TREE_PCA) <- c("Acuracia TREE PCA",
                               "Kappa TREE PCA"); dados_TREE_PCA

# Plotando Árvore de decisão
summary(tree_model_PCA)
plot(tree_model_PCA)
text(tree_model_PCA)


# SVM com PCA ####

# Treinando modelo
svm_model_PCA <- svm(outcome ~.,
                     pcatreino,
                     probability =T)   #o ~. define como imput todas as outras colunas alÃ©m de class

# Previsão na base pcateste
predictionsSVM_PCA <- predict(svm_model_PCA,
                              pcateste,
                              probability =T)

# Matriz de confusão
tableSVM_PCA <- table(predictionsSVM_PCA,pcateste$outcome); tableSVM_PCA

# Medindo acurácia do modelo
acuracySVM_PCA <-  1 - mean(predictionsSVM_PCA != pcateste$outcome); acuracySVM_PCA #95%

# Kappa:
Kappa_SVM_PCA = confusionMatrix(tableSVM_PCA)$overall[2]; Kappa_SVM_PCA

# Info
dados_SVM_PCA = data.frame(acuracySVM_PCA,
                            unname(Kappa_SVM_PCA)); dados_SVM_PCA

# Tabela Info
colnames(dados_SVM_PCA) <- c("Acuracia SVM PCA",
                              "Kappa SVM PCA"); dados_SVM_PCA

# Plotando sumário
summary(svm_model_PCA)

# Plotando tabela de probabilidades
probabilidades_PCA <- attr(predictionsSVM_PCA,
                           "probabilities")

predictionsAndProbabilities_PCA <- cbind(pcateste$outcome,
                                         predictionsSVM_PCA,
                                         probabilidades_PCA)
View(predictionsAndProbabilities_PCA)


# Random Forest com PCA ####

# Treinando modelo
forest_model_PCA <- randomForest(outcome ~.,
                                 data = pcatreino,
                                 importance = TRUE,
                                 do.trace = 100)

# Previsão na base pcateste
predictionsForest_PCA <- predict(forest_model_PCA,
                                 pcateste)

# Matriz de confusão
tableRForest_PCA <- table(predictionsForest_PCA,
                          pcateste$outcome); tableRForest_PCA

# Medindo acurácia do modelo
acuracyRForest_PCA <- 1 - mean(predictionsForest_PCA != pcateste$outcome); acuracyRForest_PCA #100%

# Kappa:
Kappa_FOREST_PCA = confusionMatrix(tableRForest_PCA)$overall[2]; Kappa_FOREST_PCA

# Info
dados_FOREST_PCA = data.frame(acuracyRForest_PCA,
                           unname(Kappa_FOREST_PCA)); dados_FOREST_PCA

# Tabela Info
colnames(dados_FOREST_PCA) <- c("Acuracia FOREST PCA",
                                "Kappa FOREST PCA"); dados_FOREST_PCA

# Plotando gráfico OOB
plot(forest_model_PCA)

legend("topright",
       legend=c("OOB",
                "0",
                "1"),
       col=c("black",
             "red",
             "green"),
       lty=1:1, cex=0.8)

#lty = line type, cex = character expansion factor

# Com o PCA não fica muito claro quais são os atributos mais significativos para o modelo
varImpPlot(forest_model_PCA)

# Modelo Treinado:

# Modelo Final: ####

modelo_final <- randomForest(outcome ~.,
                            data = treino,
                            importance = TRUE,
                            do.trace = 100,
                            ntree = 400)

# Previsão na base de teste
previsao_final <- predict(modelo_final,
                          teste)

# Matriz de confusão
c_matrix_final <- table(previsao_final,
                        teste$outcome); c_matrix_final

# Medindo acurácia
final_acuracy <- 1 - mean(previsao_final
                          != teste$outcome); final_acuracy #100%

# Kappa:
final_Kappa = confusionMatrix(c_matrix_final)$overall[2]; final_Kappa

# Info
dados_final = data.frame(final_acuracy,
                        unname(final_Kappa)); dados_final

# Tabela Info
colnames(dados_final) <- c("Acuracia",
                            "Kappa"); dados_final

# Plotando gráfico para OOB
plot(modelo_final)

legend("topright",
       legend=c("OOB",
                "0",
                "1"),
       col=c("black",
             "red",
             "green"),
       lty=1:1, cex=0.8)

#lty = line type, cex = character expansion factor

# Com o PCA não fica muito claro quais são os atributos mais significativos para o modelo
varImpPlot(modelo_final)

# Salvando o Modelo Final:

# save(modelo_final, file = 'horese_predict')


