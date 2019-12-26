# Carregar pacotes
library(tidyverse)
library(magrittr)
library(rpart)
library(caret)

# Carregar dados
dados_credito <- read_csv( file = "/home/vm-data-science/dados/dados_credito_small.csv" )
dados_credito %>% head

# amostra de treino
set.seed(201)
dados_credito_train <- dados_credito %>% sample_frac(., 0.6)
dados_credito_train %>% dim

# amostra de teste
dados_credito_test <- dados_credito %>% 
  filter( !id %in% dados_credito_train$id )

dados_credito_test %>% dim

# treinar modelo
modelo_arvore_classificacao <- rpart( default ~ educacao_cat + idade_cat + renda_cat, 
                                      data = dados_credito_train %>% select( -id ),
                                      parms = list( split = "gini") )

# previsao
dados_credito_test %<>% 
  mutate( default_pred = predict( modelo_arvore_classificacao, 
                                  dados_credito_test,
                                  type = 'class' ) )

# teste
confusionMatrix(as.factor(dados_credito_test$default), 
                dados_credito_test$default_pred,
                positive = 'pagou')







