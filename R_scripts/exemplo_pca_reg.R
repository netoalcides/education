library(tidyverse)
library(magrittr)
library(psych)
library(plotly)
library(anomalyDetection)
library(bootStepAIC)
library(glmnet)

rmse <- function( real, prediction ){ sqrt( mean( (real - prediction)^2 ) ) }

load( "nerd_project2/data/dados_nerd.RData" )

dados_nerd_train %<>% 
  dplyr::select( nerdy, contains('Q'), education, gender, married, familysize, ASD ) %>% 
  mutate_all( funs( ifelse( . == 0, NA, . ) ) )

dados_nerd_test %<>% 
  dplyr::select( nerdy, contains('Q'), education, gender, married, familysize, ASD ) %>% 
  mutate_all( funs( ifelse( . == 0, NA, . ) ) )


# missings por id
dados_nerd_train %>% 
  mutate( id = row_number() ) %>% 
  group_by(id) %>% 
  mutate_all( funs( ifelse( is.na(.) == TRUE, 'NA', 'Existe') ) ) %>% 
  gather( key = variaveis, value = valores, -id) %>% 
  dplyr::select( id, valores ) %>% 
  count( valores ) %>% 
  rename( variaveis_completas = n ) %>% 
  mutate( variaveis_no_dataset = dim(dados_nerd_train)[2],
          freq = variaveis_completas/ sum(variaveis_completas) ) %>% 
  filter( valores == 'Existe' ) %>% 
  filter( freq < 1 ) %>% 
  dplyr::select( id, variaveis_no_dataset, variaveis_completas, freq) %>% 
  arrange( freq ) %>% 
  head(10)



# por coluna
dados_nerd_train %>% 
  mutate_all( funs( ifelse( is.na(.) == TRUE, 0, 1 ) ) ) %>% 
  gather( key = variaveis, value = linhas_completas ) %>% 
  group_by( variaveis ) %>% 
  summarise( total_linhas = n(), 
             total_linhas_completas = sum(linhas_completas),
             perc_linhas_completas = total_linhas_completas / total_linhas,
             perc_linhas_incompletas = 1 - perc_linhas_completas ) %>% 
  arrange( perc_linhas_completas )


dados_nerd_train %>% 
  dplyr::select( contains('Q'), gender, education, married, ASD ) %>% 
  map( ~count(data.frame(x=.x), x) )


# tratar os missings

dados_nerd_train %<>% 
  na.omit()

dados_nerd_test %<>% 
  na.omit()

# save( dados_nerd_train, dados_nerd_test, file = 'dados/dados_nerd_nomissings.RData')


# outliers

dados_nerd_train %>% 
  mutate( id = row_number(),
          mahala_dist = mahalanobis_distance( dplyr::select(dados_nerd_train,
                                                     contains('Q')) ) ) %>% 
  plot_ly( x = ~id,
           y = ~mahala_dist,
           type = 'scatter',
           mode = 'markers',
           width = 800 )


dados_nerd_train %>%
  mutate( id = row_number() ) %>% 
  filter( id == 527 ) %>% 
  data.frame

dados_nerd_train %>%
  mutate( id = row_number() ) %>% 
  filter( id == 524 ) %>% 
  data.frame


dados_nerd_train %>% 
  dplyr::select( contains('Q') ) %>% 
  gather( key = variaveis, value = notas ) %>% 
  plot_ly( x = ~variaveis,
           y = ~notas,
           type = 'box' )


# Exploratorio var depedente

dados_nerd_train %>% 
  ggplot( aes( x = nerdy ) ) +
  geom_histogram()


# etapas do PCA

correlacao <- dados_nerd_train %>% 
  dplyr::select( contains('Q') ) %>% 
  cor() %>% 
  round(., 2)

correlacao

plot_ly( z = correlacao,
         x = colnames(correlacao),
         y = colnames(correlacao), 
         colorscale = "Greys", 
         type = "heatmap" )

pca_model <- principal(dados_nerd_train %>% 
                         dplyr::select( contains('Q') ), 
                       nfactors = 26, 
                       rotate = "none" )

pca_model$Vaccounted %>% 
  as.data.frame() %>% 
  rownames_to_column( var = 'medidas' ) %>% 
  gather( key = PC, value = valores, -medidas ) %>% 
  spread( key = medidas, value = valores ) %>% 
  arrange( desc(`SS loadings`) )


pca_model$values %>% 
  as_data_frame %>% 
  rename( autovalor = value ) %>% 
  mutate( dimensao = 1:26 ) %>% 
  plot_ly(x = ~dimensao,
          y = ~autovalor,
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 10, color = 'red') )

# adicionar no banco

scores_pca <- pca_model$scores[, 1:9] %>% 
  as_data_frame()

dados_nerd_train_modelo <- dados_nerd_train %>% 
  mutate( gender = as.factor(gender),
          education = as.factor(education),
          married = as.factor(married),
          ASD = as.factor(ASD) ) %>% 
  bind_cols(., scores_pca )

# modelo regressao

modelo_reg_questoes <- lm( formula = nerdy ~ ., 
    data = dados_nerd_train_modelo %>% dplyr::select( nerdy, contains('Q'), familysize, gender, education, married, ASD ) )

summary( modelo_reg_questoes )

modelo_reg_pca <- lm( formula = nerdy ~ ., 
                           data = dados_nerd_train_modelo %>% dplyr::select( nerdy, contains('PC'), familysize, gender, education, married, ASD ) )

summary( modelo_reg_pca )

modelo_reg_questoes_stepwise <- boot.stepAIC(modelo_reg_questoes, 
                                            data = dados_nerd_train_modelo, 
                                            B = 10, 
                                            direction = "both", 
                                            verbose = TRUE )

summary( modelo_reg_questoes_stepwise$OrigStepAIC )


modelo_reg_pca_stepwise <- boot.stepAIC(modelo_reg_pca, 
                                       data = dados_nerd_train_modelo, 
                                       B = 10, 
                                       direction = "both", 
                                       verbose = TRUE )

summary( modelo_reg_pca_stepwise$OrigStepAIC )


# bootstepAIC como funciona
# nrow( dados_nerd_train)
# 
# length(sample(993, 993 * 10, replace = TRUE))
# 
# zz <- sample(993, 993 * 10, replace = TRUE)
# 
# dim(zz) <- c(993, 10)
# 
# dados_nerd_train[ zz[,1], ]


# criar os scores na amostra de teste

scores_teste <- predict( pca_model, 
         dados_nerd_test %>% 
           dplyr::select( contains('Q') ) ) %>% 
  tbl_df()


dados_nerd_test_modelo <- dados_nerd_test %>% 
  mutate( gender = as.factor(gender),
          education = as.factor(education),
          married = as.factor(married),
          ASD = as.factor(ASD) ) %>% 
  bind_cols(., scores_teste[, 1:9] )


dados_nerd_test_modelo %>% 
  mutate( pred_nerdy_reg_questoes = predict( modelo_reg_questoes, . ),
          pred_nerdy_reg_pca = predict( modelo_reg_pca, . ),
          pred_nerdy_reg_questoes_stepwise = predict( modelo_reg_questoes_stepwise$OrigStepAIC, . ),
          pred_nerdy_reg_pca_stepwise = predict( modelo_reg_pca_stepwise$OrigStepAIC, . ) ) %>%
  summarise( rmse_pred_nerdy_reg_questoes = rmse(nerdy, pred_nerdy_reg_questoes),
             rmse_pred_nerdy_reg_pca = rmse(nerdy, pred_nerdy_reg_pca),
             rmse_pred_nerdy_reg_questoes_stepwise = rmse(nerdy, pred_nerdy_reg_questoes_stepwise),
             rmse_pred_nerdy_reg_pca_stepwise = rmse(nerdy, pred_nerdy_reg_pca_stepwise) ) %>% 
  gather( key = modelos, value = rmse ) %>% 
  arrange( rmse )


library(rpart)

set.seed(543)
dados_nerd_train_modelo_train <- dados_nerd_train_modelo %>% 
  sample_frac(., 0.8)

dados_nerd_train_modelo_valid <- setdiff( dados_nerd_train_modelo, dados_nerd_train_modelo_train )


iteracoes <- 150 # numero de iteracoes para tunning
s_seeds <- sample(1000000:9999999, iteracoes) # sementes aleatorias
dados_amostra_avaliacao_questoes <- NULL
dados_amostra_avaliacao_pca <- NULL

for ( iter in 1:iteracoes ){
  
  set.seed( s_seeds[iter] )
  minsplit_ <- sample(10:30, 1)
  cp_ <- runif(1, 0.001, 0.1)
  maxcompete_ <- sample(3:30, 1)
  maxdepth_ <- sample(10:50, 1)
  
  modelo_arvore_questoes <- rpart( formula = nerdy ~ ., 
                          data = dados_nerd_train_modelo_train %>% 
                            dplyr::select( nerdy, contains('Q') ),
                          control = list( minsplit = minsplit_,
                                          cp = cp_,
                                          maxcompete = maxcompete_,
                                          maxdepth = maxdepth_ ) )
  
  modelo_arvore_pca <- rpart( formula = nerdy ~ ., 
                                   data = dados_nerd_train_modelo_train %>% 
                                     dplyr::select( nerdy, contains('PC') ),
                                   control = list( minsplit = minsplit_,
                                                   cp = cp_,
                                                   maxcompete = maxcompete_,
                                                   maxdepth = maxdepth_ ) )
  
  pred_tree_questoes <- predict( modelo_arvore_questoes, dados_nerd_train_modelo_valid )
  pred_tree_pca <- predict( modelo_arvore_pca, dados_nerd_train_modelo_valid )
  
  erro_questoes <- rmse( dados_nerd_train_modelo_valid$nerdy, pred_tree_questoes )
  erro_pca <- rmse( dados_nerd_train_modelo_valid$nerdy, pred_tree_pca )
  
  aval_questoes <- data_frame( seed = s_seeds[iter],
                      minsplit_ = minsplit_,
                      cp_ = cp_,
                      maxcompete_ = maxcompete_,
                      maxdepth_ = maxdepth_,
                      erro = erro_questoes )
  
  aval_pca <- data_frame( seed = s_seeds[iter],
                               minsplit_ = minsplit_,
                               cp_ = cp_,
                               maxcompete_ = maxcompete_,
                               maxdepth_ = maxdepth_,
                               erro = erro_pca )
  
  dados_amostra_avaliacao_questoes <- bind_rows( dados_amostra_avaliacao_questoes, aval_questoes )
  dados_amostra_avaliacao_pca <- bind_rows( dados_amostra_avaliacao_pca, aval_pca )
  
}


bests_questoes <- dados_amostra_avaliacao_questoes %>% 
  arrange( erro ) %>% 
  head(1)

bests_pca <- dados_amostra_avaliacao_pca %>% 
  arrange( erro ) %>% 
  head(1)



# treino o(s) melhores

modelo_arvore_1 <- rpart( formula = nerdy ~ ., 
                        data = dados_nerd_train_modelo_train %>% 
                          dplyr::select( nerdy, contains('Q') ),
                        control = list( minsplit = bests_questoes$minsplit_[1],
                                        cp = bests_questoes$cp_[1],
                                        maxcompete = bests_questoes$maxcompete_[1],
                                        maxdepth = bests_questoes$maxdepth_[1] ) )

modelo_arvore_2 <- rpart( formula = nerdy ~ ., 
                          data = dados_nerd_train_modelo_train %>% 
                            dplyr::select( nerdy, contains('PC') ),
                          control = list( minsplit = bests_pca$minsplit_[1],
                                          cp = bests_pca$cp_[1],
                                          maxcompete = bests_pca$maxcompete_[1],
                                          maxdepth = bests_pca$maxdepth_[1] ) )




dados_nerd_test_modelo %>% 
  mutate( pred_modelo_1 = predict( modelo_arvore_1, . ),
          pred_modelo_2 = predict( modelo_arvore_2, . ) ) %>%
  summarise( rmse_modelo_1 = rmse(nerdy, pred_modelo_1),
             rmse_modelo_2 = rmse(nerdy, pred_modelo_2) ) %>% 
  gather( key = modelos, value = rmse ) %>% 
  arrange( rmse )


###################################################################################
#### Cluster ####


padronizar <- function(x){ (x - min(x)) / (max(x) - min(x)) }

dados_nerd_train_padronizados <- dados_nerd_train %>% 
  dplyr::select( contains('Q') ) %>% 
  mutate_if( is.numeric, padronizar )

# cluster hierarquico
distancia <- dist( x = dados_nerd_train_padronizados, 
                   method =  'euclidian' )

modelo_cluster_hierarquico <- hclust( distancia,
                                      method = 'ward.D' )

plot(modelo_cluster_hierarquico)
rect.hclust( modelo_cluster_hierarquico, 
             k = 4, 
             border = "red" )

data_frame( grupos = 31:1 + 1,
            aglomeracao = modelo_cluster_hierarquico$height[872:902] ) %>% 
  plot_ly( x = ~grupos,
           y = ~aglomeracao,
           type = 'scatter',
           mode = 'lines+markers',
           marker = list(size = 10, color = 'red') ) %>% 
  layout( xaxis = list( autorange="reversed") )


# centroides
centroides <- dados_nerd_train_padronizados %>% 
  mutate( grupos = cutree(modelo_cluster_hierarquico, 
                          k = 4 ) ) %>% 
  group_by( grupos ) %>% 
  summarise_if( is.numeric, mean )


set.seed(123)
clusters_k4 <- kmeans(dados_nerd_train_padronizados , 
                      centers = centroides[-1] )



dados_nerd_train %<>% 
  mutate( grupos_final = as.factor(clusters_k4$cluster) )


dados_nerd_train %>% 
  dplyr::select( nerdy, Q1:Q5, grupos_final)


## treinamento


modelo_reg_questoes <- lm( formula = nerdy ~ ., 
                           data = dados_nerd_train %>% dplyr::select( nerdy, contains('Q') ) )

summary( modelo_reg_questoes )





modelo_reg_clus <- lm( formula = nerdy ~ ., 
                      data = dados_nerd_train %>% dplyr::select( nerdy, grupos_final ) )

summary( modelo_reg_clus )


modelo_reg_questclus <- lm( formula = nerdy ~ ., 
                       data = dados_nerd_train %>% dplyr::select( nerdy, contains('Q'), grupos_final ) )

summary( modelo_reg_questclus )

# teste
library(clue)

dados_nerd_test_padronizados <- dados_nerd_test %>% 
  dplyr::select( contains('Q') ) %>% 
  mutate_if( is.numeric, padronizar )

clusters_teste <- cl_predict( clusters_k4, 
            newdata = dados_nerd_test_padronizados )


dados_nerd_test %<>% 
  mutate( grupos_final = as.factor(clusters_teste) )


amostra_avalicao <- dados_nerd_test %>% 
  mutate( pred_nerdy_reg_quest = predict( modelo_reg_questoes, . ),
          pred_nerdy_reg_clus = predict( modelo_reg_clus, . ),
          pred_nerdy_reg_questclus = predict( modelo_reg_questclus, . ) ) %>% 
  dplyr::select( nerdy, pred_nerdy_reg_quest, pred_nerdy_reg_clus, pred_nerdy_reg_questclus)

amostra_avalicao %>%
  summarise( erro_modelo_1 = rmse(nerdy, pred_nerdy_reg_quest),
             erro_modelo_2 = rmse(nerdy, pred_nerdy_reg_clus),
             erro_modelo_3 = rmse(nerdy, pred_nerdy_reg_questclus) )







          