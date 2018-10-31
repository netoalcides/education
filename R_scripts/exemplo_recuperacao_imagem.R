########## Exemplo Recuperacao de Imagens ##########

## pacotes ##
library(tidyverse)
library(magrittr)
library(reticulate)
use_python('/usr/local/bin/python3.6')
library(keras)
library(Matrix)
library(NMF)
library(NNLM)

## carrega imagem ##
imagem <- image_load( path = 'dados/scallet.jpg' ) %>% 
  image_to_array(., data_format = "channels_first" )

# dimensao
imagem %>% 
  dim

# camadas
imagem[1,,] %>% 
  as.raster( max = max(imagem, na.rm = TRUE) ) %>% 
  plot()

imagem[2,,] %>% 
  as.raster( max = max(imagem, na.rm = TRUE) ) %>% 
  plot()

imagem[3,,] %>% 
  as.raster( max = max(imagem, na.rm = TRUE) ) %>% 
  plot()


## insere missings ##

camada <- 2
imagem_problema <- imagem[camada,,]

nr <- nrow( imagem_problema ) # numero de linhas
nc <- ncol( imagem_problema ) # numero de colunas

p <- 0.3 # proporcao de NA's

ina <- is.na( unlist(imagem_problema) ) # ajusta as posicoes dos NA's caso ja exista algum
n2 <- floor( p*nr*nc ) - sum( ina )  # determina local dos novos NA's

ina[ sample(which(!is.na(ina)), n2) ] <- TRUE # ajusta onde nao tem NA, caso ja exista algum
imagem_problema[matrix(ina, nr=nr,nc=nc)] <- NA # insere os NA's

imagem_problema %>% 
  as.raster( max = max(imagem, na.rm = TRUE) ) %>% 
  plot()

## Testa modelo

for( i in 1:30 ){
  
  show( paste0("Etapa: ", i) )
  modelo <- nnmf(imagem_problema, 
                 k = i,
                 method = 'scd', 
                 loss = 'mse', 
                 n.threads = 0, 
                 max.iter =  1000 )
  
  imagem_recuperada <- modelo$W %*% modelo$H
  
  imagem_recuperada %>% 
    as.raster( max = max(imagem_recuperada, na.rm = TRUE) ) %>% 
    plot() %>% 
    title( main = paste0('teste ', i) ) %>% 
    show()
  
}

# Compara o problema com o ajustado

par( mfrow=c(1,3) )

imagem[camada,,] %>% 
  as.raster( max = max(imagem, na.rm = TRUE) ) %>% 
  plot() %>% 
  title( main = 'Original' )

imagem_problema %>% 
  as.raster( max = max(imagem_problema, na.rm = TRUE) ) %>% 
  plot() %>% 
  title( main = 'Problemas' )

imagem_recuperada %>% 
  as.raster( max = max(imagem_recuperada, na.rm = TRUE) ) %>% 
  plot() %>% 
  title( main = 'Recuperada' )



