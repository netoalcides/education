########## Exemplo Mnist ##########

## pacotes ##
library(tidyverse)
library(magrittr)
library(reticulate)
use_python('/usr/local/bin/python3.6')
library(keras)


## dados ##
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

str(train_images)
str(train_labels)

# eixos (ordem) do tensor

train_images %>% 
  dim %>% 
  length

# formato
train_images %>% 
  dim

# tipo do dados
train_images %>% 
  typeof()

# vendo as imagens
im <- 3

train_images[im,,]

train_images[im,,] %>% 
  as.raster( max = max(train_images[im,,]) ) %>% 
  plot()

# criando um modelo

network <- keras_model_sequential() %>%
  layer_dense( units = 512, 
               activation = "relu", 
               input_shape = c(28 * 28) ) %>%
  layer_dense( units = 10, 
               activation = "softmax" )

# metodo de treinamento
network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# preparacao dos dados - normalizar min 0, max 1

train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255
train_images %>% dim

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255
test_images %>% dim

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

# treino modelo - vai demorar um pouco

network %>% fit(train_images, 
                train_labels, 
                epochs = 5, 
                batch_size = 128)

# avaliacao
network %>% evaluate(test_images, test_labels)

# previsao
classes_pred <- network %>% 
  predict_classes(test_images[1:10000,])

im <- 1

classes_pred[im]

mnist$test$x[im,,] %>% 
  as.raster( max = max(mnist$test$x[im,,]) ) %>% 
  plot()

# confusion matrix
data_frame( real = mnist$test$y,
            previsto = classes_pred ) %>% 
  count( real, previsto ) %>% 
  spread( key = real, value = n, fill = 0 )


# usando imagem real
imagem <- image_load(path = "dados/teste_6_2.png",
                     grayscale = FALSE,
                     target_size = c(28, 28) )

imagem <- image_to_array( imagem )

imagem <- imagem[,,1]

imagem %>% 
  as.raster( max = max(imagem) ) %>% 
  plot()

imagem_teste <- array_reshape(imagem, c(1, 28 * 28))

imagem_teste <- imagem_teste / max(imagem_teste)

network %>% 
  predict_classes(imagem_teste)




