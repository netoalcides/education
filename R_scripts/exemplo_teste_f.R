#### Carregar pacotes ####
library(tidyverse)
library(magrittr)


#### Comparar 2 variancias ####

dados_salarios <- read_csv( file = 'education/dados/dados_salarios.csv' )

dados_salarios %>% 
  group_by( cidade ) %>% 
  summarise( media_salarios = mean(salarios),
             dp_salarios = sd(salarios),
             n = n() )

var.test( formula = salarios ~ cidade,
          data = dados_salarios )



#### Comparar mais de 2 medias ####

dados_dietas <- read_csv2( file = 'education/dados/dados_dietas.csv' )

dados_dietas %>% 
  group_by( dieta ) %>% 
  summarise( media_perda_peso = mean(perda_peso_kg),
             dp_perda_peso = sd(perda_peso_kg),
             n = n() )

resultados_teste <- aov( formula = perda_peso_kg ~ dieta, 
                         data = dados_dietas )

summary(resultados_teste)


