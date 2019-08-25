### Carregar pacotes ###
library(tidyverse)
library(magrittr)
library(lubridate)



### Carregar dados ###
dados <- read_csv( file = 'education/dados/acidentes.csv' )



### Exemplo histograma
dados_acidentes_mensal <- dados %>% 
  mutate( data_mensal = as.Date(paste0( year(data_acidente), '-', 
                                        month(data_acidente), '-',  
                                        '01') ) ) %>%
  count(data_mensal)

hist(dados_acidentes_mensal$n)

dados_acidentes_mensal %>% 
  ggplot( aes( x = n ) ) + 
  geom_histogram( aes(y = ..density..), 
                  colour="black", 
                  fill="white", 
                  bins = 9)



### Exemplos simulados

### distribuicao uniforme

n = 6
p = 1/n

dados_uniforme <- data_frame( resultados = 1:n,
                              probabilidade = p )

dados_uniforme %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' )



### distribuicao uniforme

p <-  0.2

dados_bernoulli <- data_frame( resultados = 0:1,
                               probabilidade = c(p, 1-p) )

dados_bernoulli %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' )



### distribuicao binomial

# dbinom( size = , # numero maximo de resultados
#         x = , # resultados 
#         prob = # probabilidade
#       )

n <- 5
p <- 0.2

dados_binomial <- data_frame( resultados = 0:n,
                              probabilidade = dbinom( x = resultados, 
                              size = n, 
                              prob = p ) )

dados_binomial %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' )



### distribuicao poisson

# dpois( x = , # resultados
#        lambda = # media de eventos dentro de um intervalo de tempo
#      )

n <- 15
media <- 2

dados_poisson <- data_frame( resultados = 0:n,
                             probabilidade = dpois( x = resultados, 
                                                    lambda = media ) )

dados_poisson %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' )






