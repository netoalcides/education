### Carregar pacotes ###
library(tidyverse)
library(magrittr)
library(lubridate)



### Exemplos simulados

## distribuicao exponencial

n = 60

duracao_media <- 12

dados_exponencial <- data_frame( resultados = 0:n,
                                 probabilidade = dexp( x = resultados, 
                                                       rate = 1/duracao_media ) )

dados_exponencial %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' ) +
  geom_line( color='steelblue' )


## distribuicao normal

n = 70
media = 30
desvio_padrao <- 6

dados_normal <- data_frame( resultados = seq( from = 0, 
                                              to = n, 
                                              by = 0.5 ),
                            probabilidade = dnorm( x = resultados, 
                                                   mean = media, 
                                                   sd = desvio_padrao ) )

dados_normal %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' ) +
  geom_line( color='steelblue' )


## distribuicao t

n = 30
media = 50
desvio_padrao <- 2

dados_t <- data_frame( resultados = seq( from = 0, 
                                              to = 100, 
                                              by = 0.5 ),
                       t = (resultados - media) / desvio_padrao / sqrt(n),
                       probabilidade = dt( x = t, df = n - 1 ) )

dados_t %>% 
  ggplot( aes( x = t,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' ) +
  geom_line( color='steelblue' )



## distribuicao qui-quadrado

n = 10

dados_quiquadrado <- data_frame( resultados = seq( from = 0, 
                                                   to = 30, 
                                                   by = 0.5 ),
                                 probabilidade = dchisq( x = resultados, 
                                                         df = n ) )

dados_quiquadrado %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' ) +
  geom_line( color='steelblue' )


## distribuicao F

n1 <- 5
n2 <- 4

dados_f <- data_frame( resultados = seq( from = 0, 
                                         to = 30, 
                                         by = 0.5 ),
                       probabilidade = df( x = resultados,
                                           df1 = n1 - 1, 
                                           df2 = n2 - 1) )

dados_f %>% 
  ggplot( aes( x = resultados,
               y = probabilidade ) ) +
  geom_bar( stat = 'identity' ) +
  geom_line( color='steelblue' )












