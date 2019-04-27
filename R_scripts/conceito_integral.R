### Conceito integral

# carregar pacotes
library(tidyverse)
library(mosaicCalc)


# funcao geral
f_x <- function(x) 2*x^2

# intervalo
minimo <- 0
maximo <- 24
n <- 8

# tamanho dos pequenos intervalos
dx <- (maximo - minimo) / n

# define o x
x <- seq( minimo, maximo, dx)

# o grafico
data_frame( x = x,
            f_x = f_x( x = x ) ) %>% 
  
  ggplot( aes( x = x, y = f_x ) ) +
  
  geom_bar( stat = 'identity' ) +
  geom_line()

# integral na raca
data_frame( x = x,
            f_x = f_x( x = x ) ) %>% 
  
  mutate( fx_dx = f_x * dx ) %>% 
  
  summarise( sum(fx_dx) )


# integral usando as regras
new_f_x <- antiD( 2 * x^2 ~ x )

new_f_x

new_f_x(24) - new_f_x(0)

# integral usando aproximacao
integrate( f_x, lower = minimo, upper = maximo )
