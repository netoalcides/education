# Carregar pacotes

library(tidyverse)
library(magrittr)


# criar formula da combinacao

formula_combinacao <- function( n, k){ factorial(n) / (factorial(n-k)*factorial(k))}

n = 5

for( i in 0:n ){
  combinacao <- formula_combinacao( n = n, k = i) %>% 
    show()
}


# obter probabilidades

formula_bernoulli <- function( p, n, k ){ p^k * (1-p)^(n-k) }

p = 0.8

for( i in 0:n ){
  probabilidade <- formula_bernoulli( p = p, n = n, k = i ) %>% 
    show()
}


# distribuicao binomial

for( i in 0:n ){
  distribuicao_binomial <- dbinom( x = i, size = n, prob = p ) %>% 
    show()
}


# grafico

tamanho_amostra = 5

dados <- data_frame( n_sucessos = 0:tamanho_amostra ) %>% 
  mutate( prob_binomial = dbinom( x = n_sucessos, 
                                  size = tamanho_amostra, 
                                  prob = 0.8 ) )

dados %>% 
  ggplot( aes( x = n_sucessos, y = prob_binomial ) ) +
  geom_bar( stat = 'identity')


# Exercicio no R


dbinom(x = 2, size = 5, prob = 0.8) + 
  dbinom(x = 3, size = 5, prob = 0.8) +
  dbinom(x = 4, size = 5, prob = 0.8)






