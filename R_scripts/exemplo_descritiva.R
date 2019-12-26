library(tidyverse)

dados <- data_frame( id = 1:10,
                     salario = c(1511,
                                 1789,
                                 1794,
                                 1794,
                                 3171,
                                 3224,
                                 3289,
                                 3680,
                                 3852,
                                 4167) )

# Minimo

dados %>% 
  summarise( min_salario = min(salario) )

# Maximo

dados %>% 
  summarise( max_salario = max(salario) )

# Media

dados %>% 
  summarise( media_salario = mean(salario) )
  
# Mediana

dados %>% 
  summarise( mediana_salario = median(salario) )

# percentil na raca

k_posicao <- function(n, p) ((n-1)*p) + 1

k_posicao(10, 0.05)

dados

(0.55*1511) + (0.45*1789)


# percentil na formula

dados %>% 
  summarise( quantile( salario, 0.05 ) )


# quartil

dados %>% 
  summarise( quartil_1 = quantile( salario, 0.25 ) )


# variancia e desvio padrao

dados %>% 
  summarise( variancia = var(salario),
             dp = sd(salario) )


