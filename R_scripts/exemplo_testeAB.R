#### Carregar pacotes ####
library(tidyverse)
library(magrittr)


#### Carregar dados ####
dados_ab <- read_csv( file = 'education/dados/exampleDataABtest.csv' )


## Exercicio teste A/B

dados_ab %>%
  group_by( group ) %>%
  summarise( media_cliques = mean(clickedTrue),
             dp_cliques = sd(clickedTrue),
             n = n() )

t.test( formula = clickedTrue ~ group,
        alternative = 'two.sided',
        conf.level = 0.95,
        data = dados_ab )





