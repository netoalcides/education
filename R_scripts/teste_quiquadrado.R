### Carregar pacotes ###

library(tidyverse)
library(magrittr)


### Teste de aderencia ###

frequencia_observada <- c(80, 20,	30,	35,	28,	100, 150)

frequencia_esperada <- c(0.25, 0.05, 0.10,	0.10,	0.10,	0.15, 0.25)

teste_aderencia <- chisq.test( x = frequencia_observada, 
                               p = frequencia_esperada )


teste_aderencia


### Teste de homogeneidade ###

dados_tratamentos <- read_csv2( file = 'education/dados/dados_tratamentos.csv')

dados_tratamentos %>% 
  count( tratamento, evolucao ) %>% 
  spread( key = evolucao, value = n)

teste_homog <- chisq.test( x = dados_tratamentos$tratamento, 
                           y = dados_tratamentos$evolucao, 
                           correct = FALSE )

teste_homog


### Teste de independencia ###

dados_empresas <- read_csv( file = 'education/dados/analise_empresas.csv')

dados_empresas %>% 
  count( SaudeFin, RespSocial ) %>% 
  spread( key = RespSocial, value = n)

teste_indep <- chisq.test( x = dados_empresas$SaudeFin, 
                           y = dados_empresas$RespSocial, 
                           correct = FALSE )

teste_indep






