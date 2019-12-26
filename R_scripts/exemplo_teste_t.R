#### Carregar pacotes ####
library(tidyverse)
library(magrittr)


#### Carregar dados ####
dados_profs <- read_csv2( file = 'education/dados/salarios_profs.csv',
                          locale = locale( encoding = 'latin1' ) )

  

#### Analises ####

# teste t 1 grupo
dados_profs_filtrados <- dados_profs %>%
  filter( FUNCAO == 'Professor Titular',
          JORNADA == 'RDIDP',
          UNID_ORGAO =='ICMC' ) %>%
  select( UNID_ORGAO, SALARIO_MENSAL_LIQUIDO )

dados_profs_filtrados %>%
  group_by( UNID_ORGAO ) %>%
  summarise( media_salarial = mean(SALARIO_MENSAL_LIQUIDO),
             dp_salarial = sd(SALARIO_MENSAL_LIQUIDO),
             n = n() )

t.test( x = dados_profs_filtrados$SALARIO_MENSAL_LIQUIDO,
        mu = 20000,
        alternative = 'greater',
        conf.level = 0.95 )


# teste t 2 grupos

dados_profs_filtrados <- dados_profs %>%
  filter( FUNCAO == 'Professor Titular',
          JORNADA == 'RDIDP',
          UNID_ORGAO %in% c('FEA', 'ICMC') ) %>%
  select( UNID_ORGAO, SALARIO_MENSAL_LIQUIDO )

dados_profs_filtrados %>%
  group_by( UNID_ORGAO ) %>%
  summarise( media_salarial = mean(SALARIO_MENSAL_LIQUIDO),
             dp_salarial = sd(SALARIO_MENSAL_LIQUIDO),
             n = n() )

t.test( formula = SALARIO_MENSAL_LIQUIDO ~ UNID_ORGAO,
        alternative = 'two.sided',
        conf.level = 0.95,
        data = dados_profs_filtrados )







dados_profs_filtrados <- dados_profs %>%
  filter( FUNCAO == 'Professor Titular',
          JORNADA == 'RDIDP',
          UNID_ORGAO %in% c('FEA', 'FFLCH') ) %>%
  select( UNID_ORGAO, SALARIO_MENSAL_LIQUIDO )

dados_profs_filtrados %>%
  group_by( UNID_ORGAO ) %>%
  summarise( media_salarial = mean(SALARIO_MENSAL_LIQUIDO),
             dp_salarial = sd(SALARIO_MENSAL_LIQUIDO),
             n = n() )

t.test( formula = SALARIO_MENSAL_LIQUIDO ~ UNID_ORGAO,
        alternative = 'two.sided',
        conf.level = 0.95,
        data = dados_profs_filtrados )









