#### Carregar pacotes ####
library(tidyverse)
library(magrittr)


#### Carregar dados ####
dados_nps <- read_csv2( file = 'education/dados/nps_example.csv' )


#### filtrar respostas incompletas dados ####

dados_nps %>% 
  count(response_status)

dados_nps_filtrados <- dados_nps %>% 
  filter( response_status == 'Complete',
          !is.na(nps_score) )


dados_nps_filtrados %>% 
  group_by( age ) %>% 
  summarise( media_nps = mean(nps_score),
             dp_nps = sd(nps_score),
             n = n() )

dados_nps_filtrados_age <- dados_nps_filtrados %>% 
  filter( age != "75+")

dados_nps_filtrados_age %>% 
  group_by( age ) %>% 
  summarise( media_nps = mean(nps_score),
             dp_nps = sd(nps_score),
             n = n() )

#### teste ANOVA ####

resultados_anova_nps <- aov( formula = nps_score ~ age, 
     data = dados_nps_filtrados_age )

summary(resultados_anova_nps)













