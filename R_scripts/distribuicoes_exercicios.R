### Carregar pacotes ###
library(tidyverse)
library(magrittr)
library(lubridate)



### Carregar dados ###
dados <- read_csv( file = 'education/dados/acidentes.csv' )



### Exercicio slide 23 ###

media_acidentes <- 20
n_acidentes <- 7

dpois( x = n_acidentes, 
       lambda = media_acidentes )



### Exercicio slide 24 ###

media_acidentes <- 20
n_acidentes <- 15:17

dpois( x = n_acidentes, 
       lambda = media_acidentes ) %>% 
  sum()



### Exercicio slide 26 ###

# obter numero de acidentes por mes

dados_acidentes_mensal <- dados %>% 
  mutate( data_mensal = as.Date(paste0( year(data_acidente), '-', 
                                        month(data_acidente), '-',  
                                        '01') ) ) %>%
  count(data_mensal)



# obter a media mensal

media_acidentes_mensal <- dados_acidentes_mensal %>% 
  summarise( media_acidentes_mensal = mean(n) )



### Exercicio slide 27 ###

dpois( x = 7, 
       lambda = media_acidentes_mensal$media_acidentes_mensal )



### Exercicio slide 28 ###

dpois( x = 4:6, 
       lambda = media_acidentes_mensal$media_acidentes_mensal ) %>% 
  sum()




