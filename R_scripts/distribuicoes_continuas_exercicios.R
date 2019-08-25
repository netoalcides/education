### Carregar pacotes ###
library(tidyverse)
library(magrittr)
library(lubridate)



### Carregar dados ###
dados <- read_csv2( file = 'education/dados/vinho_nacional.csv' )


### Exercicio slide 18

media <- 50
dp <- 10

prob_35 <- pnorm( q = 30, mean = media, sd = dp )

prob_45 <- pnorm( q = 45, mean = media, sd = dp )

prob_45 - prob_35


### Exercicio slide 19

media <- 50
dp <- 10

prob_85 <- pnorm( q = 85, mean = media, sd = dp )

1 - prob_85


### Exercicio slide 22

resultados_vinhos <- dados %>% 
  summarise( media = mean(vendas_vinho_nacional),
             dp = sd(vendas_vinho_nacional) )


### Exercicio slide 23

p_330 <- pnorm( q = 330, 
                mean = resultados_vinhos$media, 
                sd = resultados_vinhos$dp )

p_370 <- pnorm( q = 370, 
                mean = resultados_vinhos$media, 
                sd = resultados_vinhos$dp )

p_370 - p_330


### Exercicio slide 24

p_370 <- pnorm( q = 370, 
                mean = resultados_vinhos$media, 
                sd = resultados_vinhos$dp )

(1 - p_370) * 100










