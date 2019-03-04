# Carregar pacotes

library(tidyverse)
library(magrittr)
library(mosaicCalc)
library(ineq)

# Carregar dados

dados_municipios <- read_csv( file = 'education/dados/dados_renda_municipios.csv' )


## Analises

### Selecionar estados

estado_1 = 'Alagoas'
estado_2 = 'Sergipe'

estados_1 <- dados_municipios %>% 
  filter( UF == estado_1 ) %>% 
  arrange( RDPC )
  
estados_2 <- dados_municipios %>% 
  filter( UF == estado_2 ) %>% 
  arrange( RDPC )


## Construir as curvas

### Municipio 1
p_renda_estado_1 <- c( 0, cumsum(estados_1$RDPC/sum(estados_1$RDPC)) )
p_familias_estado_1 <- seq( from = 0, 
                               to = 1, 
                               by = 1/dim(estados_1)[1] )

dados_estado_1 <- data_frame(p_renda_estado_1, p_familias_estado_1)


### Municipio 2
p_renda_estado_2 <- c( 0, cumsum(estados_2$RDPC/sum(estados_2$RDPC)) )
p_familias_estado_2 <- seq( from = 0, 
                            to = 1, 
                            by = 1/dim(estados_2)[1] )
dados_estado_2 <- data_frame(p_renda_estado_2, p_familias_estado_2)

### Curvas Brutas

dados_estado_1 %>% 
  ggplot( aes( x = p_familias_estado_1, y = p_renda_estado_1 ) ) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, color="red", linetype = "dashed")

dados_estado_2 %>% 
  ggplot( aes( x = p_familias_estado_2, y = p_renda_estado_2 ) ) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, color="red", linetype = "dashed")


### Curva ajustada estado 1

modelo_1 <- lm( formula = p_renda_estado_1 ~ p_familias_estado_1 + I(p_familias_estado_1^2), 
                data = dados_estado_1 )

dados_estado_1 %<>% 
  mutate( p_renda_estado_1_pred = predict(modelo_1),
          p_renda_estado_1_pred = ifelse( p_familias_estado_1 == 0, 0, 
                                          ifelse( p_familias_estado_1 == 1, 1, p_renda_estado_1_pred ) ) )

dados_estado_1 %>% 
  ggplot( aes( x = p_familias_estado_1, y = p_renda_estado_1 ) ) +
  geom_line() +
  geom_line( aes( y = p_renda_estado_1_pred ), color = 'green' ) +
  geom_abline(intercept = 0, slope = 1, color="red", linetype = "dashed")


### Curva ajustada estado 2

modelo_2 <- lm( formula = p_renda_estado_2 ~ p_familias_estado_2 + I(p_familias_estado_2^2), 
                data = dados_estado_2 )

dados_estado_2 %<>% 
  mutate( p_renda_estado_2_pred = predict(modelo_2),
          p_renda_estado_2_pred = ifelse( p_familias_estado_2 == 0, 0, 
                                          ifelse( p_familias_estado_2 == 1, 1, p_renda_estado_2_pred ) ) )

dados_estado_2 %>% 
  ggplot( aes( x = p_familias_estado_2, y = p_renda_estado_2 ) ) +
  geom_line() +
  geom_line( aes( y = p_renda_estado_2_pred ), color = 'green' ) +
  geom_abline(intercept = 0, slope = 1, color="red", linetype = "dashed")


### Indice de Gini estado 1

modelo_1

Fp_estado_1 <- antiD( p_familias - 0.004197 - 0.638235*p_familias - 0.319308*p_familias^2 ~ p_familias )
Fp_estado_1

gini_estado_1 <- 2 * ( Fp_estado_1(1) - Fp_estado_1(0) )
gini_estado_1

### Indice de Gini estado 2

modelo_2

Fp_estado_2 <- antiD( p_familias - 0.0009889 - 0.6505861*p_familias - 0.3112828*p_familias^2 ~ p_familias )
Fp_estado_2

gini_estado_2 <- 2 * ( Fp_estado_2(1) - Fp_estado_2(0) )
gini_estado_2


### Resultados finais

gini_estado_1
ineq( estados_1$RDPC, type = "Gini" ) # formula que ajuda a vida

gini_estado_2
ineq( estados_2$RDPC, type = "Gini" ) # formula que ajuda a vida






## Geracao dos dados

# dados <- read_csv2( file = 'education/dados/dados_municipios_all.zip',
#                     locale = locale( encoding = 'latin1') )
# 
# id_estados <- read_csv2( file = 'education/dados/id_estados.csv',
#                          locale = locale( encoding = 'latin1') )
# 
# 
# dados %>% 
#   filter( ANO == 2010 ) %>% 
#   select( ID_UF = UF, Municipio, RDPC ) %>% 
#   left_join(., id_estados,
#             by = 'ID_UF' ) %>% 
#   select( UF, Municipio, RDPC ) %>% 
#   write_csv(., path = 'education/dados/dados_renda_municipios.csv')

