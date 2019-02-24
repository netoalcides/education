### Carregar pacotes

library(tidyverse)
library(magrittr)


### Carregar dados

dados <- read_csv2( file = 'education/dados/exemplo_preco_quantidade.csv' )

### Analises

## Modelo de Regressao Linear (assunto do proximo semestre)
# objetivo: encontrar um modelo que ajude a prever a quantidade dado o preco
modelo_regressao <- lm( formula = Quantidade ~ Preco,
                        data = dados )

summary( modelo_regressao )

# parametros
a <- 50
b <- -2


## Equacoes do exemplo Buscar o melhor preco

custo_unitario <- 0.2 # 1 unidade do produto custa R$0,20

# equacao de previsao da quantidade
# quantidade = alpha - beta * preco
funcao_quantidade <- function( alpha, beta, preco ) alpha + beta*preco 

# ver a equacao
body( funcao_quantidade )

# obter a derivada da funcao
D( body(funcao_quantidade), 'preco')

# adicionar quantidade esperada no banco

dados %<>% 
  mutate( quantidade_esperada = funcao_quantidade( alpha = a, beta = b, preco = Preco) )


# equacao da receita
# receita = preco * quantidade
funcao_receita <- function( alpha, beta, preco ) (preco * alpha) + (beta * (preco^2) )

funcao_receita( alpha = a, beta = b, preco = 5 )

# ver a equacao
body( funcao_receita )

# obter a derivada da funcao
D( body( funcao_receita ),  'preco' )


# equacao custo total
# custo_total = custo_unitario * quantidade
funcao_custo <- function( alpha, beta, preco, custo ) custo * alpha + (custo * beta * preco)

# ver a equacao
body( funcao_custo )

# obter a derivada da funcao
D( body( funcao_custo ),  'preco' )



# equacao lucro
# Lucro = receita - custo_total
funcao_lucro <- function( alpha, beta, preco, custo )  ( preco * alpha + (beta * preco^2) ) - ( custo * alpha + (custo * beta * preco) )
funcao_lucro( alpha = a, beta = b, preco = 5, custo = custo_unitario)

# ver a equacao
body( funcao_lucro )

# obter a derivada da funcao
D( body( funcao_lucro ),  'preco' )


## graficos

# preco x quantidade
dados %>% 
  ggplot( aes( x = Preco, y = Quantidade ) ) +
  geom_point()

# preco x quantidade
dados %>% 
  ggplot( aes( x = Preco, y = quantidade_esperada ) ) +
  geom_point()


# receita x preco

dados %<>% 
  mutate( Receita = funcao_receita( alpha = a, beta = b, preco = Preco ) )

dados %>% 
  ggplot( aes( x = Preco, y = Receita ) ) +
  geom_point()

# lucro x preco

dados %<>% 
  mutate( Lucro = funcao_lucro( alpha = a, beta = b, preco = Preco, custo = custo_unitario ) )


dados %>% 
  ggplot( aes( x = Preco, y = Lucro ) ) +
  geom_point()

## Melhor preco

funcao_melhor_preco <- function( preco, custo, alpha, beta ){}
funcao_melhor_preco

body(funcao_melhor_preco) <- D( body( funcao_lucro ),  'preco' )


# obter melhor preco
melhor_preco <- uniroot( f = funcao_melhor_preco, 
                         lower = 5, 
                         upper = 20, 
                         alpha = a,
                         beta = b,
                         custo = custo_unitario )

melhor_preco

melhor_preco$root

# receita maxima

funcao_receita( alpha = a, beta = b, preco = melhor_preco$root )

funcao_lucro( alpha = a, beta = b, preco = melhor_preco$root, custo = custo_unitario )




