### Conceito integral
library(mosaicCalc)

# funcao geral
f_x <- function(x) x^2

# tamanho dos pequenos intervalos
dx <- 0.1

# intervalo
minimo <- -3
maximo <- 3
x <- seq( minimo, maximo, dx)

# somas dos intervalos
soma_etapa <- rep(0, length(x) )
soma_total <- NULL

for (i in 2:length(x)){
  
  # overwrite each zero with each number
  soma_etapa[i] = f_x(x[i])*(x[i]-x[i-1])
}



soma_etapa
sum(soma_etapa)

integrate( f_x, lower = minimo, upper = maximo)

barplot(soma_etapa, names.arg=x)


### Usando o pacote mosaicCalc

new_f_x <- antiD( x^2 ~ x )

new_f_x

new_f_x(3) - new_f_x(-3)
