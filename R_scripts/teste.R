library(tidyverse)
library(lubridate)
library(magrittr)
library(plotly)

dados <- read_csv2( file = "dados/dados_1997_2011_paises_csv.csv", 
                    locale = locale(encoding = "latin1" ) )

dados %>% 
  plot_ly( x = ~populacao,
           y = ~pib, 
           frame = ~ano, 
           text = ~pais,
           color = ~pais,
           type = 'scatter',
           mode = 'markers') %>% 
  layout(
    yaxis = list(
      type = "log"
    ),
    xaxis = list(
      type = "log"
    )
  )
  