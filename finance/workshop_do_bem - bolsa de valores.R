library(tidyquant, warn.conflicts = F)
library(PortfolioAnalytics, warn.conflicts = F)
library(timetk, warn.conflicts = F)
library(ROI, warn.conflicts = F)
library(ROI.plugin.glpk, warn.conflicts = F)
library(ROI.plugin.quadprog, warn.conflicts = F)

#acoes selecionadas
tickers <- c("AABA","CSCO","EBAY","MSFT","ORCL") #stocks_data1.csv
tickers <- c("ABEV3.SA","PETR4.SA","BBDC4.SA","ITUB4.SA","CIEL3.SA") #stocks_data2.csv
tickers <- c("ECOR3.SA","ELET3.SA","GOLL4.SA","MGLU3.SA","SUZB3.SA") #stocks_data3.csv

#definindo o periodo historico de 1 ano para as cotacoes
#from <- today() - years(1)

#acessando os dados de cotacoes
#stocks_data <- tq_get(tickers, 
#                     get = "stock.prices",
#                     from = from)

#stocks_data <- stocks_data[complete.cases(stocks_data),]

#acessando os dados de cotacoes intraday - Algo Trading
#av_api_key("api-key")
#stocks_data <- tq_get(tickers, 
#                      get = "alphavantager",
#                      av_fun = "TIME_SERIES_INTRADAY",
#                      interval = "1min",
#                      from = from)

stocks_data <- as.tibble(read_csv("dados/stocks_data1.csv",
                                  col_types = cols(date = col_date(format = "%Y-%m-%d"))))

#Analise dos precos historicos
stocks_data %>%
  ggplot(aes(date, close, color = symbol)) +
  geom_line() +
  ggtitle("Historico de Precos") +
  theme_tq()

stocks_data %>%
  ggplot(aes(date, close, color = symbol)) +
  geom_line() +
  geom_smooth(method = "loess") +
  ggtitle("Historico de Precos") +
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y")

stocks_data %>%
  ggplot(aes(date)) +
  geom_bar(aes(date, volume / 1000000, color = symbol), stat = "identity") +
  ggtitle("Volume operados em milhoes de acoes") + 
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2) 
  
#Retorno diario
stocks_data <- stocks_data %>%
  group_by(symbol) %>%
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn, 
            period = "daily",
            type = "log",
            col_rename = "returns")

stocks_data <- stocks_data %>%
  group_by(symbol) %>%
  tq_mutate(select = returns,
            mutate_fun = runSum,
            n = 1,
            cumulative = T,
            col_rename = "returns_cumulative")

stocks_data %>%
  ggplot(aes(date, returns_cumulative, color = symbol)) +
  geom_line() +
  ggtitle("Retorno Acumulado no Periodo") +
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2)

#Risco
stocks_data %>%
  ggplot(aes(date, returns, color = symbol)) +
  geom_line() +
  ggtitle("Variacao de Risco e Retorno") +
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2)

#Weighting and Optimization
stocks_data_returns <- stocks_data %>%
  select(date, symbol, returns) %>%
  spread(key = symbol, value = returns) %>%
  tk_ts(frequency = 1, silent = T, start = min(stocks_data$date))

#define quais acoes irao compor o portfolio
p <- portfolio.spec(assets = colnames(stocks_data_returns))

#define que os pesos definidos devem totalizar 1
p <- add.constraint(portfolio = p,
                    type = "full_investment")

#define que sera assumida apenas a posicao comprada nas acoes
p <- add.constraint(portfolio = p,
                    type = "long_only")

#define que o objetivo sera o menor risco
p <- add.objective(portfolio = p,
                   type = "risk",
                   name = "var")

#define que o objetivo sera o maior retorno
p <- add.objective(portfolio = p,
                   type = "return",
                   name = "mean")

#define que o objetivo sera minimizar perda "expected tail loss'
p <-  add.objective(portfolio = p, 
                    type = "risk_budget", 
                    name = "ETL",
                    arguments = list(p = 0.95), #intervalo de confianca para minimizacao de perda
                    max_prisk = 0.3) #nenhuma acao deve ser responsavel por mais de 30% do risco

#visualiza o conjunto de criterios e objetivos escolhidos
print(p)

#realiza a otimizacao de pesos de acordo com os criterios e objetivos escolhidos
opt <- optimize.portfolio(R = stocks_data_returns,
                   portfolio = p,
                   optimize_method = "ROI",
                   trace = T)

opt

weights1 <- opt$weights
weights2 <- opt$weights
weights3 <- opt$weights

#insira neste trecho os pesos adquiridos para comparacao dos Portfolios
weights <- c(
  weights1,
  weights2,
  weights3
)

#cria uma tabela relacionado os pesos e o numero de portfolio a ser criado
weights_table <-  tibble(tickers) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(weights)) %>%
  group_by(portfolio)

#constroi o portfolio de acordo com a tabela de pesos
portfolio_returns <- stocks_data %>%
  tq_repeat_df(n = 3) %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = returns, 
               weights     = weights_table, 
               col_rename  = "returns")

#calcula o retorno acumulado dos portfolios
portfolio_returns <- portfolio_returns %>%
  group_by(portfolio) %>%
  tq_mutate(select = returns,
            mutate_fun = runSum,
            n = 1,
            cumulative = T,
            col_rename = "returns_cumulative")

#compara o retorno acumulado dos portfolios
portfolio_returns %>%
  ungroup %>% 
  mutate( portfolio = as.factor(portfolio) ) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(x = date, y = returns_cumulative, color = portfolio)) +
  theme_tq() +
  facet_wrap(~ portfolio, ncol = 1)



