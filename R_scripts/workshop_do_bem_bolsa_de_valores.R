# sudo apt-get install libglpk-dev

library(tidyquant, warn.conflicts = F)
library(PortfolioAnalytics, warn.conflicts = F)
library(timetk, warn.conflicts = F)
library(ROI, warn.conflicts = F)
library(ROI.plugin.glpk, warn.conflicts = F)
library(ROI.plugin.quadprog, warn.conflicts = F)

#ações selecionadas
tickers <- c("AABA","CSCO","EBAY","MSFT","ORCL")
# tickers <- c("ABEV3.SA","PETR4.SA","BBDC4.SA","ITUB4.SA","CIEL3.SA")
# tickers <- c("ECOR3.SA","ELET3.SA","GOLL4.SA","MGLU3.SA","SUZB3.SA")

#definindo o periodo histórico de 1 ano para as cotações
from <- today() - years(1)

#acessando os dados de cotações
stocks_data <- tq_get(tickers, 
                      get = "stock.prices", 
                      from = from)

# stocks_data <- as.tibble(read_csv("stocks_data.csv",
#                                   col_types = cols(date = col_date(format = "%Y-%m-%d"))))

#Analise dos preços históricos
stocks_data %>%
  ggplot(aes(date, close, color = symbol)) +
  geom_line() +
  ggtitle("Histórico de Preços") +
  theme_tq()

stocks_data %>%
  ggplot(aes(date, close, color = symbol)) +
  geom_line() +
  geom_smooth(method = "loess") +
  ggtitle("Histórico de Preços") +
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y")

stocks_data %>%
  ggplot(aes(date)) +
  geom_bar(aes(date, volume / 1000000, color = symbol), stat = "identity") +
  ggtitle("Volume operados em milhões de ações") + 
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2) 
  
#Retorno diário
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
  ggtitle("Retorno Acumulado no Período") +
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2)

#Risco
stocks_data %>%
  ggplot(aes(date, returns, color = symbol)) +
  geom_line() +
  ggtitle("Variação de Risco") +
  theme_tq() +
  facet_wrap(~ symbol, ncol = 2)

#Weighting and Optimization
stocks_data_returns <- stocks_data %>%
  select(date, symbol, returns) %>%
  spread(key = symbol, value = returns) %>%
  tk_ts(frequency = 1, silent = T, start = min(stocks_data$date))

p <- portfolio.spec(assets = colnames(stocks_data_returns))

p <- add.constraint(portfolio = p,
                    type = "full_investment")

p <- add.constraint(portfolio = p,
                    type = "long_only")

p <- add.objective(portfolio = p,
                   type = "risk",
                   name = "var")

p <- add.objective(portfolio = p,
                   type = "return",
                   name = "mean")

p <- add.objective(portfolio = p,
                   type = "weight_concentration",
                   name = "HHI",
                   conc_aversion = 1)

print(p)

optimize.portfolio(R = stocks_data_returns,
                   portfolio = p,
                   optimize_method = "ROI",
                   trace = T)

weights <- c(
  0.50, 0.15, 0.15, 0.10, 0.10,
  0.10, 0.50, 0.10, 0.15, 0.15,
  0.15, 0.10, 0.50, 0.10, 0.15
)

weights_table <-  tibble(tickers) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(weights)) %>%
  group_by(portfolio)

portfolio_returns <- stocks_data %>%
  tq_repeat_df(n = 3) %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = returns, 
               weights     = weights_table, 
               col_rename  = "returns")

portfolio_returns <- portfolio_returns %>%
  group_by(portfolio) %>%
  tq_mutate(select = returns,
            mutate_fun = runSum,
            n = 1,
            cumulative = T,
            col_rename = "returns_cumulative") %>% 
  ungroup

portfolio_returns %>%
  mutate( portfolio = as.factor(portfolio)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(x = date, y = returns_cumulative, color = portfolio)) +
  theme_tq() +
  facet_wrap(~ portfolio, ncol = 2)



