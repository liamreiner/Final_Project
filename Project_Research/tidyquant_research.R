library(shiny)
library(tidyquant)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(tidyverse)
library(forecast)
library(dygraphs)
library(ts343)


monthly_portfolio_returns <- c("AAPL", "TSLA", "NRXP", "MA", "V") %>%
  tq_get(get  = "stock.prices",
         from = "1999-01-31",
         to   = "2021-01-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Portfolio")
monthly_portfolio_returns


weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
portfolio <- monthly_portfolio_returns %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Portfolio, 
               weights      = weights, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 5000)

ggplot(data = portfolio, aes(x = date, y = investment.growth)) + 
  geom_area(aes(data = monthly_portfolio_returns, fill = symbol)) 




ggplot(data = portfolio, aes(x = date, y = investment.growth)) +
  geom_line(size = 1) +
  labs(title = "Analysis of Equities: Portfolio Growth",
       subtitle = "20% AAPL, 20% TSLA, 20% NRXP, 20% Mastercard, and 20% Visa",
       x = "Time", y = "Portfolio Value") +
  geom_forecast(data = portfolio, aes(x = date, y = investment.growth))


### Forecasting Research

devtools::install_github("statmanrobin/ts343")
install.packages("ts343")

portfolio2 <- ts(portfolio$investment.growth, frequency = 12, start = 1999)
modHW <- HoltWinters(portfolio2)
modHW <- forecast(portfolio2)
plot(modHW)
summary(modHW)
plot.forecast(modHW)
sesplot(modHW,include=21)



benchmark <- c("SPY")
benchmark <- tq_get(benchmark,
                    get  = "stock.prices",
                    from = "1850-01-31",
                    to   = "2021-10-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "benchmark_returns") %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = benchmark_returns, 
               col_rename   = "benchmark.growth",
               wealth.index = TRUE) %>%
  mutate(benchmark.growth = benchmark.growth * 5000)



portfolio_forecast <- ts(portfolio$investment.growth, frequency = 12, start = 1999)
portfolio_forecast <- HoltWinters(portfolio_forecast)
portfolio_forecast <- forecast(portfolio_forecast)
autoplot(portfolio_forecast, conf.int = FALSE)


portfolio_forecast %>%
  stlf(h = 60) %>%
  autoplot(title = "Portfolio Returns")

library(knitr)

portfolio_forecast <- ts(portfolio$investment.growth, frequency = 12, start = 1999)
modHW=hw(portfolio_forecast,initial="simple",h=12,level=0.95)
table2=summary(modHW)

kable(modHW)

table1 = sesplot(modHW,include=72, main = "Forecast of Portfolio Value")
kable(table2)




sesplotmod = function (mod, include = NULL, attach = "fits", main = "", ylab = "", xlab = "", 
         lwd = 2) 
{
  x = mod$x
  n = length(x)
  past = mod$fitted
  upper = mod$upper
  lower = mod$lower
  nextfit = mod$mean
  if (!is.null(include)) {
    x = subset(x, start = n - include + 1)
    past = subset(past, start = n - include + 1)
  }
  if (attach == "series") {
    nextfit = ts(c(tail(x, 1), nextfit), start = end(x), 
                 freq = frequency(x))
    lower = ts(c(tail(x, 1), lower), start = end(x), freq = frequency(x))
    upper = ts(c(tail(x, 1), upper), start = end(x), freq = frequency(x))
  }
  if (attach == "fits") {
    
    nextfit = ts(c(tail(past, 1), nextfit), start = end(past), 
                                      freq = frequency(past))
    lower = ts(c(tail(past, 1), lower), start = end(past), 
             freq = frequency(past))
    upper = ts(c(tail(past, 1), upper), start = end(past), 
             freq = frequency(past))
  }
  
  ts.plot(x, past, nextfit, lower, upper, col = c("black", 
                                                  "blue", "blue", "red", "red"), 
          lwd = lwd, main = main, ylab = ylab, xlab = xlab)
}

sesplotmod(mod = modHW, ylab = "Portfolio Value (USD)", xlab = "Date (Years)")

