library(two.laws.big.bang)
library(two.laws.quant.data)
library(two.laws.quant.indicators)

library(two.laws.quant.charts)
library(lubridate)

library(quantmod)
library(TTR)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)
library(blotter)
library(quantstrat)
# library(egg)
# library(rlist)

# devtools::install_github("braverock/blotter")
# devtools::install_github("braverock/quantstrat")


DataSet <- LoadTestSymbol(amplitude = c(1, 2, 1, 2), slope = c(0.2, -0.5, -.1, -1.5)) %>%
  df_filter(Date > ymd("2017-01-01"))

DataSet %<>%
  AddMACD() %>%
  AddBBands()

browser()

DS <- xts(
  DataSet[, -1],
  order.by = DataSet[[1]]
)

#StockChart$ggplot()

#####

initDate = "1990-01-01"
from = "2017-01-01"
to = "2017-11-01"
options(width = 70)

symbols <- "DS"
rm(list = ls(.blotter), envir = .blotter)

currency('USD')
Sys.setenv(TZ = "UTC")
stock(symbols, "USD", multiplier = 1)


osDollarATR <- function(
  orderside,
  tradeSize,
  pctATR,
  maxPctATR = pctATR,
  data,
  timestamp,
  symbol,
  prefer = "Open",
  portfolio,
  integerQty = TRUE,
  strMod = "",
  rebal = FALSE
){
  return(100)
}

tradeSize <- 10000
initEq = tradeSize * length(symbols)

strategy.st <- "simple"
portfolio.st <- "simple"
account.st <- "simple"

rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(
  portfolio.st,
  symbols = symbols,
  initDate <- initDate,
  currency = 'USD'
  )

initAcct(
  account.st,
  portfolios = portfolio.st,
  initDate <- initDate,
  currency = 'USD',
  initEq <- initEq
)

initOrders(portfolio.st, initDate = initDate)

strategy(strategy.st, store=TRUE)
nLag = 252
pctATR = 0.02
period = 10

add.signal(
  strategy.st,
  name = "sigCrossover",
  arguments = list(columns = c("MACD", "MACD.Signal")),
  relationship = "gt",
  label = "coverOrBuy"
)

add.signal(
  strategy.st,
  name = "sigCrossover",
  arguments = list(columns = c("MACD", "MACD.Signal")),
  relationship = "lt",
  label = "sellOrShort"
)

# Long Rules

add.rule(
  strategy.st,
  name = "ruleSignal",
  arguments = list(
    sigcol = "coverOrBuy",
    sigval = TRUE,
    ordertype = "market",
    orderside = "long",
    replace = FALSE,
    prefer = "Open",
    osFUN = osDollarATR,
    tradeSize = tradeSize,
    pctATR = pctATR,
    atrMod = "X"
  ),
  type = "enter",
  path.dep = TRUE
)

add.rule(
  strategy.st,
  name = "ruleSignal",
  arguments = list(
    sigcol = "sellOrShort",
    sigval = TRUE,
    orderqty = "all",
    ordertype = "market",
    orderside = "long",
    replace = FALSE,
    prefer = "Open"
  ),
  type = "exit",
  path.dep = TRUE
)

out <- applyStrategy(
  strategy = strategy.st,
  portfolios = portfolio.st
)

