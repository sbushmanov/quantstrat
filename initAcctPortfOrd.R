# cleaning workspace
require(quantstrat)
rm(list=ls(all=T))
rm(envir=FinancialInstrument:::.instrument)
detach(name = "package:quantstrat", unload=T)
detach(name = "package:blotter", unload=T)
library(quantstrat)
Sys.setenv(TZ='MSK')
load("./data/quotes.RData")

# symbols traded
symbolsTraded <- symbols

# blotter inputs
initDate <- '1970-01-01' # string, must be before 1st close date of TS
initEq <- 1e5
nm <- 'mine' # portfolio and account name

# initiate currency and stocks
currency(primary_id = 'RUB')
stock(primary_id = symbolsTraded,
      currency = 'RUB')

# initiate Account and Portfolio (package:blotter)
initPortf(name = nm,                # must be before Acct
          currency = 'RUB',
          initDate = initDate,
          symbols = symbolsTraded)

initAcct(name = nm,
         portfolios = nm,
         initDate = initDate,
         currency = 'RUB',
         initEq = initEq )

# initiate Orders (package:quantstrat)
initOrders(portfolio = nm,
           symbols = symbolsTraded,
           initDate = initDate)

