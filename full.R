#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#                                                                       #
#      Quantmod  (package:quantmod)                                     #
#                                                                       #
#########################################################################

options(width=81,continue=" ",digits=8)

## # install these packages from CRAN (or r-forge)
## #
## install.packages("xts", dependencies=TRUE)
## install.packages("PerformanceAnalytics", dependencies=TRUE)
## #
## # Install these package from r-forge
## #
## install.packages("quantmod", repos = "http://R-Forge.R-project.org")
## install.packages("TTR", repos = "http://R-Forge.R-project.org")
## install.packages("FinancialInstrument", repos = "http://R-Forge.R-project.org")
## install.packages("blotter", repos = "http://R-Forge.R-project.org")
## install.packages("quantstrat", repos = "http://R-Forge.R-project.org")

library(quantmod)
getSymbols("^GSPC")


## ------------------------------------------------------------------------
ls()
class(GSPC)
class(index(GSPC))
dim(GSPC)

## ----size='tiny'---------------------------------------------------------
tail(GSPC,4)
tail(Cl(GSPC),4)
tail(Ad(GSPC),4)

## ----GSPC1,cache=FALSE---------------------------------------------------
chartSeries(GSPC,subset="2014",theme="white")


## ----GSPC0,cache=FALSE,size='tiny'---------------------------------------
whiteTheme <- chartTheme("white")
names(whiteTheme)
whiteTheme$bg.col <- "white"
whiteTheme$dn.col <- "pink"
whiteTheme$up.col <- "lightgreen"
whiteTheme$border <- "lightgray"
x <- chartSeries(GSPC,subset="last 3 months",theme=whiteTheme,TA=NULL)
class(x)


## ----size='tiny'---------------------------------------------------------
myStr <- "2013-07-04"
class(myStr)
myDate <- as.Date(myStr)
class(myDate)
as.numeric(myDate)
format(myDate,"%m/%d/%y")
as.Date("110704",format="%y%m%d")


## ----size='tiny'---------------------------------------------------------
d <- Sys.time()
class(d)
unclass(d)
sapply(unclass(as.POSIXlt(d)), function(x) x)


## ----GSPC2,cache=FALSE---------------------------------------------------
chartSeries(GSPC["2014"],theme=whiteTheme,name="S&P 500")


## ----GSPC3,cache=FALSE---------------------------------------------------
chartSeries(GSPC["2013/2014"],theme=whiteTheme,name="S&P 500")


## ----GSPC4,cache=FALSE---------------------------------------------------
chartSeries(GSPC["2014-06::2014-07"],theme=whiteTheme,name="S&P 500")


## ----GSPC5,cache=FALSE---------------------------------------------------
chartSeries(GSPC["201406::"],theme=whiteTheme,name="S&P 500")


## ----size='tiny',results='hide'------------------------------------------
getSymbols("SPY",from="2000-01-01")


## ----size='tiny'---------------------------------------------------------
class(SPY)
head(SPY)
head(index(SPY))
class(index(SPY))


## ----results='hide',size='tiny'------------------------------------------
getSymbols("SBUX",index.class="POSIXct",from="2000-01-01")


## ----SBUX1,cache=FALSE,size='tiny'---------------------------------------
class(SBUX)
head(SBUX)
head(index(SBUX))
class(index(SBUX))
chartSeries(SBUX,theme=whiteTheme,minor.ticks=FALSE)


## ------------------------------------------------------------------------
(spl <- getSplits("SBUX"))
class(spl)


## ----size='tiny'---------------------------------------------------------
(div <- getDividends("SBUX"))
class(div)


## ----eval=FALSE----------------------------------------------------------
## adjustOHLC(x, adjust = c("split","dividend"), use.Adjusted = FALSE,
##  ratio = NULL, symbol.name=deparse(substitute(x)))


## ----size='tiny'---------------------------------------------------------
head(SBUX)
adj.exact <- adjustOHLC(SBUX)
head(adj.exact)


## ----SBUX2,cache=FALSE---------------------------------------------------
head(adj.exact)
adj.approx <- adjustOHLC(SBUX, use.Adjusted=TRUE)
head(adj.approx)
chartSeries(adj.exact,theme=whiteTheme,name="SBUX",minor.ticks=FALSE)


## ----size='tiny'---------------------------------------------------------
getSymbols("SBUX",index.class="POSIXct",from="2000-01-01",adjust=T)
head(SBUX)
head(adj.exact)


## ----results='hide'------------------------------------------------------
getSymbols('DTB3',src='FRED')


## ----RRF,cache=FALSE-----------------------------------------------------
first(DTB3,'1 week')
last(DTB3,'1 week')
chartSeries(DTB3,theme="white",minor.ticks=FALSE)



## Quandl(code, type = c("raw", "ts", "zoo", "xts"), start_date, end_date,
##  transformation = c("", "diff", "rdiff", "normalize", "cumul", "rdiff_from"),
##  collapse = c("", "weekly", "monthly", "quarterly", "annual"),
##  sort = c("desc", "asc"), meta = FALSE, authcode = Quandl.auth(), ...)

library(Quandl)


## ------------------------------------------------------------------------
cl1 = Quandl("OFDP/FUTURE_CL1",type="xts")
class(cl1)
class(index(cl1))
first(cl1)
last(cl1)

cl1 <- cl1[,c("Open","High","Low","Settle","Volume")]
colnames(cl1) <-  c("Open","High","Low","Close","Volume")
sum(is.na(coredata(cl1)))
sum(coredata(cl1)<0.01)
cl1[cl1 < 0.1] <- NA
cl1 <- na.approx(cl1)
chartSeries(cl1,name="Nymex Crude (front contract)",theme=chartTheme("white"))

b <- BBands(HLC=HLC(SBUX["2014"]), n=20, sd=2)
tail(b,10)
chartSeries(SBUX,TA='addBBands();addBBands(draw="p");addVo()',
  subset='2014',theme="white")

macd  <- MACD( Cl(SBUX), 12, 26, 9, maType="EMA" )
tail(macd)
chartSeries(SBUX, TA = "addMACD()",subset="2014",theme=whiteTheme)

rsi  <- RSI( Cl(SBUX), n = 14 )
tail(rsi)
chartSeries(SBUX, TA = "addRSI()",subset="2013",theme=whiteTheme)

myTheme<-chart_theme()
myTheme$col$up.col<-'lightgreen'
myTheme$col$dn.col<-'pink'
#
chart_Series(SBUX["2013"],TA='add_BBands(lwd=2)',theme=myTheme,name="SBUX")

## strptime(x, format, tz = "")

## xts(x = NULL, order.by = index(x), frequency = NULL,
##  unique = TRUE, tzone = Sys.getenv("TZ"), ...)


## ----size='tiny'---------------------------------------------------------
fn1 <- "GBPUSD.txt"
dat <- read.table(file=fn1,sep=",",header=T,as.is=T)
head(dat)
tm <- strptime(
  paste(dat[,"Date"], sprintf("%04d",dat[,"Time"])),
  format="%m/%d/%Y %H%M")
class(tm)
head(tm)

GBP <- xts(x=dat[,c("Open","High","Low","Close")],order.by=tm)
GBP <- GBP['2007']
first(GBP,'4 hours')
barChart(GBP,TA='addSMA(n = 7, col = "red");addSMA(n = 44, col = "blue")',
  subset='2007-12-24/2007-12-26',theme="white",name="GBPUSD")

chart_Series(GBP,subset='2007-12-24/2007-12-26',theme=myTheme,name="GBPUSD",
  TA='add_SMA(n=7,col="red",lwd=2);add_SMA(n=44,col="blue",lwd=2)')
#
# find cross-over bar
#
fastMA <- SMA(Cl(GBP),n=7)
slowMA <- SMA(Cl(GBP),n=44)
co <- fastMA > slowMA
x <- which(co['2007-12-24/2007-12-26'])[1]
#
# identify cross-over bar
#
ss <- GBP['2007-12-24/2007-12-26']
add_TA(ss[x,"Low"]-0.0005,pch=17,type="p",col="red", on=1,cex=2)
#
text(x=x,y=ss[x,"Low"]-0.0005,"Crossover\nbar",pos=1)



## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#                                                                       #
#      Blotter  (package:blotter)                                       #
#                                                                       #
#########################################################################

options(width=81,continue=" ",digits=8)


## ----cache=FALSE---------------------------------------------------------
library(blotter)
search()


## ------------------------------------------------------------------------
ls()
ls(all=T)


## ------------------------------------------------------------------------
args(currency)
args(stock)


## ----echo=FALSE,results='hide'-------------------------------------------
suppressWarnings(try(rm(list=c("account.bFaber","portfolio.bFaber"),pos=.blotter),silent=TRUE))
suppressWarnings(try(rm(list=c("b.strategy","myTheme","SPY",".getSymbols")),silent=TRUE))


## ------------------------------------------------------------------------
currency("USD")
stock("SPY",currency="USD",multiplier=1)
ls(all=T)
ls(envir=FinancialInstrument:::.instrument)


## ------------------------------------------------------------------------
get("USD",envir=FinancialInstrument:::.instrument)
get("SPY",envir=FinancialInstrument:::.instrument)


## ------------------------------------------------------------------------
# system settings
initDate <- '1997-12-31'
startDate <- '1998-01-01'
endDate <-  '2014-06-30'
initEq <- 1e6

## ------------------------------------------------------------------------
Sys.setenv(TZ="UTC")

## ----echo=FALSE,results='hide'-------------------------------------------
if(file.exists("SPY.RData"))
{
  load("SPY.RData")
} else {
  getSymbols('SPY', from=startDate, to=endDate, index.class="POSIXct", adjust=T)
  save(list="SPY",file="SPY.RData")
}

## ----eval=FALSE----------------------------------------------------------
## getSymbols('SPY', from=startDate, to=endDate, index.class="POSIXct", adjust=T)


## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=120,digits=6)

## ------------------------------------------------------------------------
SPY=to.monthly(SPY, indexAt='endof', drop.time=FALSE)
SPY$SMA10m <- SMA(Cl(SPY), 10)
tail(SPY)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=81,digits=8)


## ------------------------------------------------------------------------
args(initPortf)


## ------------------------------------------------------------------------
args(initAcct)


## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=120,digits=6)

## ------------------------------------------------------------------------
b.strategy <- "bFaber"
initPortf(b.strategy, 'SPY', initDate=initDate)
initAcct(b.strategy, portfolios=b.strategy, initDate=initDate, initEq=initEq)
initDate
first(SPY)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=81,digits=8)


## ------------------------------------------------------------------------
ls()
ls(.blotter)
ls(envir=FinancialInstrument:::.instrument)


## ----FABER,cache=FALSE---------------------------------------------------
# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

# plot OHLC series
chart_Series(
  x=SPY,
  theme=myTheme,
  name="SPY",
  TA="add_SMA(n=10,col=4)"
  )


## ----results='hide',size='tiny'------------------------------------------
for( i in 1:nrow(SPY) )
{
  # update values for this date
  CurrentDate <- time(SPY)[i]
  equity = getEndEq(b.strategy, CurrentDate)
  ClosePrice <- as.numeric(Cl(SPY[i,]))
  Posn <- getPosQty(b.strategy, Symbol='SPY', Date=CurrentDate)
  UnitSize = as.numeric(trunc(equity/ClosePrice))
  MA <- as.numeric(SPY[i,'SMA10m'])
  # change market position if necessary
  if( !is.na(MA) ) # if the moving average has begun
  {
    if( Posn == 0 ) { # No position, test to go Long
      if( ClosePrice > MA ) {
        # enter long position
        addTxn(b.strategy, Symbol='SPY', TxnDate=CurrentDate,
          TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0) }
    } else { # Have a position, so check exit
      if( ClosePrice < MA ) {
        # exit position
        addTxn(b.strategy, Symbol='SPY', TxnDate=CurrentDate,
          TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0)
      } else {
        if( i==nrow(SPY) ) # exit on last day
          addTxn(b.strategy, Symbol='SPY', TxnDate=CurrentDate,
            TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0)
      }
    }
  }
  updatePortf(b.strategy,Dates=CurrentDate)
  updateAcct(b.strategy,Dates=CurrentDate)
  updateEndEq(b.strategy,CurrentDate)
} # End dates loop


## ----echo=F--------------------------------------------------------------
options(width=120)

## ----size='tiny'---------------------------------------------------------
getTxns(Portfolio=b.strategy, Symbol="SPY")

## ----echo=F--------------------------------------------------------------
options(width=81)


## ------------------------------------------------------------------------
args(updatePortf)


## ------------------------------------------------------------------------
args(updateAcct)


## ------------------------------------------------------------------------
args(updateEndEq)


## ----size='Tiny'---------------------------------------------------------
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate(b.strategy,b.strategy)


## ----PERF,cache=FALSE----------------------------------------------------
args(chart.Posn)
chart.Posn(b.strategy, Symbol = 'SPY',theme=myTheme,
  TA='add_SMA(n=10,col=4, on=1)')


## ------------------------------------------------------------------------
tstats <- tradeStats(Portfolio=b.strategy)

## ----TSTATS1,echo=FALSE,cache=FALSE--------------------------------------
textplot(t(tstats)[1:15,,drop=FALSE],show.colnames=FALSE,halign='left')

## ----TSTATS2,echo=FALSE,cache=FALSE--------------------------------------
textplot(t(tstats)[16:30,,drop=FALSE],show.colnames=FALSE,halign='left')


## ------------------------------------------------------------------------
# trade related
tab.trades <- cbind(
  c("Trades","Win Percent","Loss Percent","W/L Ratio"),
  c(tstats[,"Num.Trades"],tstats[,c("Percent.Positive","Percent.Negative")],
  tstats[,"Percent.Positive"]/tstats[,"Percent.Negative"]))

# profit related
tab.profit <- cbind(
  c("Net Profit","Gross Profits","Gross Losses","Profit Factor"),
  c(tstats[,c("Net.Trading.PL","Gross.Profits","Gross.Losses",
    "Profit.Factor")]))

# averages
tab.wins <- cbind(
  c("Avg Trade","Avg Win","Avg Loss","Avg W/L Ratio"),
  c(tstats[,c("Avg.Trade.PL","Avg.Win.Trade","Avg.Losing.Trade",
    "Avg.WinLoss.Ratio")]))

trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)


## ----label=trade.stats.tab,echo=F,results='asis',cache=FALSE-------------
library(xtable)
print(xtable(trade.stats.tab, digits = c(0,0,2,0,2,0,2)),
  include.rownames = F,include.colnames=F, size="footnotesize")


## ------------------------------------------------------------------------
pts <- perTradeStats(Portfolio=b.strategy)

## ----PTSTATS1,echo=FALSE,cache=FALSE,fig.width=7,	fig.height=3-----------
gplots:::textplot(pts[,1:7],show.rownames=FALSE,halign="right")

## ----PTSTATS2,echo=FALSE,cache=FALSE,fig.width=12,	fig.height=4----------
pts[,8:15] <- round(pts[,8:15],4)
gplots:::textplot(pts[,8:15],,show.rownames=FALSE,halign="right")


## ----PERF2,cache=FALSE---------------------------------------------------
library(PerformanceAnalytics)
rets <- PortfReturns(Account=b.strategy)
rownames(rets) <- NULL
tail(rets)
charts.PerformanceSummary(rets,colorset = bluefocus)


## ------------------------------------------------------------------------
args(table.Arbitrary)


## ------------------------------------------------------------------------
tab.perf <- table.Arbitrary(rets,
  metrics=c(
    "Return.cumulative",
    "Return.annualized",
    "SharpeRatio.annualized",
    "CalmarRatio"),
  metricsNames=c(
    "Cumulative Return",
    "Annualized Return",
    "Annualized Sharpe Ratio",
    "Calmar Ratio"))
tab.perf


## ------------------------------------------------------------------------
tab.risk <- table.Arbitrary(rets,
  metrics=c(
    "StdDev.annualized",
    "maxDrawdown",
    "VaR",
    "ES"),
  metricsNames=c(
    "Annualized StdDev",
    "Max DrawDown",
    "Value-at-Risk",
    "Conditional VaR"))
tab.risk


## ------------------------------------------------------------------------
performance.stats.tab <- data.frame(
  rownames(tab.perf),tab.perf[,1],
  rownames(tab.risk),tab.risk[,1])

## ----label=performance.stats.tab,echo=F,results='asis'-------------------
print(xtable(performance.stats.tab, digits = c(0,0,3,0,3)),
  include.rownames = F,include.colnames=F, size="normalsize")


## ----results='hide'------------------------------------------------------
# remove objects to allow re-runs
suppressWarnings(try(rm(list=c("account.buyHold","portfolio.buyHold"),pos=.blotter),silent=TRUE))
# initialize portfolio and account
initPortf("buyHold", 'SPY', initDate=initDate)
initAcct("buyHold", portfolios="buyHold",
  initDate=initDate, initEq=initEq)
# place an entry order
CurrentDate <- time(getTxns(Portfolio=b.strategy, Symbol="SPY"))[2]
equity = getEndEq("buyHold", CurrentDate)
ClosePrice <- as.numeric(Cl(SPY[CurrentDate,]))
UnitSize = as.numeric(trunc(equity/ClosePrice))
addTxn("buyHold", Symbol='SPY', TxnDate=CurrentDate, TxnPrice=ClosePrice,
  TxnQty = UnitSize , TxnFees=0)
# place an exit order
LastDate <- last(time(SPY))
LastPrice <- as.numeric(Cl(SPY[LastDate,]))
addTxn("buyHold", Symbol='SPY', TxnDate=LastDate, TxnPrice=LastPrice,
  TxnQty = -UnitSize , TxnFees=0)
# update portfolio and account
updatePortf(Portfolio="buyHold")
updateAcct(name="buyHold")
updateEndEq(Account="buyHold")

## ----BUYANDHOLD,cache=FALSE----------------------------------------------
chart.Posn("buyHold", Symbol = 'SPY', Dates = '1998::',theme=myTheme)


## ----FABERBUYHOLDCOMP,cache=FALSE----------------------------------------
rets.bh <- PortfReturns(Account="buyHold")
returns <- cbind(rets,rets.bh)
colnames(returns) <- c("Faber","BuyHold")
returns["2011"]
charts.PerformanceSummary(returns, geometric=FALSE, wealth.index=TRUE)


## ----FBHRISKRETURN,cache=FALSE-------------------------------------------
table.AnnualizedReturns(returns)
chart.RiskReturnScatter(returns, Rf = 0, add.sharpe = c(1, 2), xlim=c(0,0.25),
  main = "Return versus Risk", colorset = c("red","blue"))


## ----FBHRELATIVE,cache=FALSE---------------------------------------------
table.Stats(returns)
chart.RelativePerformance(returns[,1],returns[,2],
  colorset = c("red","blue"), lwd = 2, legend.loc = "topleft")


## ------------------------------------------------------------------------
thePortfolio = getPortfolio(b.strategy)
names(thePortfolio)
names(thePortfolio$symbols)
names(thePortfolio$symbols$SPY)
names(thePortfolio$summary)


## ----echo=F--------------------------------------------------------------
options(width=105)

## ----size='tiny'---------------------------------------------------------
thePortfolio$symbols$SPY$txn[1:12,]

## ----echo=F--------------------------------------------------------------
options(width=81)


## ----BLOTPOSPL,cache=FALSE-----------------------------------------------
library(lattice)
xyplot(thePortfolio$symbols$SPY$posPL.USD,type="h",col=4)

## ----BLOTSUM,cache=FALSE-------------------------------------------------
xyplot(thePortfolio$summary,type="h",col=4)


## ------------------------------------------------------------------------
args(str)


## ----size='Tiny'---------------------------------------------------------
str(thePortfolio)


## ----ACCTSUM,cache=FALSE-------------------------------------------------
theAccount = getAccount(b.strategy)
names(theAccount)
names(theAccount$portfolios)
names(theAccount$portfolios$bFaber)
names(theAccount$summary)
xyplot(theAccount$summary)



## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE,cache=FALSE,size='scriptsize',
  fig.path='figures/',fig.show='hide',fig.keep='last',
  fig.align='center',	fig.width=7,	fig.height=5,
  message=FALSE,warning=FALSE)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=81,continue=" ",digits=8)


## ------------------------------------------------------------------------
library(quantstrat)
search()


## ----echo=F,results='hide'-----------------------------------------------
suppressWarnings(try(rm(list=ls(FinancialInstrument:::.instrument),pos=FinancialInstrument:::.instrument),silent=TRUE))


## ------------------------------------------------------------------------
currency("USD")
stock("SPY",currency="USD",multiplier=1)
ls(envir=FinancialInstrument:::.instrument)
ls(all=T)


## ------------------------------------------------------------------------
# system settings
initDate <- '1997-12-31'
startDate <- '1998-01-01'
endDate <-  '2014-06-30'
initEq <- 1e6

## ------------------------------------------------------------------------
Sys.setenv(TZ="UTC")

## ----echo=FALSE,results='hide'-------------------------------------------
if(file.exists("SPY.RData"))
{
  load("SPY.RData")
} else {
  getSymbols('SPY', from=startDate, to=endDate, index.class="POSIXct", adjust=T)
  save(list="SPY",file="SPY.RData")
}

## ----eval=FALSE----------------------------------------------------------
## getSymbols('SPY', from=startDate, to=endDate, index.class="POSIXct", adjust=T)

## ------------------------------------------------------------------------
SPY=to.monthly(SPY, indexAt='endof', drop.time=FALSE)
SPY$SMA10m <- SMA(Cl(SPY), 10)


## ------------------------------------------------------------------------
# inz portfolio, account
qs.strategy <- "qsFaber"

## ------------------------------------------------------------------------
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run

## ----results='hide'------------------------------------------------------
initPortf(qs.strategy,'SPY', initDate=initDate)

## ----results='hide'------------------------------------------------------
initAcct(qs.strategy,portfolios=qs.strategy, initDate=initDate, initEq=initEq)


## ------------------------------------------------------------------------
# initialize orders container
args(initOrders)
initOrders(portfolio=qs.strategy,initDate=initDate)

# instantiate a new strategy object
args(strategy)
strategy(qs.strategy,store=TRUE)


## ------------------------------------------------------------------------
ls(all=T)
ls(.blotter)
ls(.strategy)


## ------------------------------------------------------------------------
args(getStrategy)
strat <-getStrategy(qs.strategy)
class(strat)
summary(strat)


## ------------------------------------------------------------------------
args(add.indicator)


## ----results='hide'------------------------------------------------------
add.indicator(strategy = qs.strategy, name = "SMA",
  arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")

## ------------------------------------------------------------------------
summary(getStrategy(qs.strategy))


## ------------------------------------------------------------------------
args(add.signal)


## ----results='hide'------------------------------------------------------
add.signal(qs.strategy,name="sigCrossover",
  arguments = list(columns=c("Close","SMA10"),relationship="gt"),
  label="Cl.gt.SMA")


## ----results='hide'------------------------------------------------------
add.signal(qs.strategy,name="sigCrossover",
  arguments = list(columns=c("Close","SMA10"),relationship="lt"),
  label="Cl.lt.SMA")


## ------------------------------------------------------------------------
summary(getStrategy(qs.strategy))


## ------------------------------------------------------------------------
args(add.rule)


## ------------------------------------------------------------------------
args(ruleSignal)


## ----results='hide'------------------------------------------------------
#   go long when close > MA
add.rule(qs.strategy, name='ruleSignal',
  arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=900,
  ordertype='market', orderside='long'),
  type='enter')

## ----results='hide'------------------------------------------------------
#   exit when close < MA
add.rule(qs.strategy, name='ruleSignal',
  arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
  ordertype='market', orderside='long'),
  type='exit')


## ------------------------------------------------------------------------
summary(getStrategy(qs.strategy))


## ------------------------------------------------------------------------
args(applyStrategy)


## ----results='hide'------------------------------------------------------
applyStrategy(strategy=qs.strategy , portfolios=qs.strategy)

## ----echo=F--------------------------------------------------------------
options(width=120)

## ----size='tiny'---------------------------------------------------------
getTxns(Portfolio=qs.strategy, Symbol="SPY")

## ----echo=F--------------------------------------------------------------
options(width=81)


## ----echo=FALSE----------------------------------------------------------
options(width=120,digits=6)

## ----size='Tiny'---------------------------------------------------------
mktdata["2002"]

## ----echo=FALSE----------------------------------------------------------
options(width=81,digits=8)


## ----results='hide'------------------------------------------------------
updatePortf(qs.strategy)

## ----results='hide'------------------------------------------------------
updateAcct(qs.strategy)

## ----results='hide'------------------------------------------------------
updateEndEq(qs.strategy)


## ----size='Tiny'---------------------------------------------------------
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate(qs.strategy,qs.strategy)


## ------------------------------------------------------------------------
# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

## ----PERFQS,cache=FALSE--------------------------------------------------
# plot performance
chart.Posn(qs.strategy, Symbol = 'SPY', Dates = '1998::',theme=myTheme,
  TA='add_SMA(n=10,col=4, on=1, lwd=2)')


## ------------------------------------------------------------------------
tstats <- t(tradeStats(qs.strategy))

## ----TSTATS1,echo=FALSE,cache=FALSE--------------------------------------
textplot(tstats[1:15,,drop=FALSE],show.colnames=FALSE,halign='left')

## ----TSTATS2,echo=FALSE,cache=FALSE--------------------------------------
textplot(tstats[16:30,,drop=FALSE],show.colnames=FALSE,halign='left')


## ------------------------------------------------------------------------
ob <- getOrderBook(qs.strategy)
class(ob)
names(ob)
names(ob$qsFaber)
names(ob$qsFaber$SPY)


## ----echo=FALSE----------------------------------------------------------
options(width=110)

## ----size='tiny'---------------------------------------------------------
ob$qsFaber$SPY[,1:5]

## ----echo=FALSE----------------------------------------------------------
options(width=81)


## ----echo=FALSE----------------------------------------------------------
options(width=110)

## ----size='tiny'---------------------------------------------------------
ob$qsFaber$SPY[,6:11]

## ----echo=FALSE----------------------------------------------------------
options(width=81)


## ----echo=FALSE----------------------------------------------------------
options(width=100)

## ----size='tiny'---------------------------------------------------------
perTradeStats(qs.strategy)

## ----echo=FALSE----------------------------------------------------------
options(width=81)


## ----FABERMAE,cache=FALSE------------------------------------------------
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MAE', scale='percent')

## ----FABERMFE,cache=FALSE------------------------------------------------
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MFE', scale='percent')


## ----echo=FALSE----------------------------------------------------------
options(width=105)

## ----ACCTSUM,cache=FALSE,size='tiny'-------------------------------------
a <- getAccount(qs.strategy)
last(a$summary,5)
library(lattice)
xyplot(a$summary,type="h",col=4)

## ----echo=FALSE----------------------------------------------------------
options(width=81)


## ------------------------------------------------------------------------
equity <- a$summary$End.Eq

## ----EQCURVE,cache=FALSE-------------------------------------------------
plot(equity,main="Faber Strategy Equity Curve")

## ------------------------------------------------------------------------
ret <- Return.calculate(equity,method="log")

## ----PERFSUM,cache=FALSE-------------------------------------------------
charts.PerformanceSummary(ret, colorset = bluefocus,
  main="Faber Strategy Performance")


## ------------------------------------------------------------------------
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")

## ----echo=FALSE,results='hide'-------------------------------------------
if(file.exists("XLX.RData"))
{
  load("XLX.RData")
} else {
  getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from=startDate, to=endDate, adjust=T)
  save(list=symbols,file="XLX.RData")
}

## ----eval=FALSE----------------------------------------------------------
## getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
##   from=startDate, to=endDate, adjust=T)

## ------------------------------------------------------------------------
for(symbol in symbols)
{
    stock(symbol, currency="USD",multiplier=1)
    x<-get(symbol)
    x<-to.monthly(x,indexAt='endof',drop.time=FALSE)
    indexFormat(x)<-'%Y-%m-%d'
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
}


## ----results='hide'------------------------------------------------------
multi.asset <- "multiAsset"
rm.strat(multi.asset) # remove strategy etc. if this is a re-run

## ----results='hide'------------------------------------------------------
initPortf(multi.asset,symbols=symbols, initDate=initDate)
initAcct(multi.asset,portfolios=multi.asset, initDate=initDate,
  initEq=initEq)
initOrders(portfolio=multi.asset,initDate=initDate)


## ----results='hide'------------------------------------------------------
applyStrategy(strategy=qs.strategy , portfolios=multi.asset)
updatePortf(multi.asset)
updateAcct(multi.asset)
updateEndEq(multi.asset)

## ------------------------------------------------------------------------
checkBlotterUpdate(multi.asset,multi.asset)


## ------------------------------------------------------------------------
a <- getAccount(multi.asset)
p <- getPortfolio(multi.asset)
names(p$symbols)


## ----XLX3x3,cache=FALSE,dev.args=list(pointsize=10),fig.width=12, fig.height=9----
par(mfrow=c(3,3))
for(symbol in symbols)
{
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
    TA="add_SMA(n=10,col='blue')")
}
par(mfrow=c(1,1))


## ----MULTITRADESTATS,echo=FALSE,cache=FALSE------------------------------
textplot(t(tradeStats(multi.asset)))


## ----echo=FALSE----------------------------------------------------------
options(width=78)

## ----IASSRET,cache=FALSE-------------------------------------------------
rets.multi <- PortfReturns(multi.asset)
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi,Return.calculate(a$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "TOTAL"
rets.multi <- rets.multi[,c("TOTAL",symbols)]
round(tail(rets.multi,5),6)
chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft",
  main="SPDR Cumulative Returns")

## ----echo=FALSE----------------------------------------------------------
options(width=81)


## ----SPDRBOX,cache=FALSE-------------------------------------------------
chart.Boxplot(rets.multi, main = "SPDR Returns", colorset= rich10equal)


## ----MULTIRETRISK,cache=FALSE--------------------------------------------
(ar.tab <- table.AnnualizedReturns(rets.multi))
max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])

chart.RiskReturnScatter(rets.multi,
  main = "SPDR Performance", colorset = rich10equal,
  xlim=c(0,max.risk*1.1),ylim=c(0,max.return))


## ------------------------------------------------------------------------
equity <- a$summary$End.Eq

## ----MULTIEQCURVE,cache=FALSE--------------------------------------------
plot(equity,main="Consolidated SPDR Equity Curve")



## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE,cache=FALSE,size='scriptsize',
  fig.path='figures/',fig.show='hide',fig.keep='last',
  fig.align='center',	fig.width=7,	fig.height=5,
  message=FALSE,warning=FALSE)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=81,continue=" ",digits=8)


## ----results='hide'------------------------------------------------------
library(quantstrat)
startDate <- '2010-01-01'  # start of data
endDate <-  '2013-07-31'   # end of data
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
Sys.setenv(TZ="UTC")       # set time zone

## ----echo=FALSE,results='hide'-------------------------------------------
if(file.exists("XLX.RData"))
{
  load("XLX.RData")
} else {
  getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
    from=startDate, to=endDate, adjust=TRUE)
  save(list=symbols,file="XLX.RData")
}

## ----eval=FALSE----------------------------------------------------------
## getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
##   from=startDate, to=endDate, adjust=TRUE)


## ------------------------------------------------------------------------
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

## ----XLX3x3,cache=FALSE,fig.height=6,fig.width=9-------------------------
par(mfrow=c(3,3))
for(symbol in symbols)
{
  plot(chart_Series(get(symbol),name=symbol,theme=myTheme))
}
par(mfrow=c(1,1))


## ----results='hide'------------------------------------------------------
initDate <- '2009-12-31'
initEq <- 1e6
currency("USD")
stock(symbols, currency="USD",multiplier=1)


## ----XLFBB,cache=FALSE---------------------------------------------------
args(BBands)
b <- BBands(HLC=HLC(XLF["2013"]), n=20, sd=2)
tail(b)
chart_Series(XLF["2013"],TA='add_BBands(lwd=2)',theme=myTheme,name="XLF")


## ----results='hide'------------------------------------------------------
rm.strat("multiAsset.bb1") # remove portfolio, account, orderbook if re-run
initPortf(name="multiAsset.bb1", symbols, initDate=initDate)
initAcct(name="multiAsset.bb1", portfolios="multiAsset.bb1",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb1", initDate=initDate)


## ----results='hide'------------------------------------------------------
strategy("bbands", store=TRUE)

## ------------------------------------------------------------------------
args(BBands)

## ----results='hide'------------------------------------------------------
add.indicator("bbands", name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='BBands')


## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("Close","up"),relationship="gt"),
  label="Cl.gt.UpperBand")

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("Close","dn"),relationship="lt"),
  label="Cl.lt.LowerBand")

## ----results='hide'------------------------------------------------------
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("High","Low","mavg"),relationship="op"),
  label="Cross.Mid")


## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100,
  ordertype='market', orderside=NULL),type='enter')

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100,
  ordertype='market', orderside=NULL),type='enter')

## ----results='hide'------------------------------------------------------
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',
  ordertype='market', orderside=NULL),type='exit')


## ----results='hide'------------------------------------------------------
SD = 2
N = 20

## ----results='hide'------------------------------------------------------
out <- applyStrategy("bbands",
  portfolios="multiAsset.bb1",parameters=list(sd=SD,n=N))


## ----results='hide'------------------------------------------------------
updatePortf("multiAsset.bb1")
updateAcct("multiAsset.bb1")
updateEndEq("multiAsset.bb1")


## ----size='Tiny'---------------------------------------------------------
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate("multiAsset.bb1","multiAsset.bb1")


## ----XLBCP,cache=FALSE---------------------------------------------------
chart.Posn("multiAsset.bb1","XLB",TA="add_BBands(n=20,sd=2)",theme=myTheme)

## ----XLBCP2,cache=FALSE--------------------------------------------------
chart.Posn("multiAsset.bb1","XLB",TA="add_BBands(n=20,sd=2)",
  Dates="2010",theme=myTheme)


## ----results='hide'------------------------------------------------------
rm.strat("multiAsset.bb2") # remove portfolio, account, orderbook if re-run
initPortf(name="multiAsset.bb2", symbols, initDate=initDate)
initAcct(name="multiAsset.bb2", portfolios="multiAsset.bb2",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb2", initDate=initDate)

## ----results='hide'------------------------------------------------------
SD=3
out <- applyStrategy("bbands",
  portfolios="multiAsset.bb2",parameters=list(sd=SD,n=N))

## ----results='hide'------------------------------------------------------
updatePortf("multiAsset.bb2")
updateAcct("multiAsset.bb2")
updateEndEq("multiAsset.bb2")

## ------------------------------------------------------------------------
checkBlotterUpdate("multiAsset.bb2","multiAsset.bb2")


## ----BBCUMRET,cache=FALSE------------------------------------------------
eq1 <- getAccount("multiAsset.bb1")$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
eq2 <- getAccount("multiAsset.bb2")$summary$End.Eq
rt2 <- Return.calculate(eq2,"log")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("SD=2","SD=3")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
  main="BBand SD Parameter Comparison",ylab="cum return",xlab="",
  minor.ticks=FALSE)


## ------------------------------------------------------------------------
args(ruleSignal)


## ------------------------------------------------------------------------
args(osNoOp)


## ------------------------------------------------------------------------
osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}


## ----XLVMACD,cache=FALSE-------------------------------------------------
args(MACD)
macd  <- MACD( Cl(XLV), 12, 26, 9, maType="EMA" )
tail(macd,3)
chart_Series(XLV,
  TA="add_MACD();add_EMA(12,col='darkgreen');add_EMA(26,col='blue')",
  subset="20100717::20101208",theme=myTheme)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd") # remove portfolio, account, orderbook if re-run
initPortf(name="multi.macd", symbols, initDate=initDate)
initAcct(name="multi.macd", portfolios="multi.macd",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd", initDate=initDate)


## ----results='hide'------------------------------------------------------
strategy("macd", store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator("macd", name = "MACD",
  arguments = list(x=quote(Cl(mktdata))),label='osc')

## ----results='hide'------------------------------------------------------
add.signal("macd",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
  label="signal.gt.zero")

## ----results='hide'------------------------------------------------------
add.signal("macd",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
  label="signal.lt.zero")


## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero",sigval=TRUE,orderqty=100,
  ordertype='market',orderside='long',osFUN='osFixedDollar'),
  type='enter',label='enter',storefun=FALSE)

## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
  arguments = list(sigcol="signal.lt.zero",sigval=TRUE,orderqty='all',
  ordertype='market',orderside='long'),
  type='exit',label='exit')


## ----results='hide'------------------------------------------------------
fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
tradeSize <- initEq/10

## ----results='hide'------------------------------------------------------
out<-applyStrategy("macd" , portfolios="multi.macd",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)


## ----results='hide'------------------------------------------------------
updatePortf("multi.macd")
updateAcct("multi.macd")
updateEndEq("multi.macd")

## ------------------------------------------------------------------------
checkBlotterUpdate("multi.macd","multi.macd")


## ----MACDCPOSN1,cache=FALSE----------------------------------------------
chart.Posn(Portfolio="multi.macd",Symbol="XLV",theme=myTheme)

## ----MACDCPOSN2,cache=FALSE----------------------------------------------
chart.Posn(Portfolio="multi.macd",Symbol="XLV",
  Dates="201006::20101213",theme=myTheme)
add_MACD()
add_EMA(12,col='red')
add_EMA(26,col='blue')


## ----echo=FALSE----------------------------------------------------------
options(width=105)

## ----size='tiny'---------------------------------------------------------
perTradeStats("multi.macd","XLF")

## ----echo=FALSE----------------------------------------------------------
options(width=82)


## ----results='hide'------------------------------------------------------
strategy("bb.lim", store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator("bb.lim", name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='BBands')

## ----results='hide'------------------------------------------------------
add.signal("bb.lim", name="sigCrossover",
  arguments=list(columns=c("Close","up"),relationship="gt"),
  label="Cl.gt.UpperBand")

## ----results='hide'------------------------------------------------------
add.signal("bb.lim", name="sigCrossover",
  arguments=list(columns=c("Close","dn"),relationship="lt"),
  label="Cl.lt.LowerBand")

## ----results='hide'------------------------------------------------------
add.signal("bb.lim", name="sigCrossover",
  arguments=list(columns=c("High","Low","mavg"),relationship="op"),
  label="Cross.Mid")


## ----results='hide'------------------------------------------------------
add.rule("bb.lim", name='ruleSignal',
  arguments=list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-1000,
  ordertype='market', orderside=NULL, osFUN='osMaxPos'),
  type='enter')

## ----results='hide'------------------------------------------------------
add.rule("bb.lim", name='ruleSignal',
  arguments=list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 1000,
  ordertype='market', orderside=NULL, osFUN='osMaxPos'),
  type='enter')

## ----results='hide'------------------------------------------------------
add.rule("bb.lim", name='ruleSignal',
  arguments=list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',
  ordertype='market', orderside=NULL),type='exit')


## ------------------------------------------------------------------------
args(addPosLimit)


## ----results='hide'------------------------------------------------------
rm.strat("multi.bb.limit") # remove portfolio, account, orderbook if re-run
initPortf(name="multi.bb.limit", symbols, initDate=initDate)
initAcct(name="multi.bb.limit", portfolios="multi.bb.limit",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.bb.limit", initDate=initDate)

## ------------------------------------------------------------------------
for(symbol in symbols)
{
  addPosLimit("multi.bb.limit", symbol, initDate, 200, 2 )
}


## ----results='hide'------------------------------------------------------
SD = 2
N = 20

## ----results='hide'------------------------------------------------------
out <- applyStrategy("bb.lim",
  portfolios="multi.bb.limit",parameters=list(sd=SD,n=N))

## ----results='hide'------------------------------------------------------
updatePortf("multi.bb.limit")
updateAcct("multi.bb.limit")
updateEndEq("multi.bb.limit")

## ------------------------------------------------------------------------
checkBlotterUpdate("multi.bb.limit","multi.bb.limit")

## ----XLBCPLIM,cache=FALSE------------------------------------------------
chart.Posn("multi.bb.limit","XLB",TA="add_BBands(n=20,sd=2)",theme=myTheme)

## ----XLBCPLIM2,cache=FALSE-----------------------------------------------
chart.Posn("multi.bb.limit","XLB",TA="add_BBands(n=20,sd=2)",
  Dates="2010",theme=myTheme)


## ----results='hide'------------------------------------------------------
strategy("faber",store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator(strategy = "faber", name = "SMA",
  arguments = list(x = quote(Cl(mktdata))), label="SMAn")

## ----results='hide'------------------------------------------------------
add.signal("faber",name="sigCrossover",
  arguments = list(columns=c("Close","SMAn"),relationship="gt"),
  label="Cl.gt.SMA")

## ----results='hide'------------------------------------------------------
add.signal("faber",name="sigCrossover",
  arguments = list(columns=c("Close","SMAn"),relationship="lt"),
  label="Cl.lt.SMA")


## ----results='hide'------------------------------------------------------
add.rule("faber", name='ruleSignal',
  arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=100000,
  ordertype='market', orderside='long', osFUN='osMaxPos'),
  type='enter', path.dep=TRUE)

## ----results='hide'------------------------------------------------------
add.rule("faber", name='ruleSignal',
  arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
  ordertype='market', orderside='long', pricemethod='market'),
  type='exit', path.dep=TRUE)


## ------------------------------------------------------------------------
args(rulePctEquity)


## ----results='hide'------------------------------------------------------
# add quaterly rebalancing
add.rule('faber', 'rulePctEquity',
  arguments=list(rebalance_on='months',
    trade.percent=1/length(symbols),
    refprice=quote(
      last(getPrice(mktdata)[paste('::',as.character(curIndex),sep='')][,1])
      ),
    digits=0
  ),
  type='rebalance',
  label='rebalance'
)


## ----results='hide'------------------------------------------------------
rm.strat("multi.faber") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.faber", symbols, initDate=initDate)
initAcct(name="multi.faber", portfolios="multi.faber",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.faber", initDate=initDate)

## ------------------------------------------------------------------------
(posval <- initEq/length(symbols))
for(symbol in symbols){
    pos<-round((posval/first(getPrice(get(symbol)))),-2)
    addPosLimit('multi.faber',symbol,initDate, maxpos=pos,minpos=-pos)
}


## ----results='hide'------------------------------------------------------
out <- applyStrategy.rebalancing(strategy="faber", portfolios="multi.faber",
  parameters=list(n=200))

## ----results='hide'------------------------------------------------------
updatePortf("multi.faber")
updateAcct("multi.faber")
updateEndEq("multi.faber")

## ------------------------------------------------------------------------
checkBlotterUpdate("multi.faber","multi.faber")

## ----XLKCPF,cache=FALSE--------------------------------------------------
chart.Posn("multi.faber","XLF",TA="add_SMA(n=200)",theme=myTheme)


## ----echo=FALSE----------------------------------------------------------
options(width=125)

## ----size='Tiny'---------------------------------------------------------
(pts <- perTradeStats("multi.faber","XLU"))

## ----XLUBAR,cache=FALSE--------------------------------------------------
mnc <- pts$Max.Notional.Cost
pe <- sapply(pts$Start,getEndEq,Account="multi.faber")/9
barplot(rbind(pe,mnc),beside=T,col=c(2,4),names.arg=format(pts$Start,"%m/%d/%y"),
  ylim=c(0,1.5e5),ylab="$",xlab="Trade Date")
legend(x="topleft",legend=c("(Portfolio Equity)/9","Order Size"),
  pch=15,col=c(2,4),bty="n")
title("Percent of Portfolio Equity versus Trade Size for XLU")

## ----echo=FALSE----------------------------------------------------------
options(width=82)



## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE,cache=FALSE,size='scriptsize',
  fig.path='figures/',fig.show='hide',fig.keep='last',
  fig.align='center',	fig.width=7,	fig.height=5,
  message=FALSE,warning=FALSE)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=79,continue=" ",digits=8)


## ----results='hide'------------------------------------------------------
library(quantstrat)
startDate <- '2010-01-01'  # start of data
endDate <-  '2013-07-31'   # end of data
symbols = c("ITOT", "AGG", "GLD", "VNQ")
Sys.setenv(TZ="UTC")       # set time zone

## ----results='hide'------------------------------------------------------
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
  from=startDate, to=endDate,adjust=TRUE)

## ----results='hide'------------------------------------------------------
initDate <- '2009-12-31'
initEq <- 1e6
currency("USD")
stock(symbols, currency="USD",multiplier=1)


## ------------------------------------------------------------------------
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

## ----XLX3x3,cache=FALSE--------------------------------------------------
par(mfrow=c(2,2))
for(symbol in symbols)
{
  plot(chart_Series(get(symbol),name=symbol))
}
par(mfrow=c(1,1))


## ------------------------------------------------------------------------
osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}


## ----results='hide'------------------------------------------------------
strategy("macd", store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator("macd", name = "MACD",
  arguments = list(x=quote(Cl(mktdata))),label='osc')

## ----results='hide'------------------------------------------------------
add.signal("macd",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
  label="signal.gt.zero")

## ----results='hide'------------------------------------------------------
add.signal("macd",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
  label="signal.lt.zero")


## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='market',
    orderqty=100,
    osFUN='osFixedDollar',
    orderset='ocolong'
  ),
  type='enter',
  label='LE'
)


## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
  arguments = list(sigcol="signal.lt.zero", sigval=TRUE,
    replace=TRUE,
    orderside='long',
    ordertype='market',
    orderqty='all',
    orderset='ocolong'
  ),
  type='exit',
  label='LX'
)


## ----size='Tiny'---------------------------------------------------------
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.nostop") # remove portfolio, account, orderbook if re-run
initPortf(name="multi.macd.nostop", symbols, initDate=initDate)
initAcct(name="multi.macd.nostop", portfolios="multi.macd.nostop",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.nostop", initDate=initDate)

fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
tradeSize <- initEq/10

out<-applyStrategy("macd" , portfolios="multi.macd.nostop",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)

updatePortf("multi.macd.nostop")
updateAcct("multi.macd.nostop")
updateEndEq("multi.macd.nostop")


## ------------------------------------------------------------------------
checkBlotterUpdate("multi.macd.nostop","multi.macd.nostop")


## ----PERFNOSTOP,cache=FALSE----------------------------------------------
equity.curve <- getAccount("multi.macd.nostop")$summary$End.Eq
returns.ns <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.ns)
charts.PerformanceSummary(returns.ns,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (no stoploss)",minor.ticks=FALSE)

## ----TRADESTATSNOSTOP,results='hide',fig.width=9,fig.height=9------------
PerformanceAnalytics:::textplot(t(tradeStats("multi.macd.nostop")))

## ----MAEVNQNOSTOP,results='hide',fig.width=7,fig.height=7----------------
chart.ME("multi.macd.nostop",'VNQ',type='MAE',scale='percent')

## ----ORDERBOOKVNQNOSTOP,results='hide',fig.width=9,fig.height=7----------
ob <- getOrderBook("multi.macd.nostop")$multi.macd.nostop$VNQ
ob.df <- data.frame(Date=time(ob),coredata(ob),stringsAsFactors=FALSE)
ob.df$Order.Price <- round(as.numeric(ob.df$Order.Price),3)
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)


## ------------------------------------------------------------------------
args(add.rule)


## ------------------------------------------------------------------------
args(ruleSignal)


## ------------------------------------------------------------------------
stopLossPercent <- 0.05

## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoplimit',
    tmult=TRUE,
    threshold=quote( stopLossPercent ),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="LE",
  label='StopLossLong',
  enabled=FALSE
)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.stop") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.stop", symbols, initDate=initDate)
initAcct(name="multi.macd.stop", portfolios="multi.macd.stop",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.stop", initDate=initDate)

## ----results='hide'------------------------------------------------------
enable.rule("macd",type="chain",labe="StopLoss")

## ----results='hide'------------------------------------------------------
out<-applyStrategy("macd" , portfolios="multi.macd.stop",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)

## ----results='hide'------------------------------------------------------
updatePortf("multi.macd.stop")
updateAcct("multi.macd.stop")
updateEndEq("multi.macd.stop")

## ------------------------------------------------------------------------
checkBlotterUpdate("multi.macd.stop","multi.macd.stop")


## ----PERFSTOP------------------------------------------------------------
equity.curve <- getAccount("multi.macd.stop")$summary$End.Eq
returns.sl <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.sl)
charts.PerformanceSummary(returns.sl,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)

## ----TRADESTATSSTOP,results='hide',fig.width=9,fig.height=9--------------
PerformanceAnalytics:::textplot(t(tradeStats("multi.macd.stop")))

## ----MAEVNQSTOP,results='hide',fig.width=7,fig.height=7------------------
chart.ME("multi.macd.stop",'VNQ',type='MAE',scale='percent')

## ----ORDERBOOKVNQSTOP,results='hide',fig.width=9,fig.height=7------------
ob <- getOrderBook("multi.macd.stop")$multi.macd.stop$VNQ
ob.df <- data.frame(Date=time(ob),coredata(ob),stringsAsFactors=FALSE)
ob.df$Order.Price <- round(as.numeric(ob.df$Order.Price),3)
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)


## ------------------------------------------------------------------------
trailingStopPercent <- 0.07

## ----results='hide'------------------------------------------------------
add.rule("macd", name = 'ruleSignal',
  arguments=list(sigcol="signal.gt.zero" , sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoptrailing',
    tmult=TRUE,
    threshold=quote(trailingStopPercent),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="LE",
  label='StopTrailingLong',
  enabled=FALSE
)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.trail") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.trail", symbols, initDate=initDate)
initAcct(name="multi.macd.trail", portfolios="multi.macd.trail",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.trail", initDate=initDate)

## ----results='hide'------------------------------------------------------
enable.rule("macd",type="chain",labe="StopTrailingLong")

## ----results='hide'------------------------------------------------------
out<-applyStrategy("macd" , portfolios="multi.macd.trail",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)

## ----results='hide'------------------------------------------------------
updatePortf("multi.macd.trail")
updateAcct("multi.macd.trail")
updateEndEq("multi.macd.trail")

## ------------------------------------------------------------------------
checkBlotterUpdate("multi.macd.trail","multi.macd.trail")


## ----PERFTRAIL-----------------------------------------------------------
equity.curve <- getAccount("multi.macd.trail")$summary$End.Eq
returns.tr <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.tr)
charts.PerformanceSummary(returns.tr,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)

## ----TRADESTATSTRAIL,results='hide',fig.width=9,fig.height=9-------------
PerformanceAnalytics:::textplot(t(tradeStats("multi.macd.trail")))

## ----MAEVNQTRAIL,results='hide',fig.width=7,fig.height=7-----------------
chart.ME("multi.macd.trail",'VNQ',type='MAE',scale='percent')

## ----ORDERBOOKVNQTRAIL,results='hide',fig.width=9,fig.height=7-----------
ob <- getOrderBook("multi.macd.trail")$multi.macd.trail$VNQ
ob <- ob[ob$Order.Status=="closed",]
ob.df <- data.frame(Date=time(ob),coredata(ob),stringsAsFactors=FALSE)
ob.df$Order.Price <- round(as.numeric(ob.df$Order.Price),3)
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)


## ------------------------------------------------------------------------
stopLossPercentRange <- seq(0.03,0.05,by=0.01)

## ----results='hide'------------------------------------------------------
add.distribution("macd",
  paramset.label = "STOPOPT",
  component.type = "chain",
  component.label = "StopLossLong",
  variable = list( threshold = stopLossPercentRange ),
  label = "StopLossLONG"
)


## ------------------------------------------------------------------------
trailingPercentRange <- seq(0.03,0.05,by=0.01)

## ----results='hide'------------------------------------------------------
add.distribution("macd",
  paramset.label = "STOPOPT",
  component.type = "chain",
  component.label = "StopTrailingLong",
  variable = list( threshold = trailingPercentRange ),
  label = "TrailingLONG"
)


## ------------------------------------------------------------------------
args(apply.paramset)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.opt") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.opt", symbols, initDate=initDate)
initAcct(name="multi.macd.opt", portfolios="multi.macd.opt",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.opt", initDate=initDate)

## ------------------------------------------------------------------------
library(parallel)
detectCores()

## ----results='hide'------------------------------------------------------
if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
#  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}


## ----results='hide'------------------------------------------------------
if( file.exists("resultsStopOpt.RData") )
{
  load("resultsStopOpt.RData")
} else {
  results <- apply.paramset("macd", paramset.label = "STOPOPT",
    portfolio="multi.macd.opt", account="multi.macd.opt", nsamples=0)
  save(list="results",file="resultsStopOpt.RData")
}

## ------------------------------------------------------------------------
head(names(results),20)


## ----PROFITMDDHEAT,fig.width=7,fig.height=7,dev='png',dpi=300------------
z <- tapply(X=results$tradeStats$Profit.To.Max.Draw,
  INDEX=list(results$tradeStats$TrailingLONG,results$tradeStats$StopLossLONG),
  FUN=median)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,
  xlab="Trailing Stop",ylab="Stop Loss")
title("Return to MaxDrawdown")


## ----results='hide'------------------------------------------------------
strategy("macd.opt", store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator("macd.opt", name = "MACD",
  arguments = list(x=quote(Cl(mktdata))),label='osc')

## ----results='hide'------------------------------------------------------
add.signal("macd.opt",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
  label="signal.gt.zero")

## ----results='hide'------------------------------------------------------
add.signal("macd.opt",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
  label="signal.lt.zero")


## ----results='hide'------------------------------------------------------
add.rule("macd.opt",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='market',
    orderqty=100,
    osFUN='osFixedDollar',
    orderset='ocolong'
  ),
  type='enter',
  label='LE'
)

add.rule("macd.opt",name='ruleSignal',
  arguments = list(sigcol="signal.lt.zero", sigval=TRUE,
    replace=TRUE,
    orderside='long',
    ordertype='market',
    orderqty='all',
    orderset='ocolong'
  ),
  type='exit',
  label='LX'
)


## ------------------------------------------------------------------------
stopLossPercent <- 0.03

## ----results='hide'------------------------------------------------------
add.rule("macd.opt",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoplimit',
    tmult=TRUE,
    threshold=quote( stopLossPercent ),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="LE",
  label='StopLossLong',
  enabled=TRUE
)


## ------------------------------------------------------------------------
trailingStopPercent <- 0.03

## ----results='hide'------------------------------------------------------
add.rule("macd.opt", name = 'ruleSignal',
  arguments=list(sigcol="signal.gt.zero" , sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoptrailing',
    tmult=TRUE,
    threshold=quote(trailingStopPercent),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="LE",
  label='StopTrailingLong',
  enabled=TRUE
)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.opt") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.opt", symbols, initDate=initDate)
initAcct(name="multi.macd.opt", portfolios="multi.macd.opt",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.opt", initDate=initDate)

## ----results='hide'------------------------------------------------------
out<-applyStrategy("macd.opt" , portfolios="multi.macd.opt",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)

## ----results='hide'------------------------------------------------------
updatePortf("multi.macd.opt")
updateAcct("multi.macd.opt")
updateEndEq("multi.macd.opt")

## ------------------------------------------------------------------------
checkBlotterUpdate("multi.macd.opt","multi.macd.opt")


## ----PERFOPT-------------------------------------------------------------
equity.curve <- getAccount("multi.macd.opt")$summary$End.Eq
returns.opt <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.opt)
charts.PerformanceSummary(returns.opt,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)

## ----TRADESTATSOPT,results='hide',fig.width=9,fig.height=9---------------
PerformanceAnalytics:::textplot(t(tradeStats("multi.macd.opt")))

## ----MAEVNQOPT,results='hide',fig.width=7,fig.height=7-------------------
chart.ME("multi.macd.opt",'VNQ',type='MAE',scale='percent')

## ----ORDERBOOKVNQOPT,results='hide',fig.width=9,fig.height=7-------------
ob <- getOrderBook("multi.macd.opt")$multi.macd.opt$VNQ
ob <- ob[ob$Order.Status=="closed",]
ob.df <- data.frame(Date=time(ob),coredata(ob),stringsAsFactors=FALSE)
ob.df$Order.Price <- round(as.numeric(ob.df$Order.Price),3)
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)


## ----PERFOPTALL----------------------------------------------------------
rets <- cbind(returns.ns,returns.sl,returns.tr,returns.opt)
colnames(rets) <- c("NoStops","StopLoss","StopAndTrail","Optimal")
charts.PerformanceSummary(rets,main="Stop Comparision")

## ----RISKREWARDOPT,results='hide',fig.width=7,fig.height=7---------------
chart.RiskReturnScatter(rets,
main = "Risk Management Evolution", colorset = rich10equal)



## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE,cache=FALSE,size='scriptsize',
  fig.path='figures/',fig.show='hide',fig.keep='last',
  fig.align='center',	fig.width=7,	fig.height=5,
  message=FALSE,warning=FALSE)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=81,continue=" ",digits=8)


## ------------------------------------------------------------------------
Sys.setenv(TZ="UTC")
library(quantstrat)

## ------------------------------------------------------------------------
initDate = '2002-10-21'
.from=initDate
.to='2002-10-31'

## ----results='hide'------------------------------------------------------
currency(c('GBP', 'USD'))
exchange_rate('GBPUSD', tick_size=0.0001)

## ----results='hide'------------------------------------------------------
getSymbols.FI(Symbols='GBPUSD',
	      dir=system.file('extdata',package='quantstrat'),
	      from=.from, to=.to)

GBPUSD = to.minutes30(GBPUSD)
GBPUSD = align.time(GBPUSD, 1800)


## ----echo=FALSE----------------------------------------------------------
options(width=105)

## ----size='tiny'---------------------------------------------------------
dim(GBPUSD)
last(GBPUSD,5)

## ----echo=FALSE----------------------------------------------------------
options(width=81)

## ----GBPUSD--------------------------------------------------------------
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart_Series(GBPUSD,theme=myTheme)


## ------------------------------------------------------------------------
# moving average lengths
.fast = 10
.slow = 30

# optimization range
.FastSMA = (1:30)
.SlowSMA = (20:80)

# trade parameters
.threshold = 0.0005
.orderqty = 100000
.txnfees = -6  # round-trip fee

# stop loss amount
.stoploss <- 0.30/100
.StopLoss = seq(0.05, 0.6, length.out=48)/100

# trading window
.timespan = 'T00:00/T23:59'

# number of optimization samples
.nsamples=80


## ------------------------------------------------------------------------
portfolio.st = 'forex'
account.st = 'IB1'
strategy.st = 'luxor'

## ------------------------------------------------------------------------
rm.strat(portfolio.st)
rm.strat(account.st)

## ----results='hide'------------------------------------------------------
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)


## ----results='hide'------------------------------------------------------
add.indicator(strategy.st, name = "SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .fast
	),
	label="nFast"
)

## ----results='hide'------------------------------------------------------
add.indicator(strategy.st, name="SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .slow
	),
	label="nSlow"
)


## ----results='hide'------------------------------------------------------
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="gte"
	),
	label='long'
)

## ----results='hide'------------------------------------------------------
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="lt"
	),
	label='short'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name='ruleSignal',
    arguments=list(sigcol='long' , sigval=TRUE,
        orderside='long' ,
        ordertype='stoplimit',
        prefer='High',
        threshold=.threshold,
        orderqty=+.orderqty,
        replace=FALSE
        ),
    type='enter',
    label='EnterLONG'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name='ruleSignal',
    arguments=list(sigcol='short', sigval=TRUE,
        orderside='short',
        ordertype='stoplimit',
        prefer='Low',
        threshold=-.threshold,
        orderqty=-.orderqty,
        replace=FALSE
        ),
    type='enter',
    label='EnterSHORT'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name='ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		orderside='long' ,
		ordertype='market',
		orderqty='all',
		TxnFees=.txnfees,
		replace=TRUE
	),
	type='exit',
	label='Exit2SHORT'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name='ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		orderside='short',
		ordertype='market',
		orderqty='all',
		TxnFees=.txnfees,
		replace=TRUE
	),
	type='exit',
	label='Exit2LONG'
)


## ----results='hide'------------------------------------------------------
out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
  Dates=paste('::',as.Date(Sys.time()),sep=''))

## ----CHARTLUXOR1---------------------------------------------------------
chart.Posn(portfolio.st, "GBPUSD",
  TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)",theme=myTheme)

## ----LUXORSTATS1---------------------------------------------------------
PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st, 'GBPUSD')))

## ----MKTDATA-------------------------------------------------------------
mk <- mktdata['2002-10-23 15:00::2002-10-24 03:00']
mk.df <- data.frame(Date=time(mk),coredata(mk))
PerformanceAnalytics:::textplot(mk.df,show.rownames=F)

## ----ORDERBOOK,fig.width=9,	fig.height=6---------------------------------
ob <- getOrderBook(portfolio.st)$forex$GBPUSD
ob.df <- data.frame(Date=time(ob),coredata(ob))
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)

## ----PERTRADESTATS-------------------------------------------------------
PerformanceAnalytics:::textplot(perTradeStats(portfolio.st,"GBPUSD"),
  show.rownames=F)

## ----MAE,fig.width=7,	fig.height=7---------------------------------------
chart.ME(portfolio.st,'GBPUSD',type='MAE',scale='percent')


## ------------------------------------------------------------------------
args(add.distribution)

## ----results='hide'------------------------------------------------------
add.distribution(strategy.st,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nFast',
	variable = list(n = .FastSMA),
	label = 'nFAST'
)


## ----results='hide'------------------------------------------------------
add.distribution(strategy.st,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nSlow',
	variable = list(n = .SlowSMA),
	label = 'nSLOW'
)


## ------------------------------------------------------------------------
args(add.distribution.constraint)

## ----results='hide'------------------------------------------------------
add.distribution.constraint(strategy.st,
	paramset.label = 'SMA',
	distribution.label.1 = 'nFAST',
	distribution.label.2 = 'nSLOW',
	operator = '<',
	label = 'SMA'
)


## ------------------------------------------------------------------------
rm.strat(portfolio.st)
rm.strat(account.st)

## ----results='hide'------------------------------------------------------
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,
  initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)


## ------------------------------------------------------------------------
library(parallel)
detectCores()

## ----results='hide'------------------------------------------------------
if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

## ------------------------------------------------------------------------
foreach(i=1:8, .combine=c) %dopar% sqrt(i)

## ----echo=FALSE,results='hide'-------------------------------------------
if( Sys.info()['sysname'] == "Windows" )
{
  registerDoSEQ()
}

## ----results='hide'------------------------------------------------------


## ------------------------------------------------------------------------
args(apply.paramset)


## ----echo=FALSE,results='hide'-------------------------------------------
if( Sys.info()['sysname'] == "Windows" )
{
  if(file.exists("resultsMAOpt.RData"))
  {
    load("resultsMAOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='SMA',
      portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
      save(list="results",file="resultsMAOpt.RData")
  }
} else {
  if(file.exists("resultsMAOpt.RData"))
  {
    load("resultsMAOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='SMA',
      portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
    save(list="results",file="resultsMAOpt.RData")
  }
}

## ----eval=FALSE----------------------------------------------------------
## results <- apply.paramset(strategy.st, paramset.label='SMA',
##   portfolio.st=portfolio.st, account.st=account.st, nsamples=0)

## ------------------------------------------------------------------------
head(names(results),20)

## ----PARAMSETTRADESTATS--------------------------------------------------
tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:10])


## ----NETPROFITHEAT,fig.width=7,fig.height=7,dev='png',dpi=300------------
# net profit
z <- tapply(X=tS[,"End.Equity"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
z[1:5,1:10]
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Net Profit")


## ----MAXDDHEAT,fig.width=7,fig.height=7,dev='png',dpi=300----------------
# maxdd
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Max Drawdown")


## ----PFACTORHEAT,fig.width=7,fig.height=7,dev='png',dpi=300--------------
# profit factor
z <- tapply(X=tS[,"Profit.Factor"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Profit Factor")

## ----AVGTRADEHEAT,fig.width=7,fig.height=7,dev='png',dpi=300-------------
# avg trade P&L
z <- tapply(X=tS[,"Avg.Trade.PL"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Average Trade")


## ----RET2MDDHEAT,fig.width=7,fig.height=7,dev='png',dpi=300--------------
# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
  INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Return to Max Drawdown")

## ----RET2MDDBAR----------------------------------------------------------
rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$nFAST[idx],tS$nSLOW[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")


## ------------------------------------------------------------------------
args(tradeGraphs)

## ----eval=FALSE----------------------------------------------------------
## 
## tradeGraphs (stats = tS, free.params = c("nFAST", "nSLOW"),
##   statistics = c("Profit.To.Max.Draw","Net.Trading.PL", "Max.Drawdown",
##   "Avg.Trade.PL", "Num.Trades", "Profit.Factor"), title = '')


## ------------------------------------------------------------------------
rm.strat(strategy.st)
strategy(strategy.st, store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator(strategy.st, name = "SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .fast
	),
	label="nFast"
)

## ----results='hide'------------------------------------------------------
add.indicator(strategy.st, name="SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .slow
	),
	label="nSlow"
)


## ----results='hide'------------------------------------------------------
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="gte"
	),
	label='long'
)

## ----results='hide'------------------------------------------------------
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="lt"
	),
	label='short'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long' ,
		ordertype='stoplimit',
		prefer='High',
		threshold=.threshold,
		TxnFees=0,
		orderqty=+.orderqty,
		osFUN=osMaxPos,
		orderset='ocolong'
	),
	type='enter',
	timespan = .timespan,
	label='EnterLONG'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit',
		prefer='Low',
		threshold=.threshold,
		TxnFees=0,
		orderqty=-.orderqty,
		osFUN=osMaxPos,
		orderset='ocoshort'
	),
	type='enter',
	timespan = .timespan,
	label='EnterSHORT'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=TRUE,
		orderside='long' ,
		ordertype='market',
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2SHORT'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=TRUE,
		orderside='short',
		ordertype='market',
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2LONG'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name = 'ruleSignal',
    arguments=list(sigcol='long' , sigval=TRUE,
        replace=FALSE,
        orderside='long',
        ordertype='stoplimit',
        tmult=TRUE,
        threshold=quote(.stoploss),
        TxnFees=.txnfees,
        orderqty='all',
        orderset='ocolong'
    ),
    type='chain', parent='EnterLONG',
    label='StopLossLONG',
    enabled=FALSE
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name = 'ruleSignal',
    arguments=list(sigcol='short' , sigval=TRUE,
        replace=FALSE,
        orderside='short',
        ordertype='stoplimit',
        tmult=TRUE,
        threshold=quote(.stoploss),
        TxnFees=.txnfees,
        orderqty='all',
        orderset='ocoshort'
    ),
    type='chain', parent='EnterSHORT',
    label='StopLossSHORT',
    enabled=FALSE
)


## ------------------------------------------------------------------------
rm.strat(portfolio.st)
rm.strat(account.st)

## ----results='hide'------------------------------------------------------
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)

initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)

## ----results='hide'------------------------------------------------------
enable.rule('luxor', 'chain', 'StopLoss')


## ----results='hide'------------------------------------------------------
out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
  Dates=paste('::',as.Date(Sys.time()),sep=''))

## ----CHARTLUXOR1SL-------------------------------------------------------
chart.Posn(portfolio.st,"GBPUSD",TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)",
  theme=myTheme)

## ----LUXORSTATS1SL-------------------------------------------------------
PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st, 'GBPUSD')))

## ----ORDERBOOKSL---------------------------------------------------------
ob <- getOrderBook(portfolio.st)$forex$GBPUSD
ob.df <- data.frame(Date=time(ob),coredata(ob))
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)

## ----PERTRADESTATSSL-----------------------------------------------------
PerformanceAnalytics:::textplot(perTradeStats(portfolio.st,"GBPUSD"),
  show.rownames=F)

## ----MAESL,fig.width=7,	fig.height=7-------------------------------------
chart.ME(portfolio.st,'GBPUSD',type='MAE',scale='percent')


## ----results='hide'------------------------------------------------------
add.distribution(strategy.st,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossLONG',
	variable = list(threshold = .StopLoss),
	label = 'StopLossLONG'
)


## ----results='hide'------------------------------------------------------
add.distribution(strategy.st,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossSHORT',
	variable = list(threshold = .StopLoss),
	label = 'StopLossSHORT'
)


## ----results='hide'------------------------------------------------------
add.distribution.constraint(strategy.st,
	paramset.label = 'StopLoss',
	distribution.label.1 = 'StopLossLONG',
	distribution.label.2 = 'StopLossSHORT',
	operator = '==',
	label = 'StopLoss'
)


## ------------------------------------------------------------------------
rm.strat(portfolio.st)
rm.strat(account.st)

## ----results='hide'------------------------------------------------------
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')

## ----results='hide'------------------------------------------------------
addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)

## ----results='hide'------------------------------------------------------
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)

## ----results='hide'------------------------------------------------------
enable.rule('luxor', 'chain', 'StopLoss')


## ----echo=FALSE,results='hide'-------------------------------------------
if( Sys.info()['sysname'] == "Windows" )
{
  if(file.exists("resultsSLOpt.RData"))
  {
    load("resultsSLOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='StopLoss',
      portfolio.st=portfolio.st, account.st=account.st, nsamples=0, verbose=TRUE)
    save(list="results",file="resultsSLOpt.RData")
  }
} else {
  if(file.exists("resultsSLOpt.RData"))
  {
    load("resultsSLOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='StopLoss',
      portfolio.st=portfolio.st, account.st=account.st, nsamples=0, verbose=TRUE)
    save(list="results",file="resultsSLOpt.RData")
  }
}

## ----eval=FALSE----------------------------------------------------------
## results <- apply.paramset(strategy.st, paramset.label='StopLoss',
##   portfolio.st=portfolio.st, account.st=account.st, nsamples=0, verbose=TRUE)

## ----PARAMSETTRADESTATSSL------------------------------------------------
tS <- results$tradeStats
idx <- order(tS[,1])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:5])

## ----STOPLOSSANAL,fig.width=9,fig.height=3.5-----------------------------
par(mfrow=c(1,3))
plot(100*tS$StopLossLONG, tS$Net.Trading.PL, type='b', xlab='Stoploss %',
  ylab='Net.Trading.PL', main='Net Profit vs Stop Loss',col=4)
plot(100*tS$StopLossLONG, tS$Max.Drawdown, type='b', xlab='Stoploss %',
  ylab='Max.Drawdown', main='MaxDrawdown vs Stop Loss',col=4)
plot(100*tS$StopLossLONG, tS$Profit.To.Max.Draw, type='b', xlab='Stoploss %',
  ylab='Profit.To.Max.Draw', main='Return/MaxDD vs Stop Loss',col=4)
par(mfrow=c(1,1))


