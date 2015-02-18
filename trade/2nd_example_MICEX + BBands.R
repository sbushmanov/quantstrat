#Load the dataset, adjust, and convert to monthly returns

set.seed(42)
library(rusquant)
#getSymbols('MICEX', src = "Finam", from='2013-06-01')
chartSeries(MICEX, theme = "wsj", TA = 'addBBands()')

########################################################
# Check period return calcualtion -- Start
########################################################
cl <- Cl(MICEX)
f <- first(cl)
tail(cl)
l <- last(cl)
periodReturn1 <- Delt(f,l)
ret <- Delt(cl)
periodReturn2 <- prod(1+ret, na.rm = T) - 1
periodReturn1
periodReturn2
#######################################################
# Check return calcualtion -- End
#######################################################

########################################################
# Max return possible -- START
# 0.002 is round trip broker commission
########################################################
positions <- sign(ret)
trades <- abs(diff(positions))
periodReturn3 <- prod(1 + ret*positions - trades*0.002, na.rm = T) -1
#######################################################
# Max return possible  -- END
#######################################################


########################################################
# Calculate target to be predicted -- START 1
########################################################

#set Window Width (kWW) for localMaxMin
# Window Width is used for smoothing:
# local Max and Mins are calculated within window
# and then used for approximating the market movements
# see plot of localMaxMin$maxmin against MICEX
kWW <- 15
cl$dt <- as.numeric(format(index(cl), "%Y%m%d"))
localMax <- rollapply(cl,
                      width = kWW,
                      by = kWW,
                      align = 'right',
                      by.column = F,
                      FUN = function(x)
                             return(x[which.max(x[, 'MICEX.Close']), c('MICEX.Close', 'dt')]))
index(localMax) <- as.Date(as.character(as.integer(localMax$dt)), format = "%Y%m%d")
localMax$dt <- NULL
colnames(localMax) <- c('max')
localMax <- localMax[!is.na(index(localMax))]
localMin <- rollapply(cl,
                      width = kWW,
                      by = kWW,
                      align = 'right',
                      by.column = F,
                      FUN = function(x)
                        return(x[which.min(x[, 'MICEX.Close']), c('MICEX.Close', 'dt')]))
index(localMin) <- as.Date(as.character(as.integer(localMin$dt)), format = "%Y%m%d")
localMin$dt <- NULL
colnames(localMin) <- c('min')
localMin <- localMin[!is.na(index(localMin))]
localMaxMin <- rbind(localMax,localMin)
colnames(localMaxMin) <- c('maxmin')

plot(Cl(MICEX))
lines(localMaxMin$maxmin, col = "green")

localMaxMin$ret <- Delt(localMaxMin$maxmin)
localMaxMin$longShort <- ifelse(localMaxMin$ret > 0, 1, -1)
(periodReturn4 <- prod(1+localMaxMin$ret*localMaxMin$longShort, na.rm = T) - 1)
dummyMICEX <- xts(rep(0, length = nrow(MICEX)), order.by = index(MICEX))
localMaxMin <- cbind(localMaxMin, dummyMICEX)
localMaxMin$MICEX <- Cl(MICEX)
localMaxMin$retMICEX <- ret
# # start rev rest
# tsFull <-xts(rep(0,10), order.by = Sys.Date() + 1:10)
# tsPart <-xts(c(1,2), order.by = as.Date(c("2014-06-15", "2014-06-18")))
# tsMerge <- merge(tsFull, tsPart)
# tsMergeNew <- rev(na.locf(rev(tsMerge)))
# # end rev test
localMaxMin$longShort <- na.locf(localMaxMin$longShort, fromLast = T, na.rm = F)

#write.xlsx(localMaxMin, "/home/sergey/Desktop/1.xlsx")
(periodReturn5 <- prod(1+localMaxMin$retMICEX*localMaxMin$longShort, na.rm = T) - 1)
chartSeries(MICEX, theme = "wsj")
addTA(localMaxMin$longShort == 1, col = "green", on = -1)
target <- localMaxMin$longShort

#######################################################
# Calculate and optimize target to be predicted -- END 1
#######################################################

#Calculate some technical indicators
periods <- c(3, 6, 9, 12)
Lags <- data.frame(lapply(c(1:2, periods), function(x) Lag(Target, x)))
EMAs <- data.frame(lapply(periods, function(x) {
  out <- EMA(Target, x)
  names(out) <- paste('EMA', x, sep='.')
  return(out)
}))
RSIs <- data.frame(lapply(periods, function(x) {
  out <- RSI(Cl(GSPC), x)
  names(out) <- paste('RSI', x, sep='.')
  return(out)
}))
# DVIs <- data.frame(lapply(periods, function(x) {
#   out <- DVI(Cl(GSPC), x)
#   names(out) <- paste('DVI', x, sep=".")
#   return(out)
# }))
dat <- data.frame(Next(Target), Lags, EMAs, RSIs)
dat <- na.omit(dat)

#Create a summary function to calculate trade costs and cumulative profit in the test set
mySummary <- function (data, lev = NULL, model = "NULL") {
  positions <- sign(data[, "pred"])
  trades <- abs(c(1,diff(positions)))
  profits <- positions*data[, "obs"] - trades*0.01
  profit <- prod(1+profits)
  names(profit) <- 'profit'
  return(profit)
}

#Build the model!
library(caret)
model <- train(x = dat[,-1], y = dat[,1], method='rpart', 
               metric='profit', maximize=TRUE,
               trControl=trainControl(method='timeslice',
                                      initialWindow=12, 
                                      fixedWindow=TRUE, 
                                      horizon=12, 
                                      summaryFunction=mySummary,
                                      verboseIter=TRUE)
               )
model
