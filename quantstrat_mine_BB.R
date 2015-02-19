source("initAcctPortfOrd.R")

# what's we've got so far
Sys.getenv("TZ")
ls(all=T)
search()
ls(.blotter)
ls(.strategy)
ls(envir=FinancialInstrument:::.instrument)

# initiate strategy, indicators, signals, and rules 
# rm.strat(name = st) # remove 'bbands' if run before

st <- 'bbands'
strategy(name = st, store=T)

head(mktdata) #error
add.indicator(strategy = st,
              arguments = list(HLC=quote(HLC(mktdata))),
              name = 'BBands',
              label='BBands')

# check the state of mktdata object
head(applyIndicators(strategy = st, SBER))
head(mktdata)

add.signal(strategy = st,
           labe="Cl.gt.up",
           arguments = list(columns=c("SBER.Close", "up.BBands"),
                            relationships="gt"),
           name = "sigCrossover")

head(applySignals(strategy = st, mktdata = mktdata), 100)
