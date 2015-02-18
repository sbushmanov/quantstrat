# download constituents of the MICEXINDEXCF
#
# the file micex.csv is downloaded from
# http://www.micex.ru/marketdata/indices/shares/composite#&index=MICEXINDEXCF
#
# a broder alternative could be downloaded as MICEX BMI

micexComp <- read.table("download/micex.csv", 
#                         colClasses = c("character",
#                                        "POSIXct",
#                                        "character",
#                                        "numeric",
#                                        "numeric"),
                        stringsAsFactors=F,
                        sep=";",
                        header = T)

symbols <- micexComp[micexComp$DATE == "2015-02-17", "SECID"]
library(rusquant)
#fix(getSymbols.Finam)  # 135 , 136: change to 5 sec
Sys.setenv(TZ="MSK")

# download tickers from Finam at 10 mins
getSymbols(symbols,
           index.class="POSIXct",
           tz="MSK",
           from="2012-01-11",
           period="10min",
           src="Finam")
# in case some tickers cannot be downloaded skip them
getSymbols(symbols[24:lenght(symbols)],
           index.class="POSIXct",
           tz="MSK",
           from="2012-01-11",
           period="10min",
           src="Finam")


# # download "MICEX" ticker separately
# getSymbols("MICEX",
#            index.class="POSIXct",
#            tz="MSK",
#            from="2012-01-11",
#            period="10min",
#            src="Finam")

# or

MICEX <-   read.table("./quotes/MICEX.csv",
                      stringsAsFactors=F,
                      sep = "," ,
                      header=T)
MICEX$X.Dt <- paste(MICEX$X.DATE, MICEX$X.TIME, sep= " ")
MICEX$X.Dt <- as.POSIXct(MICEX$X.Dt, format="%Y%m%d %H%M%S", tz="MSK" )
MICEX$X.DATE. <- MICEX$X.TIME. <- NULL
MICEX <- xts(x = MICEX[3:7], order.by = MICEX[,8], tzone = 'MSK')
names(MICEX) <- c("MICEX.Open",
                  "MICEX.High",
                  "MICEX.Low",
                  "MICEX.Close",
                  "MICEX.Volume")
save.image("./data/quotes.RData")







