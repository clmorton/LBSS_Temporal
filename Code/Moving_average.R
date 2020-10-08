#This code calculates a series of variables to generate stationary data (7 day differencing and 9 term moving average residuals) as well as lagged and lead variables
#Required packages - sed the function instal.packages() if using a package for the first time
library(readr)
library(forecast)

#Attach data
Int_missing <- read_csv("Data/Int_missing.csv", 
                        col_types = cols(Date = col_date(format = "%d/%m/%Y")))
data <- ts(Int_missing, frequency = 365.25, start = 2012)

#Create moving averages and lagged variables
Fob <- data[,"Fob"]
Fob.ln <- log(data[,"Fob"])
Fob.ma9 <- (((lag(Fob, -28)) + (lag(Fob, -21)) + (lag(Fob, -14)) + (lag(Fob, -7)) + Fob + (lag(Fob, 7)) +
               (lag(Fob, 14)) + (lag(Fob, 21)) + (lag(Fob, 28)))/9)
Fob.ma9.res <- ((Fob - Fob.ma9)/Fob.ma9)
Fob.ma9.res.lag1 <- lag(Fob.ma9.res, -1)
Fob.ln.ma9 <- (((lag(Fob.ln, -28)) + (lag(Fob.ln, -21)) + (lag(Fob.ln, -14)) + (lag(Fob.ln, -7)) + Fob.ln + (lag(Fob.ln, 7)) +
                  (lag(Fob.ln, 14)) + (lag(Fob.ln, 21)) + (lag(Fob.ln, 28)))/9)
Fob.ln.ma9.res <- ((Fob.ln - Fob.ln.ma9)/Fob.ln.ma9)
Fob.ln.ma9.res.lag1 <- lag(Fob.ln.ma9.res, -1)
Fob.dif7 <- diff((data[,"Fob"]), 7)
Fob.dif7.lag1 <- lag(Fob.dif7, -1)
Fob.ln.dif7 <- diff(Fob.ln, 7)
Fob.ln.dif7.lag1 <- lag(Fob.ln.dif7, -1)

Card <- data[,"Card"]
Card.ln <- log(data[,"Card"])
Card.ma9 <- (((lag(Card, -28)) + (lag(Card, -21)) + (lag(Card, -14)) + (lag(Card, -7)) + Card + (lag(Card, 7)) +
                (lag(Card, 14)) + (lag(Card, 21)) + (lag(Card, 28)))/9)
Card.ma9.res <- ((Card - Card.ma9)/Card.ma9)
Card.ma9.res.lag1 <- lag(Card.ma9.res, -1)
Card.ln.ma9 <- (((lag(Card.ln, -28)) + (lag(Card.ln, -21)) + (lag(Card.ln, -14)) + (lag(Card.ln, -7)) + Card.ln + (lag(Card.ln, 7)) +
                   (lag(Card.ln, 14)) + (lag(Card.ln, 21)) + (lag(Card.ln, 28)))/9)
Card.ln.ma9.res <- ((Card.ln - Card.ln.ma9)/Card.ln.ma9)
Card.ln.ma9.res.lag1 <- lag(Card.ln.ma9.res, -1)
Card.dif7 <- diff((data[,"Card"]), 7)
Card.dif7.lag1 <- lag(Card.dif7, -1)
Card.ln.dif7 <- diff(Card.ln, 7)
Card.ln.dif7.lag1 <- lag(Card.ln.dif7, -1)

Max.air.temp <- data[,"Max.air.temp"]
Max.air.temp.ln <- log(data[,"Max.air.temp"])
Max.air.temp.ma9 <- (((lag(Max.air.temp, -28)) + (lag(Max.air.temp, -21)) + (lag(Max.air.temp, -14)) + (lag(Max.air.temp, -7)) + Max.air.temp + (lag(Max.air.temp, 7)) +
                        (lag(Max.air.temp, 14)) + (lag(Max.air.temp, 21)) + (lag(Max.air.temp, 28)))/9)
Max.air.temp.ma9.res <- ((Max.air.temp - Max.air.temp.ma9)/Max.air.temp.ma9)
Max.air.temp.ma9.res.lag1 <- lag(Max.air.temp.ma9.res, -1)
Max.air.temp.ma9.res.lead1 <- lag(Max.air.temp.ma9.res, 1)
Max.air.temp.ln.ma9 <- (((lag(Max.air.temp.ln, -28)) + (lag(Max.air.temp.ln, -21)) + (lag(Max.air.temp.ln, -14)) + (lag(Max.air.temp.ln, -7)) + Max.air.temp.ln + (lag(Max.air.temp.ln, 7)) +
                           (lag(Max.air.temp.ln, 14)) + (lag(Max.air.temp.ln, 21)) + (lag(Max.air.temp.ln, 28)))/9)
Max.air.temp.ln.ma9.res <- ((Max.air.temp.ln - Max.air.temp.ln.ma9)/Max.air.temp.ln.ma9)
Max.air.temp.ln.ma9.res.lag1 <- lag(Max.air.temp.ln.ma9.res, -1)
Max.air.temp.ln.ma9.res.lead1 <- lag(Max.air.temp.ln.ma9.res, 1)
Max.air.temp.dif7 <- diff((data[,"Max.air.temp"]), 7)
Max.air.temp.dif7.lag1 <- lag(Max.air.temp.dif7, -1)
Max.air.temp.dif7.lead1 <- lag(Max.air.temp.dif7, 1)
Max.air.temp.ln.dif7 <- diff(Max.air.temp.ln, 7)
Max.air.temp.ln.dif7.lag1 <- lag(Max.air.temp.ln.dif7, -1)
Max.air.temp.ln.dif7.lead1 <- lag(Max.air.temp.ln.dif7, 1)

Percip <- data[,"Percip"]
Percip.light <- ifelse(Percip > 0 & Percip < 5, 1, 0)
Percip.light.lag1 <- lag(Percip.light, -1)
Percip.light.lead1 <- lag(Percip.light, 1)
Percip.heavy <- ifelse(Percip >= 5, 1, 0)
Percip.heavy.lag1 <- lag(Percip.heavy, -1)
Percip.heavy.lead1 <- lag(Percip.heavy, 1)
Percip.dif7 <- diff((data[,"Percip"]), 7)
Percip.dif7.lag1 <- lag(Percip.dif7, -1)
Percip.dif7.lead1 <- lag(Percip.dif7, 1)

Rel.humid <- data[,"Rel.humid"]
Rel.humid.ln <- log(data[,"Rel.humid"])
Rel.humid.ma9 <- (((lag(Rel.humid, -28)) + (lag(Rel.humid, -21)) + (lag(Rel.humid, -14)) + (lag(Rel.humid, -7)) + Rel.humid + (lag(Rel.humid, 7)) +
                     (lag(Rel.humid, 14)) + (lag(Rel.humid, 21)) + (lag(Rel.humid, 28)))/9)
Rel.humid.ma9.res <- ((Rel.humid - Rel.humid.ma9)/Rel.humid.ma9)
Rel.humid.ma9.res.lag1 <- lag(Rel.humid.ma9.res, -1)
Rel.humid.ma9.res.lead1 <- lag(Rel.humid.ma9.res, 1)
Rel.humid.ln.ma9 <- (((lag(Rel.humid.ln, -28)) + (lag(Rel.humid.ln, -21)) + (lag(Rel.humid.ln, -14)) + (lag(Rel.humid.ln, -7)) + Rel.humid.ln + (lag(Rel.humid.ln, 7)) +
                        (lag(Rel.humid.ln, 14)) + (lag(Rel.humid.ln, 21)) + (lag(Rel.humid.ln, 28)))/9)
Rel.humid.ln.ma9.res <- ((Rel.humid.ln - Rel.humid.ln.ma9)/Rel.humid.ln.ma9)
Rel.humid.ln.ma9.res.lag1 <- lag(Rel.humid.ln.ma9.res, -1)
Rel.humid.ln.ma9.res.lead1 <- lag(Rel.humid.ln.ma9.res, 1)
Rel.humid.dif7 <- diff((data[,"Rel.humid"]), 7)
Rel.humid.dif7.lag1 <- lag(Rel.humid.dif7, -1)
Rel.humid.dif7.lead1 <- lag(Rel.humid.dif7, 1)
Rel.humid.ln.dif7 <- diff(Rel.humid.ln, 7)
Rel.humid.ln.dif7.lag1 <- lag(Rel.humid.ln.dif7, -1)
Rel.humid.ln.dif7.lead1 <- lag(Rel.humid.ln.dif7, 1)

Avg.wind <- data[,"Avg.wind"]
Avg.wind.ln <- log(data[,"Avg.wind"])
Avg.wind.ma9 <- (((lag(Avg.wind, -28)) + (lag(Avg.wind, -21)) + (lag(Avg.wind, -14)) + (lag(Avg.wind, -7)) + Avg.wind + (lag(Avg.wind, 7)) +
                    (lag(Avg.wind, 14)) + (lag(Avg.wind, 21)) + (lag(Avg.wind, 28)))/9)
Avg.wind.ma9.res <- ((Avg.wind - Avg.wind.ma9)/Avg.wind.ma9)
Avg.wind.ma9.res.lag1 <- lag(Avg.wind.ma9.res, -1)
Avg.wind.ma9.res.lead1 <- lag(Avg.wind.ma9.res, 1)
Avg.wind.ln.ma9 <- (((lag(Avg.wind.ln, -28)) + (lag(Avg.wind.ln, -21)) + (lag(Avg.wind.ln, -14)) + (lag(Avg.wind.ln, -7)) + Avg.wind.ln + (lag(Avg.wind.ln, 7)) +
                       (lag(Avg.wind.ln, 14)) + (lag(Avg.wind.ln, 21)) + (lag(Avg.wind.ln, 28)))/9)
Avg.wind.ln.ma9.res <- ((Avg.wind.ln - Avg.wind.ln.ma9)/Avg.wind.ln.ma9)
Avg.wind.ln.ma9.res.lag1 <- lag(Avg.wind.ln.ma9.res, -1)
Avg.wind.ln.ma9.res.lead1 <- lag(Avg.wind.ln.ma9.res, 1)
Avg.wind.dif7 <- diff((data[,"Avg.wind"]), 7)
Avg.wind.dif7.lag1 <- lag(Avg.wind.dif7, -1)
Avg.wind.dif7.lead1 <- lag(Avg.wind.dif7, 1)
Avg.wind.ln.dif7 <- diff(Avg.wind.ln, 7)
Avg.wind.ln.dif7.lag1 <- lag(Avg.wind.ln.dif7, -1)
Avg.wind.ln.dif7.lead1 <- lag(Avg.wind.ln.dif7, 1)

Daylight <- data[,"Daylight"]
Daylight.ln <- log(data[,"Daylight"])
Daylight.ma9 <- (((lag(Daylight, -28)) + (lag(Daylight, -21)) + (lag(Daylight, -14)) + (lag(Daylight, -7)) + Daylight + (lag(Daylight, 7)) +
                    (lag(Daylight, 14)) + (lag(Daylight, 21)) + (lag(Daylight, 28)))/9)
Daylight.ma9.res <- ((Daylight - Daylight.ma9)/Daylight.ma9)
Daylight.ma9.res.lag1 <- lag(Daylight.ma9.res, -1)
Daylight.ma9.res.lead1 <- lag(Daylight.ma9.res, 1)
Daylight.ln.ma9 <- (((lag(Daylight.ln, -28)) + (lag(Daylight.ln, -21)) + (lag(Daylight.ln, -14)) + (lag(Daylight.ln, -7)) + Daylight.ln + (lag(Daylight.ln, 7)) +
                       (lag(Daylight.ln, 14)) + (lag(Daylight.ln, 21)) + (lag(Daylight.ln, 28)))/9)
Daylight.ln.ma9.res <- ((Daylight.ln - Daylight.ln.ma9)/Daylight.ln.ma9)
Daylight.ln.ma9.res.lag1 <- lag(Daylight.ln.ma9.res, -1)
Daylight.ln.ma9.res.lead1 <- lag(Daylight.ln.ma9.res, 1)
Daylight.dif7 <- diff((data[,"Daylight"]), 7)
Daylight.dif7.lag1 <- lag(Daylight.dif7, -1)
Daylight.dif7.lead1 <- lag(Daylight.dif7, 1)
Daylight.ln.dif7 <- diff(Daylight.ln, 7)
Daylight.ln.dif7.lag1 <- lag(Daylight.ln.dif7, -1)
Daylight.ln.dif7.lead1 <- lag(Daylight.ln.dif7, 1)

Ozone.mean <- data[,"Ozone.mean"]
Ozone.mean.ln <- log(data[,"Ozone.mean"])
Ozone.mean.ma9 <- (((lag(Ozone.mean, -28)) + (lag(Ozone.mean, -21)) + (lag(Ozone.mean, -14)) + (lag(Ozone.mean, -7)) + Ozone.mean + (lag(Ozone.mean, 7)) +
                      (lag(Ozone.mean, 14)) + (lag(Ozone.mean, 21)) + (lag(Ozone.mean, 28)))/9)
Ozone.mean.ma9.res <- ((Ozone.mean - Ozone.mean.ma9)/Ozone.mean.ma9)
Ozone.mean.ma9.res.lag1 <- lag(Ozone.mean.ma9.res, -1)
Ozone.mean.ma9.res.lead1 <- lag(Ozone.mean.ma9.res, 1)
Ozone.mean.ln.ma9 <- (((lag(Ozone.mean.ln, -28)) + (lag(Ozone.mean.ln, -21)) + (lag(Ozone.mean.ln, -14)) + (lag(Ozone.mean.ln, -7)) + Ozone.mean.ln + (lag(Ozone.mean.ln, 7)) +
                         (lag(Ozone.mean.ln, 14)) + (lag(Ozone.mean.ln, 21)) + (lag(Ozone.mean.ln, 28)))/9)
Ozone.mean.ln.ma9.res <- ((Ozone.mean.ln - Ozone.mean.ln.ma9)/Ozone.mean.ln.ma9)
Ozone.mean.ln.ma9.res.lag1 <- lag(Ozone.mean.ln.ma9.res, -1)
Ozone.mean.ln.ma9.res.lead1 <- lag(Ozone.mean.ln.ma9.res, 1)
Ozone.mean.dif7 <- diff((data[,"Ozone.mean"]), 7)
Ozone.mean.dif7.lag1 <- lag(Ozone.mean.dif7, -1)
Ozone.mean.dif7.lead1 <- lag(Ozone.mean.dif7, 1)
Ozone.mean.ln.dif7 <- diff(Ozone.mean.ln, 7)
Ozone.mean.ln.dif7.lag1 <- lag(Ozone.mean.ln.dif7, -1)
Ozone.mean.ln.dif7.lead1 <- lag(Ozone.mean.ln.dif7, 1)

NOX.mean <- data[,"NOX.mean"]
NOX.mean.ln <- log(data[,"NOX.mean"])
NOX.mean.ma9 <- (((lag(NOX.mean, -28)) + (lag(NOX.mean, -21)) + (lag(NOX.mean, -14)) + (lag(NOX.mean, -7)) + NOX.mean + (lag(NOX.mean, 7)) +
                    (lag(NOX.mean, 14)) + (lag(NOX.mean, 21)) + (lag(NOX.mean, 28)))/9)
NOX.mean.ma9.res <- ((NOX.mean - NOX.mean.ma9)/NOX.mean.ma9)
NOX.mean.ma9.res.lag1 <- lag(NOX.mean.ma9.res, -1)
NOX.mean.ma9.res.lead1 <- lag(NOX.mean.ma9.res, 1)
NOX.mean.ln.ma9 <- (((lag(NOX.mean.ln, -28)) + (lag(NOX.mean.ln, -21)) + (lag(NOX.mean.ln, -14)) + (lag(NOX.mean.ln, -7)) + NOX.mean.ln + (lag(NOX.mean.ln, 7)) +
                       (lag(NOX.mean.ln, 14)) + (lag(NOX.mean.ln, 21)) + (lag(NOX.mean.ln, 28)))/9)
NOX.mean.ln.ma9.res <- ((NOX.mean.ln - NOX.mean.ln.ma9)/NOX.mean.ln.ma9)
NOX.mean.ln.ma9.res.lag1 <- lag(NOX.mean.ln.ma9.res, -1)
NOX.mean.ln.ma9.res.lead1 <- lag(NOX.mean.ln.ma9.res, 1)
NOX.mean.dif7 <- diff((data[,"NOX.mean"]), 7)
NOX.mean.dif7.lag1 <- lag(NOX.mean.dif7, -1)
NOX.mean.dif7.lead1 <- lag(NOX.mean.dif7, 1)
NOX.mean.ln.dif7 <- diff(NOX.mean.ln, 7)
NOX.mean.ln.dif7.lag1 <- lag(NOX.mean.ln.dif7, -1)
NOX.mean.ln.dif7.lead1 <- lag(NOX.mean.ln.dif7, 1)

PM10.mean <- data[,"PM10.mean"]
PM10.mean.ln <- log(data[,"PM10.mean"])
PM10.mean.ma9 <- (((lag(PM10.mean, -28)) + (lag(PM10.mean, -21)) + (lag(PM10.mean, -14)) + (lag(PM10.mean, -7)) + PM10.mean + (lag(PM10.mean, 7)) +
                     (lag(PM10.mean, 14)) + (lag(PM10.mean, 21)) + (lag(PM10.mean, 28)))/9)
PM10.mean.ma9.res <- ((PM10.mean - PM10.mean.ma9)/PM10.mean.ma9)
PM10.mean.ma9.res.lag1 <- lag(PM10.mean.ma9.res, -1)
PM10.mean.ma9.res.lead1 <- lag(PM10.mean.ma9.res, 1)
PM10.mean.ln.ma9 <- (((lag(PM10.mean.ln, -28)) + (lag(PM10.mean.ln, -21)) + (lag(PM10.mean.ln, -14)) + (lag(PM10.mean.ln, -7)) + PM10.mean.ln + (lag(PM10.mean.ln, 7)) +
                        (lag(PM10.mean.ln, 14)) + (lag(PM10.mean.ln, 21)) + (lag(PM10.mean.ln, 28)))/9)
PM10.mean.ln.ma9.res <- ((PM10.mean.ln - PM10.mean.ln.ma9)/PM10.mean.ln.ma9)
PM10.mean.ln.ma9.res.lag1 <- lag(PM10.mean.ln.ma9.res, -1)
PM10.mean.ln.ma9.res.lead1 <- lag(PM10.mean.ln.ma9.res, 1)
PM10.mean.dif7 <- diff((data[,"PM10.mean"]), 7)
PM10.mean.dif7.lag1 <- lag(PM10.mean.dif7, -1)
PM10.mean.dif7.lead1 <- lag(PM10.mean.dif7, 1)
PM10.mean.ln.dif7 <- diff(PM10.mean.ln, 7)
PM10.mean.ln.dif7.lag1 <- lag(PM10.mean.ln.dif7, -1)
PM10.mean.ln.dif7.lead1 <- lag(PM10.mean.ln.dif7, 1)

Bank.hol <- data[,"Bank.hol"]

#Save lag and difference variables
Int_missing_dif_ma_lag <- cbind(Fob, Fob.ln, Fob.ma9, Fob.ma9.res, Fob.ma9.res.lag1, Fob.ln.ma9, Fob.ln.ma9.res, 
                                Fob.ln.ma9.res.lag1, Fob.dif7, Fob.dif7.lag1, Fob.ln.dif7, Fob.ln.dif7.lag1, Card, Card.ln, 
                                Card.ma9, Card.ma9.res, Card.ma9.res.lag1, Card.ln.ma9, Card.ln.ma9.res, 
                                Card.ln.ma9.res.lag1, Card.dif7, Card.dif7.lag1, Card.ln.dif7, Card.ln.dif7.lag1,
                                Max.air.temp, Max.air.temp.ln, Max.air.temp.ma9, Max.air.temp.ma9.res, Max.air.temp.ma9.res.lag1,
                                Max.air.temp.ma9.res.lead1, Max.air.temp.ln.ma9, Max.air.temp.ln.ma9.res, Max.air.temp.ln.ma9.res.lag1,
                                Max.air.temp.ln.ma9.res.lead1, Max.air.temp.dif7, Max.air.temp.dif7.lag1, Max.air.temp.dif7.lead1,
                                Max.air.temp.ln.dif7, Max.air.temp.ln.dif7.lag1, Max.air.temp.ln.dif7.lead1,
                                Percip, Percip.light, Percip.light.lag1, Percip.light.lead1, Percip.heavy, Percip.heavy.lag1,
                                Percip.heavy.lead1, Percip.dif7, Percip.dif7.lag1, Percip.dif7.lead1, Rel.humid,
                                Rel.humid.ln, Rel.humid.ma9, Rel.humid.ma9.res, Rel.humid.ma9.res.lag1, Rel.humid.ma9.res.lead1,
                                Rel.humid.ln.ma9, Rel.humid.ln.ma9.res, Rel.humid.ln.ma9.res.lag1, Rel.humid.ln.ma9.res.lead1,
                                Rel.humid.dif7, Rel.humid.dif7.lag1, Rel.humid.dif7.lead1, Rel.humid.ln.dif7, Rel.humid.ln.dif7.lag1,
                                Rel.humid.ln.dif7.lead1, Avg.wind, Avg.wind.ln, Avg.wind.ma9, Avg.wind.ma9.res, Avg.wind.ma9.res.lag1,
                                Avg.wind.ma9.res.lead1, Avg.wind.ln.ma9, Avg.wind.ln.ma9.res, Avg.wind.ln.ma9.res.lag1,
                                Avg.wind.ln.ma9.res.lead1, Avg.wind.dif7, Avg.wind.dif7.lag1, Avg.wind.dif7.lead1, Avg.wind.ln.dif7,
                                Avg.wind.ln.dif7.lag1, Avg.wind.ln.dif7.lead1, Daylight, Daylight.ln, Daylight.ma9, Daylight.ma9.res,
                                Daylight.ma9.res.lag1, Daylight.ma9.res.lead1, Daylight.ln.ma9, Daylight.ln.ma9.res,
                                Daylight.ln.ma9.res.lag1, Daylight.ln.ma9.res.lead1, Daylight.dif7, Daylight.dif7.lag1,
                                Daylight.dif7.lead1, Daylight.ln.dif7, Daylight.ln.dif7.lag1, Daylight.ln.dif7.lead1, Ozone.mean,
                                Ozone.mean.ln, Ozone.mean.ma9, Ozone.mean.ma9.res, Ozone.mean.ma9.res.lag1, Ozone.mean.ma9.res.lead1,
                                Ozone.mean.ln.ma9, Ozone.mean.ln.ma9.res, Ozone.mean.ln.ma9.res.lag1, Ozone.mean.ln.ma9.res.lead1,
                                Ozone.mean.dif7, Ozone.mean.dif7.lag1, Ozone.mean.dif7.lead1, Ozone.mean.ln.dif7,
                                Ozone.mean.ln.dif7.lag1, Ozone.mean.ln.dif7.lead1, NOX.mean, NOX.mean.ln, NOX.mean.ma9,
                                NOX.mean.ma9.res, NOX.mean.ma9.res.lag1, NOX.mean.ma9.res.lead1, NOX.mean.ln.ma9, NOX.mean.ln.ma9.res,
                                NOX.mean.ln.ma9.res.lag1, NOX.mean.ln.ma9.res.lead1, NOX.mean.dif7, NOX.mean.dif7.lag1,
                                NOX.mean.dif7.lead1, NOX.mean.ln.dif7, NOX.mean.ln.dif7.lag1, NOX.mean.ln.dif7.lead1, PM10.mean,
                                PM10.mean.ln, PM10.mean.ma9, PM10.mean.ma9.res, PM10.mean.ma9.res.lag1, PM10.mean.ma9.res.lead1,
                                PM10.mean.ln.ma9, PM10.mean.ln.ma9.res, PM10.mean.ln.ma9.res.lag1, PM10.mean.ln.ma9.res.lead1,
                                PM10.mean.dif7, PM10.mean.dif7.lag1, PM10.mean.dif7.lead1, PM10.mean.ln.dif7, PM10.mean.ln.dif7.lag1,
                                PM10.mean.ln.dif7.lead1, Bank.hol)
View(Int_missing_dif_ma_lag)
write.csv(Int_missing_dif_ma_lag, file = "Int_missing_dif_ma_lag.csv", na = "")
