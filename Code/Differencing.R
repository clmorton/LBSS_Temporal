#Required packages - sed the function instal.packages() if using a package for the first time

library(readr)

#Attach data
Int_missing <- read_csv("Data/Int_missing.csv", 
                        col_types = cols(Date = col_date(format = "%d/%m/%Y")))
data <- ts(Int_missing, frequency = 365.25, start = 2012)

#Create lag and difference variables

Fob <- data[,"Fob"]
Fob.ln <- log(data[,"Fob"])
Fob.dif7 <- diff((data[,"Fob"]), 7)
Fob.ln.dif7 <- diff(Fob.ln, 7)
Fob.ln.dif7.lag1 <- lag(Fob.ln.dif7, -1)
Fob.ln.dif7.lag7 <- lag(Fob.ln.dif7, -7)

Card <- data[,"Card"]
Card.ln <- log(data[,"Card"])
Card.dif7 <- diff((data[,"Card"]), 7)
Card.ln.dif7 <- diff(Card.ln, 7)
Card.ln.dif7.lag1 <- lag(Card.ln.dif7, -1)
Card.ln.dif7.lag7 <- lag(Card.ln.dif7, -7)

Max.air.temp <- data[,"Max.air.temp"]
Max.air.temp.ln <- log(data[,"Max.air.temp"])
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
Rel.humid.dif7 <- diff((data[,"Rel.humid"]), 7)
Rel.humid.dif7.lag1 <- lag(Rel.humid.dif7, -1)
Rel.humid.dif7.lead1 <- lag(Rel.humid.dif7, 1)
Rel.humid.ln.dif7 <- diff(Rel.humid.ln, 7)
Rel.humid.ln.dif7.lag1 <- lag(Rel.humid.ln.dif7, -1)
Rel.humid.ln.dif7.lead1 <- lag(Rel.humid.ln.dif7, 1)

Avg.wind <- data[,"Avg.wind"]
Avg.wind.ln <- log(data[,"Avg.wind"])
Avg.wind.dif7 <- diff((data[,"Avg.wind"]), 7)
Avg.wind.dif7.lag1 <- lag(Avg.wind.dif7, -1)
Avg.wind.dif7.lead1 <- lag(Avg.wind.dif7, 1)
Avg.wind.ln.dif7 <- diff(Avg.wind.ln, 7)
Avg.wind.ln.dif7.lag1 <- lag(Avg.wind.ln.dif7, -1)
Avg.wind.ln.dif7.lead1 <- lag(Avg.wind.ln.dif7, 1)

Daylight <- data[,'Daylight']
Daylight.ln <- log(data[,"Daylight"])
Daylight.dif7 <- diff((data[,"Daylight"]), 7)
Daylight.ln.dif7 <- diff(Daylight.ln, 7)

Ozone.mean <- data[,"Ozone.mean"]
Ozone.mean.ln <- log(data[,"Ozone.mean"])
Ozone.mean.dif7 <- diff((data[,"Ozone.mean"]), 7)
Ozone.mean.dif7.lag1 <- lag(Ozone.mean.dif7, -1)
Ozone.mean.ln.dif7 <- diff(Ozone.mean.ln, 7)
Ozone.mean.ln.dif7.lag1 <- lag(Ozone.mean.ln.dif7, -1)
Ozone.mean.ln.dif7.lead1 <- lag(Ozone.mean.ln.dif7, 1)

NOX.mean <- data[,"NOX.mean"]
NOX.mean.ln <- log(data[,"NOX.mean"])
NOX.mean.dif7 <- diff((data[,"NOX.mean"]), 7)
NOX.mean.dif7.lag1 <- lag(NOX.mean.dif7, -1)
NOX.mean.ln.dif7 <- diff(NOX.mean.ln, 7)
NOX.mean.ln.dif7.lag1 <- lag(NOX.mean.ln.dif7, -1)
NOX.mean.ln.dif7.lead1 <- lag(NOX.mean.ln.dif7, 1)

PM10.mean <- data[,"PM10.mean"]
PM10.mean.ln <- log(data[,"PM10.mean"])
PM10.mean.dif7 <- diff((data[,"PM10.mean"]), 7)
PM10.mean.dif7.lag1 <- lag(PM10.mean.dif7, -1)
PM10.mean.ln.dif7 <- diff(PM10.mean.ln, 7)
PM10.mean.ln.dif7.lag1 <- lag(PM10.mean.ln.dif7, -1)
PM10.mean.ln.dif7.lead1 <- lag(PM10.mean.ln.dif7, 1)

PM2.5.mean <- data[,"PM2.5.mean"]
PM2.5.mean.ln <- log(data[,"PM2.5.mean"])
PM2.5.mean.dif7 <- diff((data[,"PM2.5.mean"]), 7)
PM2.5.mean.dif7.lag1 <- lag(PM2.5.mean.dif7, -1)
PM2.5.mean.ln.dif7 <- diff(PM2.5.mean.ln, 7)
PM2.5.mean.ln.dif7.lag1 <- lag(PM2.5.mean.ln.dif7, -1)
PM2.5.mean.ln.dif7.lead1 <- lag(PM2.5.mean.ln.dif7, 1)

Bank.hol <- data[,"Bank.hol"]

#Save lag and difference variables
Int_missing_dif_lag <- cbind(Fob, Fob.ln, Fob.dif7, Fob.ln.dif7, Fob.ln.dif7.lag1, Fob.ln.dif7.lag7, Card, Card.ln, Card.dif7,
                             Card.ln.dif7, Card.ln.dif7.lag1, Card.ln.dif7.lag7, Max.air.temp, Max.air.temp.ln, Max.air.temp.dif7,
                             Max.air.temp.dif7.lag1, Max.air.temp.dif7.lead1, Max.air.temp.ln.dif7, Max.air.temp.ln.dif7.lag1,
                             Max.air.temp.ln.dif7.lead1, Percip, Percip.light, Percip.light.lag1, Percip.light.lead1,
                             Percip.heavy, Percip.heavy.lag1, Percip.heavy.lead1, Percip.dif7, Percip.dif7.lag1,
                             Percip.dif7.lead1, Rel.humid, Rel.humid.ln, Rel.humid.dif7, Rel.humid.dif7.lag1,
                             Rel.humid.dif7.lead1, Rel.humid.ln.dif7, Rel.humid.ln.dif7.lag1, Rel.humid.ln.dif7.lead1,
                             Avg.wind, Avg.wind.ln, Avg.wind.dif7, Avg.wind.dif7.lag1, Avg.wind.dif7.lead1, Avg.wind.ln.dif7,
                             Avg.wind.ln.dif7.lag1, Avg.wind.ln.dif7.lead1, Daylight, Daylight.ln, Daylight.dif7,
                             Daylight.ln.dif7, Ozone.mean, Ozone.mean.ln, Ozone.mean.dif7, Ozone.mean.dif7.lag1, Ozone.mean.ln.dif7,
                             Ozone.mean.ln.dif7.lag1, Ozone.mean.ln.dif7.lead1, NOX.mean, NOX.mean.ln, NOX.mean.dif7,
                             NOX.mean.dif7.lag1, NOX.mean.ln.dif7, NOX.mean.ln.dif7.lag1, NOX.mean.ln.dif7.lead1, PM10.mean,
                             PM10.mean.ln, PM10.mean.dif7, PM10.mean.dif7.lag1, PM10.mean.ln.dif7, PM10.mean.ln.dif7.lag1,
                             PM10.mean.ln.dif7.lead1, PM2.5.mean, PM2.5.mean.ln, PM2.5.mean.dif7, PM2.5.mean.dif7.lag1,
                             PM2.5.mean.ln.dif7, PM2.5.mean.ln.dif7.lag1, PM2.5.mean.ln.dif7.lead1, Bank.hol)
View(Int_missing_dif_lag)
write.csv(Int_missing_dif_lag, file = "Int_missing_dif_lag.csv", na = "")
