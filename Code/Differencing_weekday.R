#Required packages - sed the function instal.packages() if using a package for the first time

library(readr)

#Attach data
Int_missing_weekday <- read_csv("Data/Int_missing_weekday.csv", 
                        col_types = cols(Date = col_date(format = "%d/%m/%Y")))
data <- ts(Int_missing_weekday, frequency = 365.25, start = 2012)

#Create lag and difference variables

Fob <- data[,"Fob"]
Fob.ln <- log(data[,"Fob"])
Fob.dif5 <- diff((data[,"Fob"]), 5)
Fob.ln.dif5 <- diff(Fob.ln, 5)
Fob.ln.dif5.lag1 <- lag(Fob.ln.dif5, -1)
Fob.ln.dif5.lag5 <- lag(Fob.ln.dif5, -5)

Card <- data[,"Card"]
Card.ln <- log(data[,"Card"])
Card.dif5 <- diff((data[,"Card"]), 5)
Card.ln.dif5 <- diff(Card.ln, 5)
Card.ln.dif5.lag1 <- lag(Card.ln.dif5, -1)
Card.ln.dif5.lag5 <- lag(Card.ln.dif5, -5)

Max.air.temp <- data[,"Max.air.temp"]
Max.air.temp.ln <- log(data[,"Max.air.temp"])
Max.air.temp.dif5 <- diff((data[,"Max.air.temp"]), 5)
Max.air.temp.dif5.lag1 <- lag(Max.air.temp.dif5, -1)
Max.air.temp.dif5.lead1 <- lag(Max.air.temp.dif5, 1)
Max.air.temp.ln.dif5 <- diff(Max.air.temp.ln, 5)
Max.air.temp.ln.dif5.lag1 <- lag(Max.air.temp.ln.dif5, -1)
Max.air.temp.ln.dif5.lead1 <- lag(Max.air.temp.ln.dif5, 1)

Percip <- data[,"Percip"]
Percip.light <- ifelse(Percip > 0 & Percip < 5, 1, 0)
Percip.light.lag1 <- lag(Percip.light, -1)
Percip.light.lead1 <- lag(Percip.light, 1)
Percip.heavy <- ifelse(Percip >= 5, 1, 0)
Percip.heavy.lag1 <- lag(Percip.heavy, -1)
Percip.heavy.lead1 <- lag(Percip.heavy, 1)
Percip.dif5 <- diff((data[,"Percip"]), 5)
Percip.dif5.lag1 <- lag(Percip.dif5, -1)
Percip.dif5.lead1 <- lag(Percip.dif5, 1)

Rel.humid <- data[,"Rel.humid"]
Rel.humid.ln <- log(data[,"Rel.humid"])
Rel.humid.dif5 <- diff((data[,"Rel.humid"]), 5)
Rel.humid.dif5.lag1 <- lag(Rel.humid.dif5, -1)
Rel.humid.dif5.lead1 <- lag(Rel.humid.dif5, 1)
Rel.humid.ln.dif5 <- diff(Rel.humid.ln, 5)
Rel.humid.ln.dif5.lag1 <- lag(Rel.humid.ln.dif5, -1)
Rel.humid.ln.dif5.lead1 <- lag(Rel.humid.ln.dif5, 1)

Avg.wind <- data[,"Avg.wind"]
Avg.wind.ln <- log(data[,"Avg.wind"])
Avg.wind.dif5 <- diff((data[,"Avg.wind"]), 5)
Avg.wind.dif5.lag1 <- lag(Avg.wind.dif5, -1)
Avg.wind.dif5.lead1 <- lag(Avg.wind.dif5, 1)
Avg.wind.ln.dif5 <- diff(Avg.wind.ln, 5)
Avg.wind.ln.dif5.lag1 <- lag(Avg.wind.ln.dif5, -1)
Avg.wind.ln.dif5.lead1 <- lag(Avg.wind.ln.dif5, 1)

Daylight <- data[,'Daylight']
Daylight.ln <- log(data[,"Daylight"])
Daylight.dif5 <- diff((data[,"Daylight"]), 5)
Daylight.ln.dif5 <- diff(Daylight.ln, 5)

Ozone.mean <- data[,"Ozone.mean"]
Ozone.mean.ln <- log(data[,"Ozone.mean"])
Ozone.mean.dif5 <- diff((data[,"Ozone.mean"]), 5)
Ozone.mean.dif5.lag1 <- lag(Ozone.mean.dif5, -1)
Ozone.mean.ln.dif5 <- diff(Ozone.mean.ln, 5)
Ozone.mean.ln.dif5.lag1 <- lag(Ozone.mean.ln.dif5, -1)
Ozone.mean.ln.dif5.lead1 <- lag(Ozone.mean.ln.dif5, 1)

NOX.mean <- data[,"NOX.mean"]
NOX.mean.ln <- log(data[,"NOX.mean"])
NOX.mean.dif5 <- diff((data[,"NOX.mean"]), 5)
NOX.mean.dif5.lag1 <- lag(NOX.mean.dif5, -1)
NOX.mean.ln.dif5 <- diff(NOX.mean.ln, 5)
NOX.mean.ln.dif5.lag1 <- lag(NOX.mean.ln.dif5, -1)
NOX.mean.ln.dif5.lead1 <- lag(NOX.mean.ln.dif5, 1)

PM10.mean <- data[,"PM10.mean"]
PM10.mean.ln <- log(data[,"PM10.mean"])
PM10.mean.dif5 <- diff((data[,"PM10.mean"]), 5)
PM10.mean.dif5.lag1 <- lag(PM10.mean.dif5, -1)
PM10.mean.ln.dif5 <- diff(PM10.mean.ln, 5)
PM10.mean.ln.dif5.lag1 <- lag(PM10.mean.ln.dif5, -1)
PM10.mean.ln.dif5.lead1 <- lag(PM10.mean.ln.dif5, 1)

PM2.5.mean <- data[,"PM2.5.mean"]
PM2.5.mean.ln <- log(data[,"PM2.5.mean"])
PM2.5.mean.dif5 <- diff((data[,"PM2.5.mean"]), 5)
PM2.5.mean.dif5.lag1 <- lag(PM2.5.mean.dif5, -1)
PM2.5.mean.ln.dif5 <- diff(PM2.5.mean.ln, 5)
PM2.5.mean.ln.dif5.lag1 <- lag(PM2.5.mean.ln.dif5, -1)
PM2.5.mean.ln.dif5.lead1 <- lag(PM2.5.mean.ln.dif5, 1)

Bank.hol <- data[,"Bank.hol"]

#Save lag and difference variables
Int_missing_dif_lag_weekday <- cbind(Fob, Fob.ln, Fob.dif5, Fob.ln.dif5, Fob.ln.dif5.lag1, Fob.ln.dif5.lag5, Card, Card.ln, Card.dif5,
                             Card.ln.dif5, Card.ln.dif5.lag1, Card.ln.dif5.lag5, Max.air.temp, Max.air.temp.ln, Max.air.temp.dif5,
                             Max.air.temp.dif5.lag1, Max.air.temp.dif5.lead1, Max.air.temp.ln.dif5, Max.air.temp.ln.dif5.lag1,
                             Max.air.temp.ln.dif5.lead1, Percip, Percip.light, Percip.light.lag1, Percip.light.lead1,
                             Percip.heavy, Percip.heavy.lag1, Percip.heavy.lead1, Percip.dif5, Percip.dif5.lag1,
                             Percip.dif5.lead1, Rel.humid, Rel.humid.ln, Rel.humid.dif5, Rel.humid.dif5.lag1,
                             Rel.humid.dif5.lead1, Rel.humid.ln.dif5, Rel.humid.ln.dif5.lag1, Rel.humid.ln.dif5.lead1,
                             Avg.wind, Avg.wind.ln, Avg.wind.dif5, Avg.wind.dif5.lag1, Avg.wind.dif5.lead1, Avg.wind.ln.dif5,
                             Avg.wind.ln.dif5.lag1, Avg.wind.ln.dif5.lead1, Daylight, Daylight.ln, Daylight.dif5,
                             Daylight.ln.dif5, Ozone.mean, Ozone.mean.ln, Ozone.mean.dif5, Ozone.mean.dif5.lag1, Ozone.mean.ln.dif5,
                             Ozone.mean.ln.dif5.lag1, Ozone.mean.ln.dif5.lead1, NOX.mean, NOX.mean.ln, NOX.mean.dif5,
                             NOX.mean.dif5.lag1, NOX.mean.ln.dif5, NOX.mean.ln.dif5.lag1, NOX.mean.ln.dif5.lead1, PM10.mean,
                             PM10.mean.ln, PM10.mean.dif5, PM10.mean.dif5.lag1, PM10.mean.ln.dif5, PM10.mean.ln.dif5.lag1,
                             PM10.mean.ln.dif5.lead1, PM2.5.mean, PM2.5.mean.ln, PM2.5.mean.dif5, PM2.5.mean.dif5.lag1,
                             PM2.5.mean.ln.dif5, PM2.5.mean.ln.dif5.lag1, PM2.5.mean.ln.dif5.lead1, Bank.hol)
View(Int_missing_dif_lag_weekday)
write.csv(Int_missing_dif_lag_weekday, file = "Int_missing_dif_lag_weekday.csv", na = "")
