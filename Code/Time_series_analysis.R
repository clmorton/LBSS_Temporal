#This code covers the statistical analysis of the bicycle data including descriptive statistics, univariate analysis, and multivariate models of cycle frequency
#Required Packages - use the function instal.packages() if using a package for the first time
library(astsa)
library(fpp2)
library(car)
library(readr)
library(psych)
library(seasonal)
library(tseries)
library(urca)
library(sandwich)
library(lmtest)
library(forecast)

#Setting up project directory and loading dataset
setwd(.../LBSS_Temporal)
Int_missing_dif_ma_lag <- read_csv("Data/Int_missing_dif_ma_lag.csv", 
                                col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                                na = "empty")
Int_missing_dif_ma_lag_2016 <- read_csv("Data/Int_missing_dif_ma_lag_2016.csv", 
                                     col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                                     na = "empty")
View(Int_missing_dif_ma_lag)
attach(Int_missing_dif_ma_lag)
data <- ts(Int_missing_dif_ma_lag, frequency = 365.25, start = 2012)
View(data)
data2016 <- ts(Int_missing_dif_ma_lag_2016, frequency = 365.25, start = 2016)
Hourly <- read_csv("Data/Hourly_data.csv")
Daily <- read_csv("Data/Daily_data.csv")

#Summary of variables included in the dataset
summary(Int_missing_dif_ma_lag)
summary(data)

#Timeseries line plots for bike hires, weather, and air pollution
autoplot(data[,c("Fob", "Fob.ma9.res", "Card", "Card.ma9.res")], facets=TRUE) +
  xlab("Year") + ylab("") +
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10)) 

autoplot(data2016[,c("Fob", "Fob.ma9.res", "Card", "Card.ma9.res")], facets=TRUE) +
  xlab("Year") + ylab("Users") +
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10)) 

autoplot(data2016[,c("Fob", "Card")], facets=TRUE) +
  xlab("Year") + ylab("Users") +
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10)) 

autoplot(data[,c("Max.air.temp", "Avg.wind", "Percip", "Rel.humid")], facets=TRUE) +
  xlab("Year") + ylab("") + 
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10))

autoplot(data2016[,c("Max.air.temp", "Avg.wind", "Percip", "Rel.humid")], facets=TRUE) +
  xlab("Year") + ylab("") + 
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10))

autoplot(data[,c("Max.air.temp.ma9.res", "Avg.wind.ma9.res", "Percip", "Rel.humid.ma9.res")], facets=TRUE) +
  xlab("Year") + ylab("") + 
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10))

autoplot(data[,c("Ozone.mean","NOX.mean", "PM10.mean")], facets=TRUE) +
  xlab("Year") + ylab("µg/m3") + 
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10))

autoplot(data2016[,c("Ozone.mean","NOX.mean", "PM10.mean")], facets=TRUE) +
  xlab("Year") + ylab("µg/m3") + 
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10))

autoplot(data[,c("Ozone.mean.ma9.res","NOX.mean.ma9.res", "PM10.mean.ma9.res")], facets=TRUE) +
  xlab("Year") + ylab("µg/m3") + 
  theme(axis.title = element_text(face ="bold", size=12)) +
  theme(axis.text = element_text(size=10))

#Timeseries plots for bike hires by hour of the day and day of the week

Fob_hourly <- ggplot(data=Hourly, aes(x=Hour, y=Mean.Fob)) +
  geom_bar(stat="identity")
Fob_hourly

Card_hourly <- ggplot(data=Hourly, aes(x=Hour, y=Mean.Card)) +
  geom_bar(stat="identity")
Card_hourly

Fob_daily <- ggplot(data=Daily, aes(x=Day, y=Mean.Fob)) +
  geom_bar(stat="identity")
Fob_daily

Card_daily <- ggplot(data=Daily, aes(x=Day, y=Mean.Card)) +
  geom_bar(stat="identity")
Card_daily

#Histograms of bike hires 
hist(data[,"Fob"], breaks = 150, xlab = "Number of Daily Hires by Fob", main = NULL, font.lab = 2, col = "gray80")
hist(data[,"Card"], breaks = 150, xlab = "Number of Daily Hires by Card", main = NULL, font.lab = 2, col = "gray80")

#Stationarity and dependence analysis for the bike hires
ggAcf(data[,"Fob"],main = "", lag.max = 25)
ggPacf(data[,"Fob"],main = "", lag.max = 25)
ggAcf(data[,"Fob.ma9.res"],main = "", lag.max = 25)
ggPacf(data[,"Fob.ma9.res"],main = "", lag.max = 25)

ggAcf(data[,"Card"],main = "", lag.max = 25)
ggPacf(data[,"Card"],main = "", lag.max = 25)
ggAcf(data[,"Card.ma9.res"],main = "", lag.max = 25)
ggPacf(data[,"Card.ma9.res"],main = "", lag.max = 25)

Box.test(data[,"Fob.ma9.res"], lag = 25, type = 'Ljung-Box')
Box.test(data[,"Card.ma9.res"], lag = 25, type = 'Ljung-Box')
data[,"Fob.ma9.res"] %>% ur.kpss() %>% summary()
data[,"Card.ma9.res"] %>% ur.kpss() %>% summary()

#Bivariate analysis between daily hires, weather, and air pollution
pairs.panels(Int_missing_dif_ma_lag[c(5, 17, 29, 42, 55, 71, 87)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
pairs.panels(Int_missing_dif_ma_lag[c(5, 17, 103, 119, 135)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)

#Boxplots for daily hires across bank holidays
ggplot(aes(y = Fob, x = as.factor(Bank.hol)), data = Int_missing_dif_ma_lag) + 
  geom_boxplot(size = 0.75) + xlab("") + ylab ("Daily Hires") +
  theme(axis.text.x=element_text(color = "black", size=10)) +
  theme(axis.text.y=element_text(color = "black", size=10)) +
  theme(axis.title.x=element_text(color = "black", face="bold" , size = 12)) +
  theme(axis.title.y=element_text(color = "black", face="bold" , size = 12)) +
  scale_x_discrete(labels = c('Non-Holiday', 'Bank Holiday'))

#Forecast models
for_fob <- auto.arima(data[,"Fob.ma9.res"])
summary(for_fob)

for_card <- auto.arima(data[,"Card.ma9.res"])
summary(for_card)

#OLS Autregression Distributed Laged models 

OLS1_Fob_ma9_res <- lm(Fob.ma9.res ~ Fob.ma9.res.lag1 +  Max.air.temp.ma9.res + Max.air.temp.ma9.res.lag1 + 
                        Percip.light + Percip.light.lag1 + Percip.heavy + Percip.heavy.lag1 + 
                        Avg.wind.ma9.res + Avg.wind.ma9.res.lag1 + Rel.humid.ma9.res + Rel.humid.ma9.res.lag1 + 
                        Daylight.ma9.res + Bank.hol)
summary(OLS1_Fob_ma9_res)
AIC(OLS1_Fob_ma9_res)
par(mfrow=c(2,2))
plot(OLS1_Fob_ma9_res, main = "OLS Fob Weather Model")
vif(OLS1_Fob_ma9_res)
anova(OLS1_Fob_ma9_res)
dwtest(OLS1_Fob_ma9_res)
bptest(OLS1_Fob_ma9_res)
checkresiduals(OLS1_Fob_ma9_res)
ggAcf(residuals(OLS1_Fob_ma9_res))
ggPacf(residuals(OLS1_Fob_ma9_res))
coeftest(OLS1_Fob_ma9_res, vcovHC)

OLS2_Fob_ma9_res <- lm(Fob.ma9.res ~ Fob.ma9.res.lag1 + Ozone.mean.ma9.res + Ozone.mean.ma9.res.lag1 + NOX.mean.ma9.res + NOX.mean.ma9.res.lag1 +
                          PM10.mean.ma9.res + PM10.mean.ma9.res.lag1 + Bank.hol)
summary(OLS2_Fob_ma9_res)
AIC(OLS2_Fob_ma9_res)
par(mfrow=c(2,2))
plot(OLS2_Fob_ma9_res, main = "OLS Fob Pollution Model")
vif(OLS2_Fob_ma9_res)
anova(OLS2_Fob_ma9_res)
dwtest(OLS2_Fob_ma9_res)
bptest(OLS2_Fob_ma9_res)
checkresiduals(OLS2_Fob_ma9_res)
ggAcf(residuals(OLS2_Fob_ma9_res))
ggPacf(residuals(OLS2_Fob_ma9_res))
coeftest(OLS2_Fob_ma9_res, vcovHC)

OLS3_Fob_ma9_res <- lm(Fob.ma9.res ~ Fob.ma9.res.lag1 + Max.air.temp.ma9.res + Max.air.temp.ma9.res.lag1 + 
                          Percip.light + Percip.light.lag1 + Percip.heavy + Percip.heavy.lag1 + 
                          Avg.wind.ma9.res + Avg.wind.ma9.res.lag1 + Rel.humid.ma9.res + Rel.humid.ma9.res.lag1 + 
                          Daylight.ma9.res + Ozone.mean.ma9.res + Ozone.mean.ma9.res.lag1 + NOX.mean.ma9.res + NOX.mean.ma9.res.lag1 +
                          PM10.mean.ma9.res + PM10.mean.ma9.res.lag1 + Bank.hol)
summary(OLS3_Fob_ma9_res)
AIC(OLS3_Fob_ma9_res)
par(mfrow=c(2,2))
plot(OLS3_Fob_ma9_res, main = "OLS Fob Integrated Model")
vif(OLS3_Fob_ma9_res)
anova(OLS3_Fob_ma9_res)
dwtest(OLS3_Fob_ma9_res)
bptest(OLS3_Fob_ma9_res)
checkresiduals(OLS3_Fob_ma9_res)
ggAcf(residuals(OLS3_Fob_ma9_res))
ggPacf(residuals(OLS3_Fob_ma9_res))
coeftest(OLS3_Fob_ma9_res, vcovHC)

OLS1_Card_ma9_res <- lm(Card.ma9.res ~ Card.ma9.res.lag1 + Max.air.temp.ma9.res + Max.air.temp.ma9.res.lag1 + 
                          Percip.light + Percip.light.lag1 + Percip.heavy + Percip.heavy.lag1 + 
                          Avg.wind.ma9.res + Avg.wind.ma9.res.lag1 + Rel.humid.ma9.res + Rel.humid.ma9.res.lag1 + 
                          Daylight.ma9.res + Bank.hol)
summary(OLS1_Card_ma9_res)
AIC(OLS1_Card_ma9_res)
par(mfrow=c(2,2))
plot(OLS1_Card_ma9_res, main = "OLS Card Weather Model")
vif(OLS1_Card_ma9_res)
anova(OLS1_Card_ma9_res)
dwtest(OLS1_Card_ma9_res)
bptest(OLS1_Card_ma9_res)
checkresiduals(OLS1_Card_ma9_res)
ggAcf(residuals(OLS1_Card_ma9_res))
ggPacf(residuals(OLS1_Card_ma9_res))
coeftest(OLS1_Card_ma9_res, vcovHC)

OLS2_Card_ma9_res <- lm(Card.ma9.res ~ Card.ma9.res.lag1 + Ozone.mean.ma9.res + Ozone.mean.ma9.res.lag1 + NOX.mean.ma9.res + NOX.mean.ma9.res.lag1 +
                          PM10.mean.ma9.res + PM10.mean.ma9.res.lag1 + Bank.hol)
summary(OLS2_Card_ma9_res)
AIC(OLS2_Card_ma9_res)
par(mfrow=c(2,2))
plot(OLS2_Card_ma9_res, main = "OLS Card Pollution Model")
vif(OLS2_Card_ma9_res)
anova(OLS2_Card_ma9_res)
dwtest(OLS2_Card_ma9_res)
bptest(OLS2_Card_ma9_res)
checkresiduals(OLS2_Card_ma9_res)
ggAcf(residuals(OLS2_Card_ma9_res))
ggPacf(residuals(OLS2_Card_ma9_res))
coeftest(OLS2_Card_ma9_res, vcovHC)

OLS3_Card_ma9_res <- lm(Card.ma9.res ~ Card.ma9.res.lag1 + Max.air.temp.ma9.res + Max.air.temp.ma9.res.lag1 + 
                          Percip.light + Percip.light.lag1 + Percip.heavy + Percip.heavy.lag1 + 
                          Avg.wind.ma9.res + Avg.wind.ma9.res.lag1 + Rel.humid.ma9.res + Rel.humid.ma9.res.lag1 + 
                          Daylight.ma9.res + Ozone.mean.ma9.res + Ozone.mean.ma9.res.lag1 + NOX.mean.ma9.res + NOX.mean.ma9.res.lag1 +
                          PM10.mean.ma9.res + PM10.mean.ma9.res.lag1 + Bank.hol)
summary(OLS3_Card_ma9_res)
AIC(OLS3_Card_ma9_res)
par(mfrow=c(2,2))
plot(OLS3_Card_ma9_res, main = "OLS Card Integrated Model")
vif(OLS3_Card_ma9_res)
anova(OLS3_Card_ma9_res)
dwtest(OLS3_Card_ma9_res)
bptest(OLS3_Card_ma9_res)
checkresiduals(OLS3_Card_ma9_res)
ggAcf(residuals(OLS3_Card_ma9_res))
ggPacf(residuals(OLS3_Card_ma9_res))
coeftest(OLS3_Card_ma9_res, vcovHC)

