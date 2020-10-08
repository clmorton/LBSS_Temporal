# Load the required libraries for the analysis - use the function instal.packages() if it's the first time
library(VIM)
library(FactoMineR)
library(missMDA)
library(imputeTS)

# Load the dataset with missing observations
data <- read.csv('Data/Monitors/Defra/Blooms_mean_missing.csv')

View(data)
summary(data, digits = 5)
plot(data)

# Examine occurence of missing observations in the entire dataset
aggr(data)
matrixplot(data, sortby = 'PM10')

## Multivariate imputation
# Creation of a categorical data set with "o" when observed and "m" when missing for mean data
pattern <- matrix("o",nrow=nrow(data),ncol=ncol(data))
pattern[is.na(data)] <- "m"
pattern<-as.data.frame(pattern)
dimnames(pattern) <- dimnames(data)

# MCA - examine the clustering of the variables based on missing observations
res.mca<-MCA(pattern,graph=F)
plot(res.mca,selectMod=grep("_m",rownames(res.mca$var$coord)),invisible="ind")

# Determine the appropriate number of dimensions to base the factor analysis on - ncp needs to be changed
res.ncp<-estim_ncpPCA(data,method.cv="Kfold")
plot(res.ncp$criterion~names(res.ncp$criterion),xlab="number of dimensions")
ncp<-2

#single imputation
res.imp<-imputePCA(data,ncp = ncp)
#PCA on the imputed data set
res.PCA<-PCA(res.imp$completeObs,graph=F)
#Graph of the variables and graph of the individuals
plot(res.PCA,choix="var")
plot(res.PCA,choix="ind")

res.BayesMIPCA<-MIPCA(X=data,nboot=100,ncp=ncp,method.mi="Bayes")

imp1<-res.BayesMIPCA$res.MI[[1]]
summary(imp1)
View(imp1)

# Examine the quality of the imputation and save the dataset
Overimpute(res.BayesMIPCA,plotvars=1:11)
write.csv(imp1, file = 'Blooms_Mean_imputed_MCA.csv')

## Univariate imputation
# Arrange the dataset as a timeseries 
ts.data <- ts(data, start = 2012, frequency = 365.25)
View(ts.data)

# Create single variable vectors from the dataset
ts.data.ozone <-ts.data[,1]
ts.data.NO <-ts.data[,2]
ts.data.NO2 <- ts.data[,3]
ts.data.NOX <- ts.data[,4]
ts.data.SO2 <- ts.data[,5]
ts.data.PM10 <- ts.data[,6]
ts.data.NV.PM10 <- ts.data[,7]
ts.data.V.PM10 <- ts.data[,8]
ts.data.PM2.5 <- ts.data[,9]
ts.data.NV.PM2.5 <- ts.data[,10]
ts.data.V.PM2.5 <- ts.data[,11]

# Examine the variables for missing variables for each time point
plotNA.distribution(ts.data.ozone)
plotNA.distribution(ts.data.NO)
plotNA.distribution(ts.data.NO2)
plotNA.distribution(ts.data.NOX)
plotNA.distribution(ts.data.SO2)
plotNA.distribution(ts.data.PM10)
plotNA.distribution(ts.data.NV.PM10)
plotNA.distribution(ts.data.V.PM10)
plotNA.distribution(ts.data.PM2.5)
plotNA.distribution(ts.data.NV.PM2.5)
plotNA.distribution(ts.data.V.PM2.5)

# Examine the variables for missing variables by time segments
plotNA.distributionBar(ts.data.ozone)
plotNA.distributionBar(ts.data.NO)
plotNA.distributionBar(ts.data.NO2)
plotNA.distributionBar(ts.data.NOX)
plotNA.distributionBar(ts.data.SO2)
plotNA.distributionBar(ts.data.PM10)
plotNA.distributionBar(ts.data.NV.PM10)
plotNA.distributionBar(ts.data.V.PM10)
plotNA.distributionBar(ts.data.PM2.5)
plotNA.distributionBar(ts.data.NV.PM2.5)
plotNA.distributionBar(ts.data.V.PM2.5)

# Impute missing observations using a Kalman filter
ts.data.ozone.imp <- na.kalman(ts.data.ozone, model = 'auto.arima')
ts.data.NO.imp <- na.kalman(ts.data.NO, model = 'auto.arima')
ts.data.NO2.imp <- na.kalman(ts.data.NO2, model = 'auto.arima')
ts.data.NOX.imp <- na.kalman(ts.data.NOX, model = 'auto.arima')
ts.data.SO2.imp <- na.kalman(ts.data.SO2, model = 'auto.arima')
ts.data.PM10.imp <- na.kalman(ts.data.PM10, model = 'auto.arima')
ts.data.NV.PM10.imp <- na.kalman(ts.data.NV.PM10, model = 'auto.arima')
ts.data.V.PM10.imp <- na.kalman(ts.data.V.PM10, model = 'auto.arima')
ts.data.PM2.5.imp <- na.kalman(ts.data.PM2.5, model = 'auto.arima')
ts.data.NV.PM2.5.imp <- na.kalman(ts.data.NV.PM2.5, model = 'auto.arima')
ts.data.V.PM2.5.imp <- na.kalman(ts.data.V.PM2.5, model = 'auto.arima')

# Examine the imputed observations in situ with the known observations
plotNA.imputations(ts.data.ozone, ts.data.ozone.imp)
plotNA.imputations(ts.data.NO, ts.data.NO.imp)
plotNA.imputations(ts.data.NO2, ts.data.NO2.imp)
plotNA.imputations(ts.data.NOX, ts.data.NOX.imp)
plotNA.imputations(ts.data.PM10, ts.data.PM10.imp)
plotNA.imputations(ts.data.NV.PM10, ts.data.NV.PM10.imp)
plotNA.imputations(ts.data.V.PM10, ts.data.V.PM10.imp)
plotNA.imputations(ts.data.PM2.5, ts.data.PM2.5.imp)
plotNA.imputations(ts.data.NV.PM2.5, ts.data.NV.PM2.5.imp)
plotNA.imputations(ts.data.V.PM2.5, ts.data.V.PM2.5.imp)

# Save new data
data.imputed <- cbind.data.frame(ts.data.ozone.imp, ts.data.NO.imp, ts.data.NO2.imp, ts.data.NOX.imp, 
                                 ts.data.PM10.imp, ts.data.NV.PM10.imp, ts.data.V.PM10.imp, 
                                 ts.data.PM2.5.imp, ts.data.NV.PM2.5.imp, ts.data.V.PM2.5.imp)
View(data.imputed)
write.csv(data.imputed, file = 'Blooms_Mean_imputed_Kalman.csv')


