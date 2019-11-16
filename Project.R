## ========================================================================================= ##
## Set the project working directory                                                         ##
## ========================================================================================= ##
getwd()
setwd("D:/UniSA, Mawson Lakes/Semester 4/Advanced Analytics Techniques 1/Project")
install.packages("forecast")
library(forecast)
## ========================================================================================= ##
## ========================================================================================= ##

## ========================================================================================= ##
## Step 1: Import the required dataset for the project
## ========================================================================================= ##
data()
lynxData = lynx
windows()
plot(lynxData,
     xlab = "Year",
     ylab = "No. of Lynx Trapped",
     main = "Figure 1.1: No. of Lynx Trapped Annually\nfrom the years 1821 - 1934")
windows()
acf(lynxData,
    lag.max = 60,
    main = "Figure 1.2: ACF for Lynx Trapped Annually\nfrom the years 1821 - 1934")
windows()
pacf(lynxData,
     lag.max = 60,
     main = "Figure 1.3: PACF for Lynx Trapped Annually\nfrom the years 1821 - 1934")

## ========================================================================================= ##
## Step 2: Examine the data for seasonality during different periods in time
## ========================================================================================= ##
lynxData1821To1860 = window(lynxData, start = c(1821), end = c(1860))
lynxData1861To1900 = window(lynxData, start = c(1861), end = c(1900))
lynxData1901To1940 = window(lynxData, start = c(1901), end = c(1940))
windows()
par(mfrow = c(3, 1))
plot(lynxData1821To1860,
     xlab = "Year",
     ylab = "No. of Lynx Trapped",
     main = "Figure 1.4: No. of Lynx Trapped Annually\nfrom the years 1821 - 1860")
plot(lynxData1861To1900,
     xlab = "Year",
     ylab = "No. of Lynx Trapped",
     main = "Figure 1.5: No. of Lynx Trapped Annually\nfrom the years 1861 - 1900")
plot(lynxData1901To1940,
     xlab = "Year",
     ylab = "No. of Lynx Trapped",
     main = "Figure 1.6: No. of Lynx Trapped Annually\nfrom the years 1901 - 1940")

## ========================================================================================= ##
## Step 3: Identify the order of differencing that eliminates trend and seasonal effects
## ========================================================================================= ##
lynxDataDiff = diff(lynxData, lag = 9)
windows()
plot(lynxDataDiff,
     xlab = "Year",
     ylab = "No. of Lynx Trapped",
     main = "Figure 1.7: No. of Lynx Trapped Annually\nfrom the years 1821 - 1934 after taking 'diff = 9'")
windows()
acf(lynxDataDiff,
    lag.max = 60,
    main = "Figure 1.8: ACF for Lynx Trapped Annually\nfrom the years 1821 - 1934 after taking 'diff = 9'")
windows()
pacf(lynxDataDiff,
     lag.max = 60,
     main = "Figure 1.9: PACF for Lynx Trapped Annually\nfrom the years 1821 - 1934 after taking 'diff = 9'")

# for(i in c(1:10)){
#   windows()
#   plot(diff(lynxData, lag = i))
# }

## ========================================================================================= ##
## Step 4: Fit multiple models to the data and compare for the best model 
## ========================================================================================= ##
arimaDataFrame = data.frame(p = as.numeric(),
                            d = as.numeric(),
                            q = as.numeric(),
                            P = as.numeric(),
                            D = as.numeric(),
                            Q = as.numeric(),
                            period = as.numeric(),
                            AIC = as.numeric())
for(p in c(0, 1, 2)){
  for(d in c(0, 1, 2)){
    for (q in c(0, 1, 2)){
      for(P in c(0, 1, 2)){
        for(D in c(0, 1, 2)){
          for (Q in c(0, 1, 2)){
            for(years in c(9, 10)){
              lynxDataDiffARIMA = arima(lynxData, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = years))
              arimaDataFrame = rbind(arimaDataFrame, data.frame(p = p, d = d, q = q,
                                                                P = P, D = D, Q = Q,
                                                                period = years, AIC = lynxDataDiffARIMA$aic))
            }
          }
        }
      }
    }
  }
}

bestARIMAModel = arimaDataFrame[arimaDataFrame$AIC == min(arimaDataFrame$AIC), ]
print(paste("The best Seasonal ARIMA Model fitted to the data is given by the parameters p:", bestARIMAModel$p,
            ", d:", bestARIMAModel$d,
            ", q:", bestARIMAModel$q,
            ", P:", bestARIMAModel$P,
            ", D:", bestARIMAModel$D,
            ", Q:", bestARIMAModel$Q,
            ", Period:", bestARIMAModel$period,
            "with AIC:", bestARIMAModel$AIC))
lynxDataDiffARIMA = arima(lynxData, order = c(bestARIMAModel$p, bestARIMAModel$d, bestARIMAModel$q),
                          seasonal = list(order = c(bestARIMAModel$P, bestARIMAModel$D, bestARIMAModel$Q),
                                          period = bestARIMAModel$period))

lynxDataDiffARIMA = arima(lynxData, order = c(1, 1, 2), seasonal = list(order = c(0, 2, 1), period = 9))
lynxDataDiffARIMA
windows()
acf(lynxDataDiffARIMA$residuals,
    main = "Figure 1.10: ACF for residuals of ARIMA model\nfitted to Lynx Trapped Annually",
    lag.max = 100)

## ========================================================================================= ##
## Step 5: Forecast future lynx trappings
## ========================================================================================= ##
windows()
plot(forecast(lynxDataDiffARIMA, h = 40),
     xlab = "Years", ylab = "No. of Lynx Trapped Annually",
     main = "Figure 1.11: Forecast for No. of Lynx Trapped Annually\nFrom the years 1935 to 1974")