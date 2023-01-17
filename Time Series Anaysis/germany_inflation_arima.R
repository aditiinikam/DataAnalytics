#importing the data - step 1
#importdat with scan()

inflation = scan()

#time series object
inflation_ts = ts(inflation, start = c(2008), frequency = 12)

#since data is avaialble from august 1948 we use c(1948,7)
inflation_ts

ts.plot(inflation_ts, xlab = "Time in years", ylab = "Inflation Rate", main = "Germany Inflation Rates jan 2008 - oct 2020")

#their is no trend
#sesaonality is present

#step -2 decomposition 
decomposed = decompose(inflation_ts)
library(ggplot2)
autoplot(decomposed)
#sesanality is present inflation can be seen at peek during holidays (christms) end as it is a high period of consumption


#ACF and PAcF - step 3

acf_x = acf(inflation_ts)


pacf_x = pacf(inflation_ts)
#significant lag at 1

#step -4 modelling

model_arima_inflation = auto.arima(inflation_ts, 
                             stepwise = T, 
                             approximation = F)
model_arima_inflation
#auto.arima gives us the best model

#prediction - step -5
arima_forecast_1 <- predict(model_arima_inflation, n.ahead = 24)$pred
arima_forecast_1

arima_error_1 <- predict(model_arima_inflation, n.ahead = 24)$se ; arima_error_1


#plotting - step-6
#initial plot
ts.plot(inflation_ts, xlab = "Time in years", ylab = "Inflation Rate", main = "Predicited Monthly Inflation (ARIMA(1,0,2)(0,1,1)[12])", xlim = c(2008,2020))
#plotting forecast with positive and negative error
points(arima_forecast_1, type = "l", col = "blue")
points(arima_forecast_1 - 2*arima_error_1, type = "l", col = 2, lty = 2)
points(arima_forecast_1 + 2*arima_error_1, type = "l", col = 2, lty = 2)

#forecast of 2 cycles i.e. 2 years is obtained from the above model ARIMA(1,0,2)(0,1,1)[12]

