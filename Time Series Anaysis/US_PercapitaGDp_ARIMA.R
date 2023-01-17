#importing data - step 1
install.packages("WDI")
library(WDI)
gdp <- WDI(country=c("US", "CA", "GB"), indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"), start=1960, end=2020) 

# Renaming Columns
names(gdp) <- c("country", "iso2c", "abbr", "year", "perCapGDP","GDP")
gdp

#Subsetting data for only United States Data

us = gdp$perCapGDP[gdp$country == "United States"] 
us
library(forecast)

# Converting Data to Time-Series Object 
us <- ts(us, start=min(gdp$year), end=max(gdp$year)) ; us

#plotting - step 2
ts.plot(us, ylab="Per Capita GDP", xlab="Year", main = "Per Capita GDP of United States")

#observation - clear trend

#ACF and PAcF - step 3

acf_x = acf(us)
# lags upto 15 lies above confidence interval that means 
#for a specific current value is affected by the previous 15th value 
pacf_x = pacf(us)
#only the first value lies above confience interval
#as it is the correlation of itself

#creating an arima model - step-4
#creating a model using auto.arima


arima_model <- arima(us, order = c(2,2,1))
summary(arima_model)

#prediction - step -5
arima_forecast <- predict(arima_model, n.ahead = 20)$pred
arima_forecast

arima_error <- predict(arima_model, n.ahead = 20)$se ; arima_error


#plotting - step-6
#initial plot
ts.plot(us, ylab="Per Capita GDP", xlab="Year", main = "Per Capita GDP of United States", xlim = c(1960,2041),ylim = c(0,100000))

#plotting forecast with positive and negative error
points(arima_forecast, type = "l", col = 2)
points(arima_forecast - 2*arima_error, type = "l", col = 2, lty = 2)
points(arima_forecast + 2*arima_error, type = "l", col = 2, lty = 2)

#as the data previous times series data the forcasted value for next 20 years also show a clear trend
#that means us percapita gdp will continue to increase for the next 20 years


            