#reading data into data frame 
Oenergy <- read.csv(file.choose())
energy1 <- Oenergy[,c(2,3,4,6)]
energy1

#removing some observation form the start in order to make the analysis easier 
#taking prices from year 2019
energy1[c(-(1:1126351)),] -> energy1

#renaming the columns 
colnames(energy1)[1] = "Date"
colnames(energy1)[2] = "Hour"
colnames(energy1)[3] = "System"
colnames(energy1)[4] = "Price"

#subletting on the basis of system = HU
energy1[energy1$System=="HU",] -> huenergy

#separating price column from hu data
huenergy1 <- huenergy[,4]

#downloading timeseries package 
install.packages("lubridate")
library(lubridate)

#converting prices into time series data type 
## reference for frequency is 24 = https://stackoverflow.com/questions/55300193/convert-dataframe-that-contains-hourly-time-series-to-ts-object-in-r
energy_ts <- ts(huenergy1, decimal_date(ymd("2019-01-01")),decimal_date(ymd("2022-01-01")),frequency = 24)

#plotting the Time series 
plot(energy_ts)
max(energy_ts)

##Using Arima model to forecast
#checking if the data is stationary 
install.packages("tseries")
library(tseries)
adf.test(energy_ts)

#setting up the Arima model 
install.packages("forecast")
library(forecast)
energy_arima = auto.arima(energy_ts, ic="aic",trace= TRUE)

#checking if this model is stationary 
adf.test(ts(energy_arima$residuals))

#forecast
price_forecast = forecast(energy_arima, level = c(95),h =24)
price_forecast
plot(price_forecast)

#forecasting using simple forecast function 
forecast(energy_ts,24) -> simple_forecast_hu
simple_forecast_hu

#storing the mean forecast values as a vector 
simple_forecast_hu[["mean"]] -> simple_forecast_mean_values
price_forecast[["mean"]] -> arima_forecast_mean_values

#comparing simple forecast with ARIMA forecast 
(simple_forecast_mean_values) - (arima_forecast_mean_values) -> difference_forecast
plot(difference_forecast)


## why there is gap in the real data and the forecast - 
###https://stackoverflow.com/questions/54859500/gap-between-forecast-and-actual-data-in-ggplot

#subletting on the basis of system = DK1
# system DK1 and DK2 have same dataset and hence time series analysis and forecasts are same
energy1[energy1$System=="DK1",] -> dk1energy


#separating price column from dk1 data
dk1energy1 <- dk1energy[,4]


#converting prices into time series data type 
## reference for frequency is 24 = https://stackoverflow.com/questions/55300193/convert-dataframe-that-contains-hourly-time-series-to-ts-object-in-r
energydk1_ts <- ts(dk1energy1, decimal_date(ymd("2019-01-01")), decimal_date(ymd("2022-01-01")),frequency = 24)

#plotting the Time sereis 
plot(energydk1_ts)
max(energydk1_ts)

##Using Arima model to forecast
#checking if the data is stationary (step 1)
adf.test(energydk1_ts)

#setting up the Arima model 
energydk1_arima = auto.arima(energydk1_ts, ic="aic",trace= TRUE)

#checking if this model is stationary 
adf.test(ts(energydk1_arima$residuals))

#forecast
price_forecastdk1 = forecast(energydk1_arima, level = c(95),h =2)
price_forecastdk1
plot(price_forecastdk1)

#subletting on the basis of system = EE
energy1[energy1$System=="EE",] -> eeenergy


#separating price column from hu data
eeenergy1 <- eeenergy[,4]


#converting prices into time series data type 
## reference for frequency is 24 = https://stackoverflow.com/questions/55300193/convert-dataframe-that-contains-hourly-time-series-to-ts-object-in-r
energyee_ts <- ts(eeenergy1, decimal_date(ymd("2019-01-01")), decimal_date(ymd("2022-01-01")),frequency = 24)

#plotting the Time series 
plot(energyee_ts)
max(energyee_ts)

##Using Arima model to forecast
#checking if the data is stationary 

adf.test(energyee_ts)

#setting up the Arima model 

energyee_arima = auto.arima(energyee_ts, ic="aic",trace= TRUE)

#checking if this model is stationary 
adf.test(ts(energyee_arima$residuals))

#forecast
price_forecastee = forecast(energyee_arima, level = c(95),h =24)
price_forecastee
plot(price_forecastee)

#subletting on the basis of system = ES
energy1[energy1$System=="ES",] -> esenergy


#separating price column from hu data
esenergy1 <- esenergy[,4]


#converting prices into time series data type 
## reference for frequency is 24 = https://stackoverflow.com/questions/55300193/convert-dataframe-that-contains-hourly-time-series-to-ts-object-in-r
energyes_ts <- ts(esenergy1, decimal_date(ymd("2019-01-01")), decimal_date(ymd("2022-01-01")),frequency = 24)

#plotting the Time series 
plot(energyes_ts)
max(energyes_ts)

##Using Arima model to forecast
#checking if the data is stationary 

adf.test(energyes_ts)

#setting up the Arima model 

energyes_arima = auto.arima(energyes_ts, ic="aic",trace= TRUE)

#checking if this model is stationary 
adf.test(ts(energyes_arima$residuals))

#forecast
price_forecastes = forecast(energyes_arima, level = c(95),h =24)
price_forecastes
plot(price_forecastes)

#subletting on the basis of system = NO1
# NO1 and NO2 are same
energy1[energy1$System=="NO1",] -> no1energy


#separating price column from hu data
no1energy1 <- no1energy[,4]


#converting prices into time series data type 
## refernce for friquency is 24 = https://stackoverflow.com/questions/55300193/convert-dataframe-that-contains-hourly-time-series-to-ts-object-in-r
energyno1_ts <- ts(no1energy1, decimal_date(ymd("2019-01-01")), decimal_date(ymd("2022-01-01")),frequency = 24)

#plotting the Time sereis 
plot(energyno1_ts)
max(energyno1_ts)

##Using Arima model to forecast
#checking if the data is stationary 

adf.test(energyno1_ts)

#setting up the Arima model 

energyno1_arima = auto.arima(energyno1_ts, ic="aic",trace= TRUE)

#checking if this model is stationary 
adf.test(ts(energyno1_arima$residuals))

#forecast
price_forecastno1 = forecast(energyno1_arima, level = c(95),h =24)
price_forecastno1
plot(price_forecastno1)



#subletting on the basis of system = NO3
# system NO3 amd NO4 are same
energy1[energy1$System=="NO3",] -> no3energy


#separating price column from hu data
no3energy1 <- no3energy[,4]


#converting prices into time series data type 
## reference for frequency is 24 = https://stackoverflow.com/questions/55300193/convert-dataframe-that-contains-hourly-time-series-to-ts-object-in-r
energyno3_ts <- ts(no3energy1, decimal_date(ymd("2019-01-01")), decimal_date(ymd("2022-01-01")),frequency = 24)

#plotting the Time series 
plot(energyno3_ts)
max(energyno3_ts)

##Using Arima model to forecast
#checking if the data is stationary 

adf.test(energyno3_ts)

#setting up the Arima model 

energyno3_arima = auto.arima(energyno3_ts, ic="aic",trace= TRUE)

#checking if this model is stationary 
adf.test(ts(energyno3_arima$residuals))

#forecast
price_forecastno3 = forecast(energyno3_arima, level = c(95),h =24)
price_forecastno3
plot(price_forecastno3)

