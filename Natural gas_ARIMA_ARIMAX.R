library(adfExplorer)
library(AER)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(rucm)
library(sweep)
library(tidyr)
library(TTR)
library(TSA)
library(tseries)
library(tidyquant)
library(timetk)
library(WiSEBoot)
library(zoo)


data_Gas <- read.csv(file = "C:/Users/125911/Documents/Gas.csv")
data_Gas_ts <- ts(data_Gas$Price, start = c(2014, 1), end = c(2018, 12), frequency = 12)
plot(data_Gas_ts)


#***************Descriptive Statistics***********************************
class(data_Gas_ts)       # tells whether the data series is in a time series format
start(data_Gas_ts)       # check start of the time series
end(data_Gas_ts)         # check end of the time series
frequency(data_Gas_ts)   #Frequency of time series: (Here) The cycle is 12 months in a year
plot(data_Gas_ts)        # To plot the time series
abline(reg=lm(data_Gas_ts~time(data_Gas_ts)))  # To fit in a line
cycle(data_Gas_ts)       # To print the cycle across years
plot(aggregate(data_Gas_ts,FUN=mean)) # To aggregate the cycles and display a year on year trend
boxplot(data_Gas_ts~cycle(data_Gas_ts)) # Box plot across months will give us a sense on seasonal effect.
view(data_Gas_ts)



#*****************ARIMA MOdel*********************************************
# Stationary check on the original Series using Augmented Dickey-Fuller Test
tseries::adf.test((data_Gas_ts), alternative ='stationary', k = 0) # In Output p-value should be < 0.1
# Stationary check on the original Series with d = 1         # p-value was > 0.1, try with difference =1
tseries::adf.test(diff(data_Gas_ts), alternative ='stationary', k = 0) # at d =1, p-value < 0.1

# Automatic selection of aparameter p, d and q from auto function. 
auto.arima(data_Gas_ts)

#ACF / PACF Check :-> An alternate way to see p, d and q graphically.
ggAcf(data_Gas_ts)
ggPacf(data_Gas_ts)

# fitting of ARIMA model.
fit1 <- arima(data_Gas_ts[-c(55:60)], order = c(0, 1, 1))  # where we get p=0, d=1 and q=1 from prev. step
fit1
as.data.frame(fitted(fit1))
as.data.frame(predict(fit1, 6))   # compare predicted value with Standard error for 6 hidden datapoints

fit_Gas<-auto.arima(data_Gas_ts)
fcast<-forecast(fit_Gas,6)
accuracy(fcast,data_Gas_ts[c(55:60)])  #MAPE calulation for ARIMA model on training and test set



#****************** ARIMAX part******************************
Score_Gas <- read.csv(file = "C:/Users/125911/Documents/Score_Gas.csv")
as.data.frame(Score_Gas$polarity)
model1 = arimax(data_Gas_ts[-c(55:60)], order = c(0,1,1), xtransf = Score_Gas$polarity, method = c("ML"))

fcast1<-forecast(model1,6)
accuracy(fcast1,data_Gas_ts[c(55:60)])  #MAPE calulation for ARIMAX model on training and test set




