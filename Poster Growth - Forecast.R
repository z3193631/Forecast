library(Ecdat)
library(forecast)
library(zoo)
library(ggplot2)

poster <- read.csv("Marketing/Forecasting/poster-growth.csv", header = TRUE)
#time.points <- seq.Date(as.Date("2012-02-06"), by = 7, length.out = nrow(poster))
poster_series <- read.zoo(poster, format = "%d/%m/%Y")
knitr::kable(print(poster_series))
poster_ts <- ts(poster_series[1:301], frequency = 52)
print(poster_ts)
plot(poster_ts)

#Alternative Method using decompose
decompose_poster = decompose(poster_ts, "multiplicative")

plot(as.ts(decompose_poster$seasonal))
plot(as.ts(decompose_poster$trend))
plot(as.ts(decompose_poster$random))
plot(decompose_poster)

de_poster <- seasadj(decompose_poster)
plot(de_poster)

#Dickey-Fuller Test for stationary time series check
require(tseries)
adf.test(poster_ts, alternative = "stationary")
acf(poster_ts, main='')
pacf(poster_ts, main='')

count_d1 = diff(de_poster, differences = 7)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#Using TBATS model
poster_tb <- ts(read.csv("Marketing/Forecasting/poster-growth.csv", header = TRUE)
   [49:285,2], frequency=52)
postertbats <- tbats(poster_tb)
print(tbats.components(postertbats))
plot(postertbats)
accuracy(postertbats)
tbats_poster <- forecast(postertbats, h=52)
plot(tbats_poster, ylab="Poster Growth")
accuracy(tbats_poster)

cv <- ts(poster_series[49:301], frequency = 52)
plot(tbats_poster, type="l", col="green")
lines(cv, type="l", col="red")

#Export Result Out to CSV
fc_table <- data.frame(tbats_poster)
write.csv(fc_table, "Marketing/Forecasting - New Poster Growth/Output_TBATS.csv", row.names=F)

#Use Regression with ARIMA error model
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(poster_ts, xreg=fourier(poster_ts, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
print(i)
head(bestfit)
fc <- forecast(bestfit, xreg=fourier(poster_ts, K=1, h=26))
plot(fc)
head(fc)

#CROSS VALIDATION with Fourier series & Regression with ARIMA error
poster_test <- ts(read.csv("Marketing/Forecasting - New Poster Growth/poster_growth.csv", header = TRUE)
                  [1:285,2], frequency=52)

bestfit_test <- list(aicc=Inf)
for(x in 1:25)
{
  fit_test <- auto.arima(poster_test, xreg=fourier(poster_test, K=x), seasonal=FALSE)
  if(fit_test$aicc < bestfit_test$aicc)
    bestfit_test <- fit_test
  else break;
}
print(x)
bestfit_test <- auto.arima(poster_test, xreg=fourier(poster_test, K=2), seasonal=FALSE)
head(bestfit_test)
fc_test <- forecast(bestfit_test, xreg=fourier(poster_ts, K=2, h=26))

accuracy(fc_test)

plot( fc_test, type="l", col="green" )
lines( poster_ts, type="l", col="red" )



##############BY CITY###############
poster_city <- read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)

poster_sydney <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                [48:284,6], frequency=52)
poster_melbourne <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                [48:284,4], frequency=52)
poster_brisbane <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                [48:284,3], frequency=52)
poster_adelaide <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                [100:300,2], frequency=52)
poster_perth <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                [100:300,5], frequency=52)

#Perth
poster_tb_perth <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                [48:285,5], frequency = 52)
plot(poster_tb_perth)
postertbats_perth <- tbats(poster_tb_perth)
print(tbats.components(postertbats_perth))
plot(postertbats_perth)
tbats_poster_perth <- forecast(postertbats_perth, h=26)
plot(tbats_poster_perth, ylab="Poster Growth Perth")
accuracy(tbats_poster_perth)

cv_perth <- ts(poster_city[48:300,5], frequency = 52)
plot(tbats_poster_perth, type="l", col="green")
lines(cv_perth, type="l", col="purple")

require(tseries)
acf(poster_tb_perth, main='')
pacf(poster_tb_perth, main='')

#Adelaide
poster_tb_ad <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                      [48:300,2], frequency=52)
head(poster_tb_ad)
poster_tb_ad <- as.numeric(poster_tb_ad)
postertbats_ad <- tbats(poster_tb_ad)
print(tbats.components(postertbats_ad))
plot(postertbats_ad)
tbats_poster_ad <- forecast(postertbats_ad, h=26)
plot(tbats_poster_ad, ylab="Poster Growth Adelaide")
accuracy(tbats_poster_ad)

cv_ad <- ts(poster_city[48:300,2], frequency = 52)
plot(tbats_poster_ad, type="l", col="green")
lines(cv_ad, type="l", col="red")

#Melbourne
poster_tb_melb <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                      [100:285,4], frequency = 52)
plot(poster_tb_melb)
postertbats_melb <- tbats(poster_tb_melb)
print(tbats.components(postertbats_melb))
plot(postertbats_melb)
tbats_poster_melb <- forecast(postertbats_melb, h=26)
plot(tbats_poster_melb, ylab="Poster Growth Melbourne")
accuracy(tbats_poster_melb)

cv_melb <- ts(poster_city[100:300,4], frequency = 52)
plot(tbats_poster_melb, type="l", col="green")
lines(cv_melb, type="l", col="black")

#Sydney
poster_tb_sydney <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                     [48:300,6], frequency = 52)
plot(poster_tb_sydney)
postertbats_sydney <- tbats(poster_tb_sydney)
print(tbats.components(postertbats_sydney))
plot(postertbats_sydney)
tbats_poster_syd <- forecast(postertbats_sydney, h=26)
plot(tbats_poster_syd, ylab="Poster Growth Sydney")
accuracy(tbats_poster_syd)

cv_syd <- ts(poster_city[48:300,6], frequency = 52)
plot(tbats_poster_syd, type="l", col="green")
lines(cv_syd, type="l", col="black")

#Brisbane
poster_tb_bris <- ts(read.csv("Marketing/Forecasting/new_poster_by_city.csv", header = TRUE)
                       [48:285,3], frequency = 52)
plot(poster_tb_bris)
postertbats_bris <- tbats(poster_tb_bris)
print(tbats.components(postertbats_bris))
plot(postertbats_bris)
tbats_poster_bris <- forecast(postertbats_bris, h=26)
plot(tbats_poster_bris, ylab="Poster Growth Brisbane")
accuracy(tbats_poster_bris)

cv_bri <- ts(poster_city[48:300,3], frequency = 52)
plot(tbats_poster_bris, type="l", col="green")
lines(cv_bri, type="l", col="black")

##Export 3 capital cities
fc_table_city_5 <- data.frame(tbats_poster_melb, tbats_poster_ad, tbats_poster_perth, tbats_poster_syd, tbats_poster_bris)
write.csv(fc_table_city_5, "Marketing/Forecasting/Output_TBATS_city_5.csv", row.names=F)