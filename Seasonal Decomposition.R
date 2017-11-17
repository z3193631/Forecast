library(Ecdat)
library(forecast)
library(zoo)

#Loading Data
#data(AirPassengers)
#timeserie_air = AirPassengers
master_city <- read.csv("Marketing/Season/Assigned Task Data - City.csv", header = TRUE)
time.points <- seq.Date(as.Date("2012-02-13"), by = 7, length.out = nrow(master_city))
sydney <- zoo(cbind(master_city["Greater.Sydney"]),time.points)
plot(as.ts(sydney))

trend_sydney = ma(sydney, order = 52, centre = T)
plot(as.ts(trend_sydne))
lines(trend_air)
plot(as.ts(trend_air))


#Detrending the time series
detrend_air = timeserie_air / trend_air
plot(as.ts(detrend_air))

#Average Seasonality
m_air = t(matrix(data = detrend_air, nrow = 12))
seasonal_air = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_air,12)))

#plot the random noise
random_air = timeserie_air / (trend_air * seasonal_air)
plot(as.ts(random_air))

#Original time series reconstruction
recomposed_air = trend_air*seasonal_air*random_air
plot(as.ts(recomposed_air))


#Alternative Method using decompose
sydney_ts = ts(sydney, frequency = 52)
decompose_sydney = decompose(sydney_ts, "multiplicative")

plot(as.ts(decompose_sydney$seasonal))
plot(as.ts(decompose_sydney$trend))
plot(as.ts(decompose_sydney$random))
plot(decompose_sydney)

#####################decomposition using stl#####################

sydney_log <- zoo(cbind(log(master_city["Greater.Sydney"])),time.points) #log dataset to normalise exponential trend
plot(as.ts(sydney_log))
print(sydney_log)
sydney_log <- ts(sydney_log[1:300])
sydney_ts = ts(sydney_log, frequency = 52)
print(sydney_ts)
#Apply stl decomposition
sydney_stl = stl(sydney_ts, "periodic")
seasonal_stl_sydney <- sydney_stl$time.series[,1]
trend_stl_sydney <- sydney_stl$time.series[,2]
random_stl_sydney <- sydney_stl$time.series[,3]

plot(sydney_ts)
plot(as.ts(seasonal_stl_sydney))
plot(trend_stl_sydney)
plot(random_stl_sydney)
plot(sydney_stl)

sydney_table <- data.frame(seasonal_stl_sydney)
write.csv(sydney_table, "Marketing/Season/sydney_season.csv", row.names=F)

#STL for Melbourne
plot(as.ts(master_city["Greater.Melbourne"]))
melbourne_log <- zoo(cbind(log(master_city["Greater.Melbourne"])),time.points) #log dataset to normalise exponential trend
melbourne_log <- ts(melbourne_log[73:281])
melbourne_ts = ts(melbourne_log, frequency = 52)

#Apply stl decomposition
melbourne_stl = stl(melbourne_ts, "periodic")
seasonal_stl_melb <- melbourne_stl$time.series[,1]
trend_stl_melb <- melbourne_stl$time.series[,2]
random_stl_melb <- melbourne_stl$time.series[,3]

plot(melbourne_ts)
plot(as.ts(seasonal_stl_melb))
plot(trend_stl_melb)
plot(random_stl_melb)
plot(melbourne_stl)

Melb_table <- data.frame(seasonal_stl_melb)
write.csv(Melb_table, "Marketing/Season/melbourne_season.csv", row.names=F)

#STL for Perth
plot(as.ts(master_city["Greater.Perth"]))
perth_log <- zoo(cbind(log(master_city["Greater.Perth"])),time.points) #log dataset to normalise exponential trend
perth_log <- ts(perth_log[152:300])
perth_ts = ts(perth_log, frequency = 52)

#Apply stl decomposition
perth_stl = stl(perth_ts, "periodic")
seasonal_stl_perth <- perth_stl$time.series[,1]
trend_stl_perth <- perth_stl$time.series[,2]
random_stl_perth <- perth_stl$time.series[,3]

plot(perth_ts)
plot(as.ts(seasonal_stl_perth))
plot(trend_stl_perth)
plot(random_stl_perth)
plot(perth_stl)

Perth_table <- data.frame(seasonal_stl_perth)
write.csv(Perth_table, "Marketing/Season/perth_season.csv", row.names=F)

#STL for Brisbane
plot(as.ts(master_city["Greater.Brisbane"]))
bri_log <- zoo(cbind(log(master_city["Greater.Brisbane"])),time.points) #log dataset to normalise exponential trend
bri_log <- ts(bri_log[100:286])
bri_ts = ts(bri_log, frequency = 52)

#Apply stl decomposition
bri_stl = stl(bri_ts, "periodic")
seasonal_stl_bri <- bri_stl$time.series[,1]
trend_stl_bri <- bri_stl$time.series[,2]
random_stl_bri <- bri_stl$time.series[,3]

plot(bri_ts)
plot(as.ts(seasonal_stl_bri))
plot(trend_stl_bri)
plot(random_stl_bri)
plot(bri_stl)

bri_table <- data.frame(seasonal_stl_bri)
write.csv(bri_table, "Marketing/Season/brisbane_season.csv", row.names=F)
