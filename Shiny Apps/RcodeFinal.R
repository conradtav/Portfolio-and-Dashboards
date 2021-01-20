library(readxl)
library(jsonlite)
library(ggplot2)
library(lubridate)

library(dplyr)
BuoyData <- data.frame(read_excel("BuoyData_2_2.xlsx", sheet = "2016-2018 data"
                            , col_types = c("date", "numeric", "numeric", "numeric"
                                            ,"numeric", "numeric", "numeric", "numeric"
                                            , "numeric", "numeric", "numeric", "numeric")))


depth_ms <- aggregate(. ~ DEPTH_m, data = BuoyData, FUN = function(x) c(mn = mean(x), range = range(x) ) )


## Dates Aggregation
dates_ms <- flatten(aggregate(. ~ DATE_TIME, data = BuoyData, FUN = function(x) c(mn = mean(x), range = range(x), std = sd(x))))

dates_ms <- data.frame(date = dates_ms$DATE_TIME
           , temp = dates_ms$T_DEGC
           , ph = dates_ms$pH
           , cloudiness = dates_ms$Tn_Ntu
           , oxygen = dates_ms$Dox_mg_L
           , AvgWind = dates_ms$AWND_mph
           , DirWind = dates_ms$WDF5_deg
           , MaxWind = dates_ms$WSF5_mph)


# Most basic line plots
p <- ggplot(dates_ms, aes(x= date , y=smooth(AvgWind.mn)) +
  geom_line() + 
  xlab("")
p

y17 <- dates_ms[which(year(dates_ms$date) == 2017 & month(dates_ms$date) > 6), ]

p <- ggplot(y17, aes(x= date , y=oxygen.mn)) +
  geom_line() + 
  xlab("")
p

p <- ggplot(dates_ms, aes(x= date , y=oxygen.mn)) +
  geom_line() + 
  xlab("")
p

## Compare Variables ##
coeff <- 10
ggplot(dates_ms, aes(x=date)) +
  geom_line( aes(y=oxygen.mn), size=1, color="cadetblue") + 
  geom_line( aes(y=temp.mn), size=1, color= "red") +
  scale_y_continuous( sec.axis = sec_axis(~.*coeff, name="temperature")
  )

ggplot(dates_ms, aes(x=date)) +
  geom_line( aes(y=oxygen.mn), size=2, color="cadetblue") + 
  geom_line( aes(y=AvgWind.mn), size=2, color= "red") +
  scale_y_continuous( sec.axis = sec_axis(~.*coeff, name="wind")
  )


# A really basic boxplot.
ggplot(dates_ms, aes(x=as.factor(year(dates_ms$date)), y=oxygen.mn)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("year")

# A really basic boxplot.
ggplot(depth_ms, aes(x=as.factor(depth_ms$DEPTH_m), y=depth_ms$)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Depth")
###################### Forecasting #######################################
library(prophet)

## Enter Your Variable ##

stats <- data.frame(dates_ms$date, dates_ms$oxygen.mn)
colnames(stats) <- c("ds", "y")
head(stats)

m <- prophet(stats)
future <- make_future_dataframe(m, periods = (2*365))
#forecast <- predict(m, future)
#plot(m, forecast)

prophet_plot_components(m, forecast)


el_nino <- tibble(
 holiday = 'el_nino',
 ds = as.Date(c('2017-09-17', '2017-09-16', '2017-09-15', '2017-09-14', '2017-09-13')),
 lower_window = 0,
 upper_window = 20
)

m <- prophet(stats, holidays = el_nino)
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)


future2 <- future %>% 
  filter(as.numeric(format(ds, "%m")) > 3) %>%
  filter(as.numeric(format(ds, "%m")) != 12 ) 
fcst <- predict(m, future2)
plot(m, fcst)
