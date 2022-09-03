# Time Series Analysis


## USING FORECAST and ZOO LIBRARY.
library(forecast)
library(zoo)

## CREATION OF DATA FRAME ##

# Setting working directory for locating files.
setwd("/Users/vanshikagupta/Documents/CSU_EastBay/BAN_673/Group_Project/sfo_airPassenger")

# Creating data frame.
project.data <- read.csv("SFO_AirPassengers.csv")

# first and last 6 records of the file
head(project.data)
tail(project.data)

# Creating time series data set in R using the ts() function
passenger.ts <- ts(project.data$Passengers_in000s, 
                   start = c(2006, 1), end = c(2019, 12), freq = 12)
passenger.ts

autoplot(passenger.ts, ylab = "Passengers", 
         main = "SFO Air Passengers Data", col = "blue", lwd = 1)

c.stl <- stl(passenger.ts, s.window = "periodic")
autoplot(c.stl, main = "SFO Air Passengers Time Series Components")

## Use plot() to plot time series data
plot(passenger.ts, 
     xlab = "Time", ylab = "Passengers", ylim = c(1000, 7000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), main = "SFO Air Passengers", lwd = 2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
passenger.stl <- stl(passenger.ts, s.window = "periodic")
autoplot(passenger.stl, main = "SFO Air Passengers Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
Acf(passenger.ts, lag.max = 12, main = "Autocorrelation for SFO Air Passengers Data")

length(passenger.ts)

# Developing data partition with the validation partition of 20 periods and 
# the rest for the training partition
validation_period <- 36
training_period <- length(passenger.ts) - validation_period
training.ts <- window(passenger.ts, start = c(2006, 01), end = c(2006, training_period))
validation.ts <- window(passenger.ts, start = c(2006, training_period + 1), 
                        end = c(2006, training_period + validation_period))

validation.ts

# Use Acf() function to identify autocorrelation for training and validation
# data sets, and plot autocorrelation for different lags (up to maximum of 12)
Acf(training.ts, lag.max = 12, main = "Autocorrelation for Training Data Set")
Acf(validation.ts, lag.max = 12, main = "Autocorrelation for Validation Data Set")


# Plotting the time series data and visualize partitions. 
plot(training.ts, 
     xlab = "Time", ylab = "Passengers", ylim = c(1000, 7000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), main = "SFO Air Passengers", lwd = 2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(validation.ts, col = "black", lty = 1, lwd = 2)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## ----- LINEAR TREND --------
# developing a regression model with linear trend (training data)
training.lin.trend <- tslm(training.ts ~ trend)
summary(training.lin.trend)
# forecasting in the validation period
training.lin.trend.pred <- forecast(training.lin.trend, 
                                    h = validation_period, level = 0)
training.lin.trend.pred

# Plot predictions for linear trend forecast.
plot(training.lin.trend.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Linear Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.lin.trend$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## ----- LINEAR TREND AND SEASONALITY--------
# developing a regression model with linear trend and seasonality (training data)
training.lin.trend.seas <- tslm(training.ts ~ trend + season)
summary(training.lin.trend.seas)
# forecasting in the validation period
training.lin.trend.seas.pred <- forecast(training.lin.trend.seas, 
                                         h = validation_period, level = 0)
training.lin.trend.seas.pred

# Plot predictions for linear trend and seasonality forecast.
plot(training.lin.trend.seas.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Linear Trend and Seasonality Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## ----- QUADRATIC TREND--------
# developing a regression model with quadratic trend (training data)
training.quad.trend <- tslm(training.ts ~ trend + I(trend^2))
summary(training.quad.trend)
# forecasting in the validation period
training.quad.trend.pred <- forecast(training.quad.trend, 
                                     h = validation_period, level = 0)
training.quad.trend.pred

# Plot predictions for quadratic trend forecast.
plot(training.quad.trend.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.quad.trend$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## ----- QUADRATIC TREND AND SEASONALITY--------
# developing a regression model with quadratic trend (training data)
training.quad.trend.seas <- tslm(training.ts ~ trend + I(trend^2) + season)
summary(training.quad.trend.seas)
# forecasting in the validation period
training.quad.trend.seas.pred <- forecast(training.quad.trend.seas, 
                                          h = validation_period, level = 0)
training.quad.trend.seas.pred

# Plot predictions for quadratic trend and seasonality forecast.
plot(training.quad.trend.seas.pred$mean, 
     xlab = "Time", ylab = "passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.quad.trend.seas$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Accuracy of regression model with linear trend (training data)
round(accuracy(training.lin.trend.pred$mean, validation.ts), 3)
# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of regression model with quadratic trend (training data)
round(accuracy(training.quad.trend.pred$mean, validation.ts), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of Naive forecast model (training data)
round(accuracy(training.naive.pred$mean, validation.ts), 3)
# Accuracy of Seasonal Naive forecast model (training data)
round(accuracy(training.snaive.pred$mean, validation.ts), 3)


## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & TRAILING MA OF RESIDUALS ------

# Plot residuals of the predictions with trend and seasonality.
plot(training.lin.trend.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-1000, 1500), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), 
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(validation.ts - training.lin.trend.pred$mean, col = "brown", lwd = 2, lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(-1500, 1500))
lines(c(2019.93, 2019.93), c(-1500, 1500))
text(2011, 1500, "Training")
text(2018.5, 1500, "Validation")
text(2021, 1500, "Future")
arrows(2005, 1300, 2016.8, 1300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 1300, 2019.8, 1300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 1300, 2022, 1300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## ------ TWO-LEVEL MODEL (regression model with linear trend and seasonality & trailing MA of residuals) ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
training.lin.trend.seas.res <- training.lin.trend.seas.pred$residuals
training.lin.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.lin.trend.seas.res <- rollmean(training.lin.trend.seas.res, k = 2, align = "right")
ma.trail.lin.trend.seas.res
# Create residuals forecast for validation period.
ma.trail.lin.trend.seas.res.pred <- forecast(ma.trail.lin.trend.seas.res, h = validation_period, level = 0)
ma.trail.lin.trend.seas.res.pred
# Regression residuals in validation period.
training.lin.trend.seas.res.valid <- validation.ts - training.lin.trend.seas.pred$mean
training.lin.trend.seas.res.valid
# To develop real forecast for validation period, 
# combine regression forecast and trailing MA forecast for residuals.
valid.forecast.2level.linTS.ma <- training.lin.trend.seas.pred$mean + ma.trail.lin.trend.seas.res.pred$mean
valid.forecast.2level.linTS.ma

# Plot the predictions for trailing MA.
plot(training.ts, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Linear Trend and Seasonality & Trailing MA") 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.ma.trailing, col = "brown", lwd = 2)
lines(valid.forecast.2level.linTS.ma, col = "brown", lwd = 2, lty = 2)
lines(validation.ts)
legend(2005,6500, legend = c("Passengers", "Training MA, k=2",
                             "Validation MA, k= 2"), 
       col = c("black", "brown", "brown"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)



## ------ TWO-LEVEL MODEL (regression model with quadratic trend and seasonality & trailing MA of residuals) ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
training.quad.trend.seas.res <- training.quad.trend.seas.pred$residuals
training.quad.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.quad.trend.seas.res <- rollmean(training.quad.trend.seas.res, k = 2, align = "right")
ma.trail.quad.trend.seas.res
# Create residuals forecast for validation period.
ma.trail.quad.trend.seas.res.pred <- forecast(ma.trail.quad.trend.seas.res, h = validation_period, level = 0)
ma.trail.quad.trend.seas.res.pred
# Regression residuals in validation period.
training.quad.trend.seas.res.valid <- validation.ts - training.quad.trend.seas.pred$mean
training.quad.trend.seas.res.valid
# To develop real forecast for validation period, 
# combine regression forecast and trailing MA forecast for residuals.
valid.forecast.2level.quadTS.ma <- training.quad.trend.seas.pred$mean + ma.trail.quad.trend.seas.res.pred$mean
valid.forecast.2level.quadTS.ma

# Plot the predictions for trailing MA.
plot(training.ts, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Quadratic Trend and Seasonality & Trailing MA") 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.ma.trailing, col = "brown", lwd = 2)
lines(valid.forecast.2level.quadTS.ma, col = "brown", lwd = 2, lty = 2)
lines(validation.ts)
legend(2005,6500, legend = c("Passengers", "Training MA, k=2",
                             "Validation MA, k= 2"), 
       col = c("black", "brown", "brown"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of two-level Model
# regression model with linear trend & seasonality and trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.linTS.ma, validation.ts), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of two-level Model
# regression model with quadratic trend & seasonality and trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.quadTS.ma, validation.ts), 3)
# Accuracy of Naive forecast model (training data)



# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(training.lin.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(validation.ts - training.lin.trend.seas.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Validation Residuals")



## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & AUTOREGRESSIVE MODEL OF RESIDUALS ------

# Using Arima() function to fit AR(1) model for training residuals of regression model with linear trend
# The Arima model of order = c(1,0,0) gives an AR(1) model
lin.trend.seas.res.ar1 <- Arima(training.lin.trend.seas$residuals, order = c(1,0,0))
summary(lin.trend.seas.res.ar1)
z.stat <- (0.7889 - 1)/0.0546
p.val <- pnorm(z.stat)
p.val

# The Arima model of order = c(2,0,0) gives an AR(2) model
lin.trend.seas.res.ar2 <- Arima(training.lin.trend.seas$residuals, order = c(2,0,0))
summary(lin.trend.seas.res.ar2)
z.stat <- (0.6619 - 1)/0.0853
p.val <- pnorm(z.stat)
p.val

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- data.frame(training.ts, training.lin.trend$fitted, 
                       training.lin.trend.seas$residuals, lin.trend.seas.res.ar1$fitted, 
                       lin.trend.seas.res.ar1$residuals, lin.trend.seas.res.ar2$fitted, 
                       lin.trend.seas.res.ar2$residuals)
names(train.df) <- c("train.data", "Regression.linearTS", "Regression.Residuals",
                     "AR(1).Model", "AR(1).Model.Residuals",
                     "AR(2).Model", "AR(2).Model.Residuals")
train.df


# Use forecast() function to make prediction of residuals in validation set.
lin.trend.seas.res.ar1.pred <- forecast(lin.trend.seas.res.ar1, h = validation_period, level = 0)
lin.trend.seas.res.ar1.pred

lin.trend.seas.res.ar2.pred <- forecast(lin.trend.seas.res.ar2, h = validation_period, level = 0)
lin.trend.seas.res.ar2.pred

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.2level.linTS.ar1.pred <- training.lin.trend.seas.pred$mean + lin.trend.seas.res.ar1.pred$mean
valid.2level.linTS.ar2.pred <- training.lin.trend.seas.pred$mean + lin.trend.seas.res.ar2.pred$mean

valid.df <- data.frame(validation.ts, training.lin.trend.seas.pred$mean,
                       valid.2level.linTS.ar2.pred)
names(valid.df) <- c("Passenger.Valid", "Reg.LinTS.Forecast", "Combined.Forecast.AR(2)")
valid.df




## ------- QUADRATIC TREND AND SEASONALITY & AUTOREGRESSIVE MODEL OF RESIDUALS ------

# The Arima model of order = c(1,0,0) gives an AR(1) model
quad.trend.seas.res.ar1 <- Arima(training.quad.trend.seas$residuals, order = c(1,0,0))
summary(quad.trend.seas.res.ar1)
z.stat <- (0.6700 - 1)/0.0639
p.val <- pnorm(z.stat)
p.val

# The Arima model of order = c(2,0,0) gives an AR(2) model
quad.trend.seas.res.ar2 <- Arima(training.quad.trend.seas$residuals, order = c(2,0,0))
summary(quad.trend.seas.res.ar2)
z.stat <- (0.5901 - 1)/0.0859
p.val <- pnorm(z.stat)
p.val

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
quad_train.df <- data.frame(training.ts, training.quad.trend$fitted, 
                            training.quad.trend.seas$residuals, quad.trend.seas.res.ar1$fitted, 
                            quad.trend.seas.res.ar1$residuals)
names(quad_train.df) <- c("train.data", "Regression.quadTS", "Regression.Residuals",
                          "AR(1).Model", "AR(1).Model.Residuals")
quad_train.df


# Use forecast() function to make prediction of residuals in validation set.
quad.trend.seas.res.ar1.pred <- forecast(quad.trend.seas.res.ar1, h = validation_period, level = 0)
quad.trend.seas.res.ar1.pred

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
quad_valid.two.level.pred <- training.quad.trend.seas.pred$mean + quad.trend.seas.res.ar1.pred$mean

quad_valid.df <- data.frame(validation.ts, training.quad.trend.seas.pred$mean, 
                            quad_valid.two.level.pred)
names(quad_valid.df) <- c("Passenger.Valid", "Reg.QuadTS.Forecast", "Combined.Forecast.(AR1)")
quad_valid.df



##--------- NAIVE FORECAST--------
# Use naive() to make naive forecast (training.naive.pred) 
# for validation data.
training.naive.pred <- naive(training.ts, h = validation_period)

# Plot predictions for naive forecast.
plot(training.naive.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Naive Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.naive.pred$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)



##--------- SEASONAL NAIVE FORECAST--------
# Use snaive() to make naive forecast (training.snaive.pred) 
# for validation data.
training.snaive.pred <- snaive(training.ts, h = validation_period)

# Plot predictions for seasonal naive forecast.
plot(training.snaive.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Seasonal Naive Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.snaive.pred$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of two-level regression model with linear trend & seasonality and 
# trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.linTS.ma, validation.ts), 3)
# Accuracy of regression model with linear trend (training data) and 
# AR(2) model for residuals (training data)
round(accuracy(valid.2level.linTS.ar2.pred, validation.ts), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of regression model with quadratic trend (training data) and 
# AR(1) model for residuals (training data)
round(accuracy(quad_valid.two.level.pred, validation.ts), 3)
# Accuracy of Seasonal Naive forecast model (training data)
round(accuracy(training.snaive.pred$mean, validation.ts), 3)



Acf(training.lin.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(lin.trend.seas.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")
Acf(lin.trend.seas.res.ar2$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")

Acf(training.quad.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(quad.trend.seas.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")



#### ENTIRE DATASET ####

## ---- LINEAR TREND AND SEASONALITY -------
# developing a regression model with linear trend ans seasonality (training data)
passenger.lin.trend.seas <- tslm(passenger.ts ~ trend + season)
summary(passenger.lin.trend.seas)
# forecasting in the validation period
passenger.lin.trend.seas.pred <- forecast(passenger.lin.trend.seas, 
                                          h = 24, level = 0)
passenger.lin.trend.seas.pred

# Use plot() function to create plot with linear trend and seasonality 
plot(passenger.ts, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(1000, 7000), xlim = c(2005, 2025), 
     main = "Linear Trend and Seasonality Regression Model - SFO Air Passengers")
lines(passenger.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(passenger.lin.trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)


## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & TRAILING MA OF RESIDUALS ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
passenger.lin.trend.seas.res <- passenger.lin.trend.seas.pred$residuals
passenger.lin.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ap.ma.trail.lin.trend.seas.res <- rollmean(passenger.lin.trend.seas.res, k = 2, align = "right")
ap.ma.trail.lin.trend.seas.res
# Create residuals forecast for future periods
ap.ma.trail.lin.trend.seas.res.pred <- forecast(ap.ma.trail.lin.trend.seas.res, h = 24, level = 0)
ap.ma.trail.lin.trend.seas.res.pred 
# combine regression forecast and trailing MA forecast for residuals.
ap.forecast.2level.linTS.ma <- passenger.lin.trend.seas.pred$mean + ap.ma.trail.lin.trend.seas.res.pred$mean
ap.forecast.2level.linTS.ma


# Use plot() function to create plot 
plot(passenger.ts, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(1000, 7000), xlim = c(2005, 2025), 
     main = "Linear Trend and Seasonality Regression Model + Trailing MA (width = 2)")
lines(passenger.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(ap.forecast.2level.linTS.ma, col = "blue", lwd = 2, lty = 2)


## ---- TWO-LEVEL MODEL (Linear T&S and AR(2) of Residuals) -------
# The Arima model of order = c(2,0,0) gives an AR(2) model
passenger.lin.trend.seas.res.ar2 <- Arima(passenger.lin.trend.seas.pred$residuals, order = c(2,0,0))
summary(passenger.lin.trend.seas.res.ar2)
z.stat <- (0.6601 - 1)/0.0770
p.val <- pnorm(z.stat)
p.val
# Use forecast() function to make prediction of residuals
passenger.lin.trend.seas.res.ar2.pred <- forecast(passenger.lin.trend.seas.res.ar2, h = 24, level = 0)
passenger.lin.trend.seas.res.ar2.pred
# two level model results
passenger.two.level.linTS.ar2.pred <- passenger.lin.trend.seas.pred$mean + passenger.lin.trend.seas.res.ar2.pred$mean
passenger.two.level.linTS.ar2.pred


# Use plot() function to create plot 
plot(passenger.ts, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(1000, 7000), xlim = c(2005, 2025), 
     main = "Linear Trend and Seasonality Regression Model + Autoregressive (2)")
lines(passenger.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(passenger.two.level.linTS.ar2.pred, col = "blue", lwd = 2, lty = 2)



# Accuracy of regression model with linear trend and seasonality(entire data)
round(accuracy(passenger.lin.trend.seas.pred$fitted, passenger.ts), 3)
# Accuracy of regression model with linear trend and seasonality (entire data) and 
# trailing MA for residuals (entire data)
round(accuracy(passenger.lin.trend.seas.pred$fitted + 
                   ap.ma.trail.lin.trend.seas.res.pred$fitted, passenger.ts), 3)
# Accuracy of regression model with linear trend and seasonality (entire data) and 
# AR(2) model for residuals (entire data)
round(accuracy(passenger.lin.trend.seas.pred$fitted + 
                   passenger.lin.trend.seas.res.ar2.pred$fitted, passenger.ts), 3)
# Accuracy of seasonal naive forecast (baseline model)
round(accuracy((snaive(passenger.ts))$fitted, passenger.ts), 3)






## MOVING AVERAGE MODEL - trailing MA ##

## --- TRAINING DATA ----
# Create trailing MA using training data and window size k=2.
train.ma.trailing <- rollmean(training.ts, k = 2, align = "right")

# Obtain the last MA in the trailing period (last.ma) and
# create forecast for the validation data (ma.trailing.pred).
last.ma <- tail(train.ma.trailing, 1)
train.ma.trailing.pred <- ts(rep(last.ma, validation_period), start = c(2006, training_period + 1),
                             end = c(2006, training_period + validation_period), freq = 12)

# Plot the predictions for trailing MA.
plot(training.ts, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Trainling MA, k=2 with Partition ") 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.ma.trailing, col = "brown", lwd = 2)
lines(train.ma.trailing.pred, col = "brown", lwd = 2, lty = 2)
lines(validation.ts)
legend(2004,6500, legend = c("Passengers", "Training MA, k=2",
                             "Validation MA, k= 2"), 
       col = c("black", "brown", "brown"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#----- ENTIRE DATASET ------

# Create trailing moving average with window (number of periods) k = 2, 4, and 6.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
passenger.ma.trailing_2 <- rollmean(passenger.ts, k = 2, align = "right")
passenger.ma.trailing_4 <- rollmean(passenger.ts, k = 4, align = "right")
passenger.ma.trailing_6 <- rollmean(passenger.ts, k = 6, align = "right")

passenger.ma.trail_2 <- c(rep(NA, length(passenger.ts) - 
                                length(passenger.ma.trailing_2)), passenger.ma.trailing_2)
passenger.ma.trail_4 <- c(rep(NA, length(passenger.ts) - 
                                length(passenger.ma.trailing_4)), passenger.ma.trailing_4)
passenger.ma.trail_6 <- c(rep(NA, length(passenger.ts) - 
                                length(passenger.ma.trailing_6)), passenger.ma.trailing_6)

passenger.ma_trailing_tab <- cbind(passenger.ts, passenger.ma.trail_2, passenger.ma.trail_4, passenger.ma.trail_6)
passenger.ma_trailing_tab

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(passenger.ma.trail_2, passenger.ts), 3)
round(accuracy(passenger.ma.trail_4, passenger.ts), 3)
round(accuracy(passenger.ma.trail_6, passenger.ts), 3)

## Create trailing MA forecast for 24 periods into the future.
passenger.ma.trailing_2.pred <- forecast(passenger.ma.trailing_2, h=24, level = 0)
passenger.ma.trailing_2.pred

# Accuracy of Trailing MA Model (entire dataset)
round(accuracy(passenger.ma.trailing_2.pred$fitted, passenger.ts), 3)



# Plot original data and trailing MA.
plot(passenger.ts, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Trailing Moving Average", lwd = 2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(passenger.ma.trailing_2, col = "blue", lwd = 2, lty = 1)
lines(passenger.ma.trailing_2.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2004, 6500, legend = c("passengers", "Trailing MA, k=2"), 
       col = c("black", "blue"), 
       lty = c(1, 1), lwd =c(1, 2), bty = "n")



## TEST PREDICTABILITY

# Use Arima() function to fit AR(1) model for passenger
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
passenger.data.ar1<- Arima(passenger.ts, order = c(1,0,0))
summary(passenger.data.ar1)
pnorm((0.9122 - 1)/0.0318)
# Hypothesis Testing: Z- Test
# Null hypothesis Ho: β1 = 1
# Alternative hypothesis H1: β1  1
# z-statistic = (β1 - 1)/(s.e.) = (0.9122 - 1)/0.0318 = -2.761006
# p-value for z-statistic = 0.002881178
# Based on the p-value of 0.0029 (< 0.05), we can reject the null hypothesis that β1 = 1.
# Therefore, the time series data for SFO Air Passengers, passenger.ts, is predictable (not random walk).


# The ARIMA model of order = c(2,0,0) gives an AR(2) model.
passenger.data.ar2<- Arima(passenger.ts, order = c(2,0,0))
summary(passenger.data.ar2)

# Create first difference of ClosePrice data using diff() function.
diff.passenger.data <- diff(passenger.ts, lag = 1)
diff.passenger.data

Acf(diff.passenger.data, lag.max = 12, 
    main = "Autocorrelation for First Differencing - SFO Air Passengers")

