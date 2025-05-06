# Install required packages if not already installed
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")

# Load the libraries into the R session
library(forecast)
library(tseries)
library(ggplot2)

# Load the AirPassengers dataset
data("AirPassengers")

# View the structure and summary of the data
str(AirPassengers)
summary(AirPassengers)

# Plot the original time series data
plot(AirPassengers, main = "Air Passengers Time Series", xlab = "Year", ylab = "Number of Passengers", col = "blue")

# Decompose the time series using classical decomposition
decomposed_ts <- decompose(AirPassengers)

# Plot the decomposition components
plot(decomposed_ts)

# Perform the Augmented Dickey-Fuller (ADF) test for stationarity
adf_test <- adf.test(AirPassengers)
print(adf_test)

# Differencing the series if necessary
differenced_ts <- diff(AirPassengers)

# Plot the differenced time series
plot(differenced_ts, main = "Differenced Air Passengers Time Series", xlab = "Year", ylab = "Differenced Number of Passengers")

# Fit the ARIMA model to the original time series data
arima_model <- auto.arima(AirPassengers)

# Print the ARIMA model summary
summary(arima_model)

# Plot the diagnostics of the ARIMA model
tsdiag(arima_model)

# Forecast the next 24 months
forecasted_values <- forecast(arima_model, h = 24)

# Plot the forecast
plot(forecasted_values, main = "ARIMA Forecast for Air Passengers")

# Split the data into training and testing sets
train_data <- window(AirPassengers, end = c(1958, 12))
test_data <- window(AirPassengers, start = c(1959, 1))

# Fit the ARIMA model to the training data
arima_model_train <- auto.arima(train_data)

# Forecast the next 12 months
forecast_test <- forecast(arima_model_train, h = 12)

# Plot the forecast and actual test data
plot(forecast_test)
lines(test_data, col = "red")

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((forecast_test$mean - test_data)^2))
cat("RMSE: ", rmse, "\n")
