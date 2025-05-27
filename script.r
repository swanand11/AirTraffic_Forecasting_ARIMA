library(zoo)
library(ggplot2)
library(dplyr)
library(tseries)
library(forecast)

# Data Cleaning
df <- data.frame(
  Date = as.yearmon(time(AirPassengers)),
  Passengers = as.numeric(AirPassengers)
)
df$Year <- format(df$Date, "%Y")
df$Month <- format(df$Date, "%B")
df$Month <- factor(df$Month, levels = month.name, ordered = TRUE)
df <- df[, c("Month", "Year", "Passengers")]

ts_data <- ts(df$Passengers, start = c(1949, 1), frequency = 12)

# EDA Function
eda_air_traffic <- function(data) {
  cat(" 1. Data Structure:\n")
  print(str(data))

  cat("\n 2. Summary Statistics:\n")
  print(summary(data$Passengers))

  cat("\n3. Missing Values:\n")
  print(colSums(is.na(data)))

  cat("\n 4. Yearly Trend Plot:\n")
  yearly_data <- data %>%
    group_by(Year) %>%
    summarise(Total_Passengers = sum(Passengers))

  print(
    ggplot(yearly_data, aes(x = as.numeric(Year), y = Total_Passengers)) +
      geom_line(color = "dodgerblue", linewidth = 1.2) +
      geom_point(color = "darkblue") +
      labs(title = "Yearly Total Air Passengers",
           x = "Year",
           y = "Total Passengers") +
      theme_minimal()
  )

  cat("\n 5. Seasonality Check: Monthly Distribution\n")
  monthly_avg <- data %>%
    group_by(Month) %>%
    summarise(Average_Passengers = mean(Passengers)) %>%
    arrange(match(Month, month.name))

  print(
    ggplot(monthly_avg, aes(x = Month, y = Average_Passengers)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Average Passengers per Month (Seasonality)",
           x = "Month",
           y = "Average Passengers") +
      theme_minimal()
  )

  cat("\n6. Average Passengers by Month:\n")
  print(monthly_avg)
}

eda_air_traffic(df)

# Decomposition
plot(decompose(ts_data))

# Stationarity Check
cat("\nADF Test for Stationarity:\n")
adf_result <- adf.test(ts_data)
print(adf_result)

# ACF and PACF Plots
par(mfrow = c(1, 2))
acf(diff(log(ts_data)), main = "ACF of Differenced Log Data")
pacf(diff(log(ts_data)), main = "PACF of Differenced Log Data")
par(mfrow = c(1, 1))

# Train/Test Split
log_data <- log(ts_data)
train <- window(log_data, end = c(1958, 12))
test <- window(log_data, start = c(1959, 1))

# Fit Seasonal ARIMA Model
fit <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
cat("\nSARIMA Model Summary:\n")
print(summary(fit))
checkresiduals(fit)  # Optional diagnostic


# Forecasting
forecast_values <- forecast(fit, h = 24)
plot(forecast_values, main = "Air Traffic Forecast (1959-1960)")

# Compare Actual vs Predicted
yhat <- exp(forecast_values$mean)
ytrue <- exp(test)
comparison_df <- data.frame(
  Month = time(ytrue),
  Actual = as.numeric(ytrue),
  Predicted = as.numeric(yhat)
)


print(
ggplot(comparison_df, aes(x = Month)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "Actual vs Predicted Air Passengers (1959-1960)",
       x = "Month", y = "Number of Passengers",
       color = "Legend") +
  theme_minimal()
)

# Accuracy
cat("\nModel Accuracy:\n")
print(accuracy(yhat, ytrue))
