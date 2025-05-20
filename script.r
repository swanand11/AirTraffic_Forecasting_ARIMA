library(zoo)
library(ggplot2)
library(dplyr)

# Data Cleaning
df <- data.frame(
  Date = as.yearmon(time(AirPassengers)),
  Passengers = as.numeric(AirPassengers)
)
df$Year <- format(df$Date, "%Y")
df$Month <- format(df$Date, "%B")
df$Month <- factor(df$Month, levels = month.name, ordered = TRUE)
df <- df[, c("Month", "Year", "Passengers")]

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

# Run the function
eda_air_traffic(df)
