ons_data <- read_csv("C:\Users\Doualeh\Downloads\R ONS Project\ons_data")

# 1. Cleaning Data Column

# Extract Year and Month from the 'date' column
ons_data$year <- as.numeric(substr(ons_data$date, 1, 4))
month.name <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
ons_data$month_num <- match(substr(ons_data$date, 6, 8), month.name)

# 2. Converting Data to Time Series format
ts_data <- ts(ons_data$inflation_rate, start=c(min(ons_data$year), min(ons_data$month_num)), frequency=12)

# 3. Visualizing Time Series
library(ggplot2)
ggplot(data=ons_data, aes(x=date, y=inflation_rate, group=1)) + 
  geom_line() + 
  ggtitle("Historical UK Inflation Rate") +
  theme_minimal()

# 4. Determining ARIMA Model
install.packages("forecast")
library(forecast)
arima_model <- auto.arima(ts_data)

# 5. Forecasting
forecast_result <- forecast(arima_model, h=12)
plot(forecast_result)

# 6. Viewing Forecast Data
forecast_df <- as.data.frame(forecast_result)
View(forecast_df)
