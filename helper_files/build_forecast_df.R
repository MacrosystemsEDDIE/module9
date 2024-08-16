# Build forecast df
# Author: Mary Lofton
# Date: 17JUL24

# Purpose: build a dataset of forecasted turnover probabilities for Mod 9 Act C

# load packages
library(tidyverse)
library(lubridate)

# create dataframe
turnover_date = "2023-10-22"
first_forecast_dates <- seq.Date(from = as.Date("2023-10-06"), to = as.Date("2023-10-06") + 16, by = 'days')
second_forecast_dates <- seq.Date(from = as.Date("2023-10-13"), to = as.Date("2023-10-13") + 16, by = 'days')

first_forecast_probs <- runif(n = 17, min = 0.03, max = 0.1)
second_forecast_probs <- c(0.02, 0.02, 0.05, 0.13, 0.21, 0.24, 0.27, 0.31, 0.41, 0.52, 0.55, 0.67, 0.69, 0.78, 0.82, 0.83, 0.84)

forecast_df <- data.frame(datetime = c(first_forecast_dates, second_forecast_dates),
                          prob_turnover = c(first_forecast_probs, second_forecast_probs),
                          fc_id = c(rep("first_forecast", times = length(first_forecast_dates)),rep("second_forecast", times = length(second_forecast_dates))))
write.csv(forecast_df, "./data/forecast_scenario_data.csv", row.names = FALSE)
