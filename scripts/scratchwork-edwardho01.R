## Loading data and libraries 

library(tidyverse)
library(tidymodels)
library(lubridate) #dealing with dates, and time series aspect 
library(Rbeast) #used for change-point detection
library(changepoint) #used for change-point detection
processed_data <- read_csv('data/processed_data.csv')
processed_data$DATE <- as_date(processed_data$DATE, tz = NULL, format = NULL)
y <- processed_data$unemploy_rate_la
out=beast(y, season='none')
plot(out)
print(out)


