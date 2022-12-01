library(tidyverse)
library(anomalize)

processed_data <- read.csv("data/processed_data.csv")

# view dataset
head(processed_data, 5)

# data processing 
str(processed_data)

#--------------------Univarite Anomaly Detection in Time Series-----------------

# change chr to Date format for 'DATE' column
df <- processed_data %>%
  mutate(DATE, DATE = as.Date.character(DATE)) %>%
  select(DATE, unemploy_rate_la) %>% 
  as_tibble(df) # convert df to a tibble

class(df)