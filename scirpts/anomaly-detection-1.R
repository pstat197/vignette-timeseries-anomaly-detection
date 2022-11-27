# import required packages 

library(tidyverse)
library(tibbletime)
library(anomalize)
library(timetk)


# import dataset 
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

# using 'anomalize' package
#The R ‘anomalize’ package enables a workflow for detecting anomalies in data. The main 
#functions are time_decompose(), anomalize(), and time_recompose().

df_anomalized <- df %>%
  time_decompose(unemploy_rate_la, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()

df_anomalized %>% glimpse() 

# Visualize the Anomalies 

df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

# adjusting trend and Seasonality 

#  a frequency and a trend were automatically selected for us

p1 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")
p1

# determine the logical frequency and trend spans based on the scale of the data
get_time_scale_template()


# adjusting local parameters 

p2 <- df %>%
  time_decompose(unemploy_rate_la,
                 frequency = "auto",
                 trend     = "6 months") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Local)")

p2

# Adjusting the Global Parameters 
time_scale_template() %>%
  mutate(trend = ifelse(time_scale == "day", "2 weeks", trend)) %>%
  set_time_scale_template()
get_time_scale_template()

p3 <- df %>%
  time_decompose(unemploy_rate_la) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Global)")
p3


# reset the time scale template defaults back to the original defaults
time_scale_template() %>%
  set_time_scale_template()
# Verify the change
get_time_scale_template()

# Extract the Anomalous Data Points 

df %>% 
  time_decompose(unemploy_rate_la) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')


# Adjusting Alpha and Max Anoms

p4 <- df %>%
  time_decompose(unemploy_rate_la) %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p4

# if we decrease alpha, it increases the bands making it more difficult to be an outlier

p5 <- df %>%
  time_decompose(unemploy_rate_la) %>%
  anomalize(remainder, alpha = 0.6, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p5

# The max_anoms parameter is used to control the maximum percentage of data that can be an anomaly

p6 <- df %>%
  time_decompose(unemploy_rate_la) %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("20% Anomalies")
#> frequency = 7 days
#> trend = 91 days

p7 <- df %>%
  time_decompose(unemploy_rate_la) %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("5% Anomalies")
#> frequency = 7 days
#> trend = 91 days
p6
p7

# Using the "Timetk" package 

## Interactive Anomaly Visualization

df %>% timetk::plot_anomaly_diagnostics(DATE,unemploy_rate_la, .facet_ncol = 2)

# To find the exact data points that are anomalies, we use tk_anomaly_diagnostics() function

df %>% timetk::tk_anomaly_diagnostics(DATE,unemploy_rate_la) %>% filter(anomaly=='Yes')

