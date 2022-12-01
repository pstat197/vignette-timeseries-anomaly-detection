## Loading data and libraries 

library(tidyverse)
library(tidymodels)
library(lubridate) #dealing with dates, and time series aspect 
library(Rbeast) #used for change-point detection
library(changepoint) #used for change-point detection
library(changepoint.np) #used for change-point nonparemetric
processed_data <- read_csv('data/processed_data.csv')
processed_data$DATE <- zoo::as.Date(processed_data$DATE, tz = NULL, format = NULL) # change date from characters to date element
data <- data.frame(processed_data$DATE,processed_data$unemploy_rate_la) %>%
  rename("unemploy_rate_la" = "processed_data.unemploy_rate_la", "date" = "processed_data.DATE")
data.ts <- ts(data$unemploy_rate_la, frequency = 10, start = c(1990,1), end = c(2021,2))
ts.plot(data.ts)
plot(processed_data$unemploy_rate_la)


## Using Rbeast
y <- data$unemploy_rate_la
out=beast(y, season='none')
plot(out)
print(out)

# How do we know if a change-point found is significant or not?
# We calculate cost of the whole data with no change
# If the difference is large enough then we say there is no change
# default change-point metric in changepoint package to test if there is a change point or 
# not is MBIC - a Modified Bayesian Information Criterion 

# cpt.mean - mean only changes
# cpt.var - variance only changes
# cpt.meanvar - mean and variance changes


## Using changepoint 
m1 <- c(data$unemploy_rate_la)
m1.amoc = cpt.mean(m1, penalty = "MBIC")
cpts(m1.amoc) # checks for at most one change-point value
plot(m1.amoc)

# we can see that we definitely need way more change points that just one
LA.default = cpt.mean(data$unemploy_rate_la)
cpts(LA.default)
param.est(LA.default)

# it can be seen that the variance might not be equal to 1, so we must appropriately scale it  
LA.scale = cpt.mean(as.vector(scale(data$unemploy_rate_la)))
cpts(LA.scale)

# since we still get the same changepoint after scaling the variance, that shows that this is actually a changepoint
# and not just because we forced it to show a change point
m2 <- c(data$unemploy_rate_la)
m2.man = cpt.var(m2,method = "PELT")
cpts(m2.man) 
param.est(m2.man)
plot(m2.man)


## using mean
m3 <- c(data$unemploy_rate_la)
m3.man = cpt.mean(m3,method = "PELT")
cpts(m3.man) 
cpts(m3.man)
param.est(m3.man)
plot(m3.man)

## finding changepoint with respect to variance and mean
mv1 <- c(data$unemploy_rate_la)
mv1.pelt <- cpt.meanvar(mv1, method = "PELT")
length(cpts(mv1.pelt))
param.est(mv1.pelt)
plot(mv1.pelt)
