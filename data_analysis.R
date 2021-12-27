#### SETUP ####
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)

setwd("~/Documents/uber_data_analysis/data_archive")

# load dataset
uber_jan_june_15 <- read.csv("uber-raw-data-janjune-15.csv")

# clean data and split datetime
uber_jan_june_15$date <- as.Date(uber_jan_june_15$Pickup_date)
uber_jan_june_15$month <- format(uber_jan_june_15$date, "%m")
uber_jan_june_15$day <- weekdays(uber_jan_june_15$date)

# create dataset to produce month vs. total journeys plot
total_pickups_month <- uber_jan_june_15 %>% group_by(month) %>%
  summarise(count = n())

# create a dataset for each month
uber_jan_15 <- uber_jan_june_15 %>% filter(month == '01')
uber_feb_15 <- uber_jan_june_15 %>% filter(month == '02')
uber_march_15 <- uber_jan_june_15 %>% filter(month == '03')
uber_april_15 <- uber_jan_june_15 %>% filter(month == '04')
uber_may_15 <- uber_jan_june_15 %>% filter(month == '05')
uber_june_15 <- uber_jan_june_15 %>% filter(month == '06')

# create datasets for total picups per day in january
total_pickups_jan_day <- uber_jan_15 %>% group_by(day) %>%
  summarise(count = n())
total_pickups_feb_day <- uber_feb_15 %>% group_by(day) %>%
  summarise(count = n())
total_pickups_march_day <- uber_march_15 %>% group_by(day) %>%
  summarise(count = n())
total_pickups_april_day <- uber_april_15 %>% group_by(day) %>%
  summarise(count = n())
total_pickups_may_day <- uber_may_15 %>% group_by(day) %>%
  summarise(count = n())
total_pickups_june_day <- uber_june_15 %>% group_by(day) %>%
  summarise(count = n())

#### PLOTS ####
# barplot to analyze the distribution of pickups per month in NYC over the first 6 months of 2015
ggplot(total_pickups_month, aes(x = month, y = count)) +
  scale_y_continuous(labels = comma, breaks = seq(0, 3000000, by = 500000)) +
  labs(y="total journeys", x = "month") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat = "identity", fill = "#bcd4e6")

# scatter plot for total number of pickups each day of the week, per month
ggplot(NULL, aes(x = factor(day, weekdays(min(uber_jan_june_15$date) + 3:9)), y = count)) +
  labs(x = 'day') + 
  scale_y_continuous(labels = comma, breaks = seq(0, 500000, by = 50000)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(data = total_pickups_jan_day, col = "lightblue") +
  geom_point(data = total_pickups_feb_day, col = "mistyrose") +
  geom_point(data = total_pickups_march_day, col = "darkseagreen") +
  geom_point(data = total_pickups_april_day, col = "lavenderblush3") +
  geom_point(data = total_pickups_may_day, col = "rosybrown") +
  geom_point(data = total_pickups_june_day, col = "peachpuff")
