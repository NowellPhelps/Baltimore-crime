# Analysis for Advanced Methods in Biostatistics III - 140.753.01
# Nowell Hollier Phelps
# February 2025 

# Initialization
rm(list = ls())
setwd("~/Desktop/Hopkins/Year 1/Methods/Term 3/Data analysis project/Code and data")
library(lubridate)
library(tidyverse)

#### A - LOAD AND CLEAN DATA #############################
data <- read.csv("DataAnalysis_Baltimore_Crime_Data.csv")

# Convert date and time to datetime class
data_subset <- data
data_subset$datetime <- paste(data_subset$CrimeDate, data_subset$CrimeTime)
data_subset$datetime <- as.POSIXct(data_subset$datetime, format = "%m/%d/%Y %H:%M:%S", tz = "EST")
data_subset <- data_subset %>% select(-CrimeDate, - CrimeTime)

# Extract components of datetime for later analysis
data_subset$year <- factor(year(data_subset$datetime))
data_subset$month <- factor(month(data_subset$datetime),
                            levels = seq(1, 12), 
                            labels = month.abb)
data_subset$hour <- hour(data_subset$datetime)
data_subset$day <- day(data_subset$datetime)
data_subset$days_in_month <- lubridate::days_in_month(data_subset$datetime)

# Figure 1a - Crime count by day over time
p <- ggplot(data_subset, aes(x = datetime)) +
  geom_histogram(stat = "bin", binwidth = 60*60*24, fill = "grey50", alpha = 0.8) +
  theme_classic() +
  scale_x_datetime(breaks = as.POSIXct(paste0(seq(2012, 2017), "-01-01 00:00:00")), 
                   labels = seq(2012, 2017),
                   expand = c(0,0)) +
  scale_y_continuous(expand = expansion(0,0))+
  labs(x = "Date", 
       y = "Crime rate (reported crimes per day)",
       title = "Baltimore crime rate by day")

# Figure 1b - Count by month and year
data_plot <- data_subset %>%
  group_by(month, year, days_in_month) %>%
  summarise(count = n()) %>%
  mutate(rate = count/days_in_month) %>%
  filter(!(month == "Sep" & year == 2017))

p <- ggplot(data_plot, aes(x = month, y = rate, colour = year, group = year)) +
  facet_wrap(~year)+
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Month",
       y = "Crime rate (reported crimes per day)",
       title = "Baltimore crime rate by month and year", 
       caption = "September 2017 is excluded from plot due to incomplete data")



# Plots of crime rate by month and day
data_numb <- data_subset %>%
  mutate(month_day_year = paste(month, day, year)) %>%
  select(month_day_year) %>%
  unique() %>%
  mutate(month_day =  sub("^([^ ]+ [^ ]+).*", "\\1", month_day_year)) %>%
  group_by(month_day) %>%
  summarise(numb_days_in_month_day = n()) 

data_plot <- data_subset %>%
  mutate(month_day = paste(month, day)) %>%
  group_by(month, day, month_day) %>%
  summarise(count = n()) %>%
  left_join(data_numb, by = "month_day") %>%
  mutate(rate = count/numb_days_in_month_day) %>%
  ungroup()
  
mean_rate <- data_plot %>%
  group_by(month) %>%
  summarise(mean_rate = mean(rate)) %>%
  ungroup()

data_plot <- data_plot %>%
  left_join(mean_rate, by = "month") %>%
  mutate(rate_centred = rate - mean_rate)


p1 <- ggplot(data_plot, aes(x = day, y = rate)) +
  geom_col() +
  facet_wrap(~month) +
  theme_minimal() +
  labs(x = "Day of month",
     y = "Crime rate (reported crimes per day)",
     title = "Baltimore crime rate by month and day")

p2 <- ggplot(data_plot, aes(x = day, y = rate_centred)) +
  geom_col() +
  facet_wrap(~month) +
  theme_minimal() +
  labs(x = "Day of month",
       y = "Mean-centred crime rate (reported crimes per day)",
       title = "Baltimore crime rate by month and day, centred by monthly average")


# Plots of 

p1 <- ggplot(data_subset, aes(x = hour)) +
  geom_histogram(stat = "bin", binwidth = 1) +
  facet_wrap(~month)


p2 <- ggplot(data_subset, aes(x = as.numeric(year))) +
  geom_histogram(stat = "bin", binwidth = 1) +
  facet_wrap(~District) +
  theme_minimal()

rm(data)





# CLEAN DATA

p <- ggplot(data, aes(x = CrimeTime)) + 
  geom_histogram(binwidth = 3600) 
p



hist(data$CrimeTime@hour)

p <- ggplot(data, aes(x = CrimeDate)) + 
  geom_histogram(binwidth = 30) 
p
