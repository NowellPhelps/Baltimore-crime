################################################################################
# Analysis for Advanced Methods in Biostatistics III - 140.753.01.             #
# Nowell Hollier Phelps                                                        #
# April 2025.                                                                  #
################################################################################

################################################################################
# INITIALISATION AND DATA CLEANING                                             #
################################################################################

################################################################################
# Load libraries and set directory
rm(list = ls())
setwd("~/Desktop/Hopkins/Year 1/Methods/Term 4/Baltimore-crime")
library(lubridate)
library(tidyverse)
library(viridis)
library(grid)
library(gridExtra)

outdir_folder <- "~/Desktop/Hopkins/Year 1/Methods/Term 4/Baltimore-crime/Figure/"
dir.create(outdir_folder, showWarnings = F)

################################################################################
# Data load and extract useful variables

# Load data
data <- read.csv("DataAnalysis_Baltimore_Crime_Data.csv")

# Convert date and time to datetime class
data_subset <- data
data_subset$datetime <- paste(data_subset$CrimeDate, data_subset$CrimeTime)
data_subset$datetime <- as.POSIXct(data_subset$datetime, format = "%m/%d/%Y %H:%M:%S", tz = "EST")
data_subset <- data_subset %>% select(-CrimeDate, - CrimeTime)

# Extract individual components of datetime for later analysis
data_subset$year <- factor(year(data_subset$datetime))
data_subset$month <- factor(month(data_subset$datetime),
                            levels = seq(1, 12), 
                            labels = month.abb)
data_subset$hour <- hour(data_subset$datetime)
data_subset$day <- day(data_subset$datetime)
data_subset$days_in_month <- lubridate::days_in_month(data_subset$datetime)
data_subset$weekday <- wday(data_subset$datetime, label = T)

################################################################################
# Data cleaning and potential duplicates removal
rm(data)

data_subset$id <- 1:nrow(data_subset)

# Remove first set of potential duplicates
duplicates <- data_subset %>% filter(Location == "1000 W PATAPSCO AVE",
                                   year == 2016, month == "Jun", day == 5)

data_subset <- data_subset %>% filter(!(id %in% duplicates$id[2:129]))

# Remove second identified set of potential duplicates
duplicates <- data_subset %>% filter(Neighborhood == "Mondawmin",
                                     year == 2015, month == "Apr", day == 27, hour == 15, Location == "2400 LIBERTY HEIGHTS AVE")

data_subset <- data_subset %>% filter(!(id %in% duplicates$id[2:36]))

# Remove third identified set of potential duplicates
duplicates <- data_subset %>% filter(Neighborhood == "Orchard Ridge",
                                      year == 2013, month == "Dec", day == 25, hour == 2)

data_subset <- data_subset %>% filter(!(id %in% duplicates$id[2:55]))

################################################################################
# Calculate rates by day and month

# Get hour floors to later match up
data_subset$year_month_day_hour <- floor_date(data_subset$datetime, unit = "hours", week_start = getOption("lubridate.week.start", 7))

# Get counts by hour and day
count_hour <- data_subset %>%
  group_by(year, month, day, hour) %>%
  summarise(count = n()) %>%
  ungroup()

count_day <- data_subset %>%
  group_by(year, month, day) %>%
  summarise(count = n()) %>%
  ungroup()

# Get rate by hour - start with skeleton dataset, merge to count dataset, and set NA to 0 then calculate rate
rate_hour <- data.frame(datetime = seq(min(data_subset$year_month_day_hour), max(data_subset$year_month_day_hour), by = 'hour')) %>%
  mutate(year = factor(year(datetime)),
         month = factor(month(datetime),
                        levels = seq(1, 12), 
                        labels = month.abb),
         day = day(datetime),
         weekday  = wday(datetime),
         hour = hour(datetime),
         days_in_month = lubridate::days_in_month(datetime)) %>%
  left_join(count_hour)

# Convert NA to 0 (no crimes recorded in hour)
rate_hour$count[which(is.na(rate_hour$count))] <- 0
# 
# rate_hour <- rate_hour %>%
#   mutate(rate = count) %>%
#   filter(!(year == 2015 & month == "Apr" & day == 27))

# resent "days in month" in april 2015 to 29, to account for removal of April 27 2015
#rate_hour$days_in_month[which(rate_hour$month == "Apr" & rate_hour$year == 2015)] <- 29

# Get rate by day - start with skeleton dataset, merge to count dataset, and set NA to 0 then calculate rate
rate_day <- data.frame(datetime = seq(min(data_subset$year_month_day_hour), max(data_subset$year_month_day_hour), by = 'day')) %>%
  mutate(year = factor(year(datetime)),
         month = factor(month(datetime),
                        levels = seq(1, 12), 
                        labels = month.abb),
         day = day(datetime),
         weekday  = wday(datetime),
         days_in_month = days_in_month(datetime)) %>%
  left_join(count_day)

# Convert NA to 0 (no crimes recorded in hour)
rate_day$count[which(is.na(rate_hour$day))] <- 0

# rate_day <- rate_day %>%
#   mutate(rate = count) %>%
#   filter(!(year == 2015 & month == "Apr" & day == 27))

# resent "days in month" in april 2015 to 29, to account for removal of April 27 2015
# rate_day$days_in_month[which(rate_day$month == "Apr" & rate_day$year == 2015)] <- 29
         

################################################################################
# EXPLORATORY DATA ANALYSIS                                                    #
################################################################################

################################################################################
fig1a <- ggplot(data_subset, aes(x = datetime)) +
  geom_histogram(stat = "bin", binwidth = 60*60*24, fill = "grey50", alpha = 0.8) +
  theme_classic() +
  scale_x_datetime(breaks = as.POSIXct(paste0(seq(2012, 2017), "-01-01 00:00:00")), 
                   labels = seq(2012, 2017),
                   expand = c(0,0)) +
  scale_y_continuous(expand = expansion(0,0))+
  labs(x = "Date", 
       y = "Crime rate (crimes per day)",
       title = "A") +
  theme(axis.text = element_text(size = 12), 
        axis.title =element_text(size = 14), 
        title = element_text(size = 16))

################################################################################
# Figure 2 - Average crime rate by year and month
data_plot <- rate_day %>%
  group_by(month, year, days_in_month) %>%
  summarise(tot_count = sum(count), 
            min = min(count), 
            max = max(count)) %>%
  mutate(rate = tot_count/days_in_month) %>%
  filter(!(month == "Sep" & year == 2017))

fig1b <- ggplot(data_plot, aes(x = month, y = year, fill =rate)) +
  geom_tile() +
  scale_fill_viridis(breaks = c(100, 120, 140, 160), limits = c(90, 160),
                     name = "Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Month",
       y = "Crime rate (crimes per day)",
       title = "B",
       caption = "") +
  theme(axis.text = element_text(size = 12), 
        axis.title =element_text(size = 14), 
        legend.title = element_text(size = 13),
        title = element_text(size = 16)) 


fig1c <- ggplot(data_plot, aes(x = month, y = rate, colour = year, group = year)) +
  geom_line() +
  scale_colour_discrete(name = "Year") +
  scale_fill_discrete(name = "Year") +
  geom_ribbon(aes(ymin = min, ymax = max, fill = year), alpha = 0.1, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Month",
       y = "Crime rate (crimes per day)",
       title = "C", 
       caption = "") +
  theme(axis.text = element_text(size = 12), 
        axis.title =element_text(size = 14), 
        legend.title = element_text(size = 13),
        title = element_text(size = 16)) 



# PLOT FIGURE 1
figNum <- 1
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 8.5, width = 8.5, onefile=T)

grid.arrange(
  arrangeGrob(
    blank,
    arrangeGrob(blank,
                fig1a,
                blank,
                fig1b,
                blank,
                nrow = 1, widths = c(.5, 10, .2, 10, .5)),
    blank, 
    arrangeGrob(blank, fig1c, blank, nrow = 1, widths = c(.5, 20., .5)), 
    blank,
    ncol = 1,
    heights = c(1, 10, 1, 10, 1)
  ))

dev.off()

################################################################################
# Figure 3 - Crime rate by month and day of month

# Plots of crime rate by month and day of month
data_numb <- rate_day %>%
  mutate(month_day_year = paste(month, day, year)) %>%
  select(month_day_year) %>%
  unique() %>%
  mutate(month_day =  sub("^([^ ]+ [^ ]+).*", "\\1", month_day_year)) %>%
  group_by(month_day) %>%
  summarise(numb_days_in_month_day = n()) 

data_plot <- rate_day %>%
  mutate(month_day = paste(month, day)) %>%
  group_by(month, day, month_day) %>%
  summarise(tot_count = sum(count)) %>%
  left_join(data_numb, by = "month_day") %>%
  mutate(rate = tot_count/numb_days_in_month_day) %>%
  ungroup()

mean_rate <- data_plot %>%
  group_by(month) %>%
  summarise(mean_rate = mean(rate)) %>%
  ungroup()

data_plot <- data_plot %>%
  left_join(mean_rate, by = "month") %>%
  mutate(rate_centred = rate - mean_rate) %>%
  mutate(rate_type = factor(ifelse(rate_centred <0, "<0", ">=0")))

pa2 <- ggplot(data_plot, aes(x = day, y = rate)) +
  geom_col() + 
  facet_wrap(~month) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Day of month",
       y = "Crime rate (crimes per day)")

p3b <- ggplot(data_plot, aes(x = day, y = rate_centred, fill = rate_type)) +
  geom_col() +
  facet_wrap(~month) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  scale_fill_manual(values = c("<0" = "darkgreen", ">=0" = "red"), 
                    labels = c("<0" = "Below monthly average",">=0" = "Above monthly average"))+
  labs(x = "Day of month",
       y = "Mean-centred average crime rate (crimes per day)",
       #title = "Baltimore crime rate by month and day of week, centred by monthly average",
       fill = "")


# Plots of crime rate by month and day of week
data_numb <- rate_day %>%
  mutate(month_weekday_year = paste(month, weekday, year)) %>%
  select(month_weekday_year) %>%
  unique() %>%
  mutate(month_weekday =  sub("^([^ ]+ [^ ]+).*", "\\1", month_weekday_year)) %>%
  group_by(month_weekday) %>%
  summarise(numb_days_in_month_weekday = n()) 

data_plot <- rate_day  %>%
  mutate(month_weekday = paste(month, weekday)) %>%
  group_by(month, weekday, month_weekday) %>%
  summarise(tot_count = sum(count)) %>%
  left_join(data_numb, by = "month_weekday") %>%
  mutate(rate = tot_count/numb_days_in_month_weekday) %>%
  ungroup()

mean_rate <- data_plot %>%
  group_by(month) %>%
  summarise(mean_rate = mean(rate)) %>%
  ungroup()

data_plot <- data_plot %>%
  left_join(mean_rate, by = "month") %>%
  mutate(rate_centred = rate - mean_rate) %>%
  mutate(rate_type = factor(ifelse(rate_centred <0, "<0", ">=0")))

pa2 <- ggplot(data_plot, aes(x = weekday, y = rate)) +
  geom_col() + 
  facet_wrap(~month) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Day of month",
       y = "Crime rate (crimes per day)")

p3b <- ggplot(data_plot, aes(x = weekday, y = rate_centred, fill = rate_type)) +
  geom_col() +
  facet_wrap(~month) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  scale_fill_manual(values = c("<0" = "darkgreen", ">=0" = "red"), 
                    labels = c("<0" = "Below monthly average",">=0" = "Above monthly average"))+
  labs(x = "Day of month",
       y = "Mean-centred average crime rate (crimes per day)",
       #title = "Baltimore crime rate by month and day of week, centred by monthly average",
       fill = "")


################################################################################
# Figure  - Crime rate by hour and month over entire period

data_mean_rate <- rate_hour %>%
  group_by(hour, month) %>%
  summarise(tot_days = sum(days_in_month)/24, 
            tot_count = sum(count)) %>%
  mutate(mean_rate = tot_count/tot_days)

data_plot <- left_join(data_mean_rate, rate_hour, by = c("month", "hour"))

p4 <- ggplot(data_plot, aes(x = factor(hour), y = rate, fill = mean_rate)) +
  facet_wrap(~month) + 
  geom_violin() +
  scale_fill_gradientn(colors = rev(heat.colors(100)))+
  scale_x_discrete(breaks = c(0, 6, 12, 18),
                   labels = c("12am", "6pm", "12pm", "6pm")) +
  theme_minimal() +
  labs(x = "Hour of day", 
       y = "Hourly crime rate", 
       fill = "Mean hourly crime rate") +
  
  theme(axis.text = element_text(size = 12), 
        axis.title =element_text(size = 14))












################################################################################
# STATISTICAL ANALYSIS                                                         #
################################################################################

################################################################################
# Model 1
M1 <- lm(rate ~ as.factor(hour) + month, data = rate_hour)
summary(M1)
extractAIC(M1)

# Model 2
M2 <- lm(rate ~ as.factor(hour) + month + month:as.factor(day), data = rate_hour)
summary(M2)
extractAIC(M2)


# cross-validation
data_train <- rate_hour[which(rate_hour$datetime <= as.POSIXct("2016-09-02")),]
data_test  <- rate_hour[which(rate_hour$datetime > as.POSIXct("2016-09-02")),]
  

M0_train <- lm(rate ~ as.factor(hour) + month, data = data_test)
M0_test <- predict(M0_train,data_test)

help <- data.frame(y = data_test$rate, 
                   ypred = M0_test) %>%
  mutate(diff = (y - ypred)^2)

sum(help$diff)/nrow(help)
# 8.837995


M1_train <- lm(rate ~ as.factor(hour) +  month + month:as.factor(day), data = data_test)
M1_test <- predict(M1_train,data_test)

help <- data.frame(y = data_test$rate, 
                   ypred = M1_test) %>%
  mutate(diff = (y - ypred)^2)

sum(help$diff)/nrow(help)
# 8.295335



RMSE(M0_test,data_test$rate)


# Plot fits for M1

coef <- coef(M1)
coef[["as.factor(hour)0"]] <- 0
coef[["monthJan"]] <- 0
months <- month.abb
hour <- seq(0, 23)

coeffs <- data.frame(month = rep(month.abb, each = 24),
                    hour = rep(seq(0, 23), 12))

coef_vec <- c()
month_vec <- c()
hour_vec <- c()
for (month in months){
  for (hour in seq(0,23)){
    month_vec <- c(month_vec, month)
    hour_vec <- c(hour_vec, hour)
    coef_vec <-c(coef_vec, coef[["(Intercept)"]] + coef[[paste0("as.factor(hour)", hour)]] +coef[[paste0("month",month)]])
  }
}

df <- data.frame(month = month_vec,
                 hour = hour_vec,
                 estimate = coef_vec)

p <- ggplot(df, aes(x = hour, y = estimate)) +
  facet_wrap(~factor(month, levels = month.abb)) +
  geom_line() +
  theme_bw()+
  scale_x_continuous(breaks = c(0, 6, 12, 18),
                 labels = c("12am", "6pm", "12pm", "6pm")) +
  labs(x = "Hour of day", 
       y = "Estimated hourly crime rate") 



