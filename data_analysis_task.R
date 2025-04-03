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
library(boot)

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

# Classify crime as violent or non-violent
violent_crime_types <- c("AGG. ASSAULT", "ASSAULT BY THREAT", "COMMON ASSAULT", "HOMICIDE", "RAPE", "SHOOTING")
data_subset$crime_violent <- ifelse(data_subset$Description %in% violent_crime_types, "violent", "not violent")

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

count_hour_stratified <- data_subset %>%
  group_by(year, month, day, hour, crime_violent) %>%
  summarise(count = n()) %>%
  ungroup()

count_day_stratified  <- data_subset %>%
  group_by(year, month, day, crime_violent) %>%
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

# Do same for stratified counts
rate_hour_stratified <- data.frame(datetime = rep(seq(min(data_subset$year_month_day_hour), max(data_subset$year_month_day_hour), by = 'hour'), 2),
                                   crime_violent = rep(c("not violent", "violent"), each = length(seq(min(data_subset$year_month_day_hour), max(data_subset$year_month_day_hour), by = 'hour')))) %>%
  mutate(year = factor(year(datetime)),
         month = factor(month(datetime),
                        levels = seq(1, 12), 
                        labels = month.abb),
         day = day(datetime),
         weekday  = wday(datetime),
         hour = hour(datetime),
         days_in_month = lubridate::days_in_month(datetime)) %>%
  left_join(count_hour_stratified)

rate_hour_stratified$count[which(is.na(rate_hour_stratified$count))] <- 0

rate_day_stratified <- data.frame(datetime = seq(min(data_subset$year_month_day_hour), max(data_subset$year_month_day_hour), by = 'day')) %>%
  mutate(year = factor(year(datetime)),
         month = factor(month(datetime),
                        levels = seq(1, 12), 
                        labels = month.abb),
         day = day(datetime),
         weekday  = wday(datetime),
         days_in_month = days_in_month(datetime)) %>%
  left_join(count_day_stratified)

# Convert NA to 0 (no crimes recorded in hour)
rate_day_stratified$count[which(is.na(rate_day_stratified$day))] <- 0

################################################################################
# EXPLORATORY DATA ANALYSIS                                                    #
################################################################################

################################################################################
# Figure 1 - Crime rate by year and month
fig1a <- ggplot(data_subset, aes(x = datetime)) +
  geom_histogram(stat = "bin", binwidth = 60*60*24, alpha = 0.8) +
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
# Figure 2  - Crime rate by hour and month over entire period
data_mean_rate <- rate_hour %>%
  group_by(hour, month) %>%
  summarise(tot_days = sum(days_in_month)/24, 
            tot_count = sum(count)) %>%
  mutate(mean_rate = tot_count/tot_days)

data_plot <- left_join(data_mean_rate, rate_hour, by = c("month", "hour"))


p2 <- ggplot(data_plot, aes(x = factor(hour), y = count, fill = mean_rate)) +
  facet_wrap(~month) + 
  geom_violin() +
  scale_fill_gradientn(colors = rev(heat.colors(100)))+
  scale_x_discrete(breaks = c(0, 6, 12, 18),
                   labels = c("12am", "6pm", "12pm", "6pm")) +
  theme_minimal() +
  labs(x = "Hour of day", 
       y = "Crime rate (crimes per hour)", 
       fill = "Mean hourly\ncrime rate") +
  theme(axis.text = element_text(size = 12), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title =element_text(size = 14))

figNum <- 2
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 10, width = 12, onefile=T)
grid.arrange(
  arrangeGrob(blank,
              arrangeGrob(
                blank, 
                p2,
                blank, 
                nrow =1, 
                widths = c(1, 10, 1)),
              blank,
              ncol = 1,
              heights = c(1, 10, 1))
)
dev.off()



################################################################################
# Figure 3 - Hourly crime rate stratified by month and day of week
data_mean_rate <- rate_hour %>%
  group_by(hour, month) %>%
  summarise(tot_days = sum(days_in_month)/24, 
            tot_count = sum(count)) %>%
  mutate(mean_rate = tot_count/tot_days)

data_plot <- left_join(data_mean_rate, rate_hour, by = c("month", "hour")) %>% 
  select(hour, month, mean_rate) %>% 
  unique()

p3a <- ggplot(data_plot, aes(x = factor(hour), y = mean_rate, colour = month, group = month)) +
  geom_line() +
  scale_colour_discrete(name = "Month") +
  scale_x_discrete(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                   labels = c("12am", "3am", "6am","9am", "12pm", "3pm", "6pm","9pm")) +
  theme_classic() +
  labs(x = "Hour of day", 
       y = "Crime rate (crimes per hour)") 

data_numb <- rate_hour %>%
  mutate(hour_weekday = paste(hour, weekday)) %>%
  select(hour_weekday) %>%
  group_by(hour_weekday) %>%
  summarise(numb_hours_in_hour_weekday = n()) 

data_plot <- rate_hour %>%
  mutate(hour_weekday = paste(hour, weekday)) %>%
  group_by(hour, weekday, hour_weekday) %>%
  summarise(tot_count = sum(count)) %>%
  left_join(data_numb, by = "hour_weekday") %>%
  mutate(rate = tot_count/numb_hours_in_hour_weekday) %>%
  ungroup()

data_plot$weekday <- factor(data_plot$weekday,
                            levels = c(2,3,4,5,6,7,1),
                            labels = as.character(wday(c(2:7, 1), label = TRUE)))

p3b <- ggplot(data_plot, aes(x = hour, colour = weekday, y = rate, group = weekday)) +
  geom_line() +
  theme_classic() +
  scale_colour_discrete(name ="Day of week") +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                   labels = c("12am", "3am", "6am","9am", "12pm", "3pm", "6pm","9pm")) +
  labs(x = "Hour of day",
       y = "Crime rate (crimes per hour)")
  
figNum <- 3
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 6.375, width = 15, onefile=T)

grid.arrange(
  arrangeGrob(blank,
              arrangeGrob(
                arrangeGrob(textGrob("A",hjust=0,just = c("left"),x = unit(0.2, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol = 1, heights = c(1, 20)),
                p3a, 
                arrangeGrob(textGrob("B",hjust=0,just = c("left"),x = unit(0.2, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol = 1, heights = c(1, 20)),
                p3b, 
                blank, 
                nrow =1, 
                widths = c(1, 10, 1, 10, 1)),
              blank,
              ncol = 1,
              heights = c(1, 10, 1))
)

dev.off()



################################################################################
# Figure 4 - Crime rate by month and day of month

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

p4 <- ggplot(data_plot, aes(x = day, y = rate_centred)) +
  geom_hline(yintercept = 0) +
  geom_point(shape = 21, aes(fill = rate_type)) +
  geom_smooth() +
  facet_wrap(~month) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1), 
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("<0" = "darkgreen", ">=0" = "red"), 
                    labels = c("<0" = "Below monthly average",">=0" = "Above monthly average"))+
  labs(x = "Day of month",
       y = "Mean-centred average crime rate (crimes per day)",
       #title = "Baltimore crime rate by month and day of week, centred by monthly average",
       fill = "")

figNum <- 4
appendix <- F
figsuffix <- ""
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 10, width = 13, onefile=T)
grid.arrange(
  arrangeGrob(blank,
              arrangeGrob(
                blank, 
                p4,
                blank, 
                nrow =1, 
                widths = c(1, 10, 1)),
              blank,
              ncol = 1,
              heights = c(1, 10, 1))
)
dev.off()


################################################################################
# Figure 5 - Impact of Freddie Gray event
data_plot <- rate_day %>% filter(month %in% c("Mar", "Apr", "May")) %>%
  mutate(year_ind = ifelse(year == 2015, 1, 0))
data_plot$year_ind = factor(data_plot$year_ind)

fig5 <- ggplot(data_plot, aes(x = day, y = count, colour = year_ind, group = year))+
  facet_wrap(~month) +
  scale_colour_manual(values = c("grey50", "blue"),
                      labels = c("Other year", "2015"),
                      name = "")+
  labs(y = "Crime rate (crimes per day)", x = "Day of month")+
  geom_line(alpha = 0.7) +
  theme_classic()

figNum <- 5
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 5, width = 8.5, onefile=T)

grid.arrange(
  arrangeGrob(
    blank,
    fig2,
    blank,
    heights = c(1, 10, 1)
  ))

dev.off()


################################################################################
# Figure 6 - Trends in violent crime by hour
data_plot_line <- rate_hour_stratified %>%
  mutate(crime_violent = ifelse(crime_violent == "not violent", "not_violent", crime_violent)) %>%
  pivot_wider(values_from = "count", names_from = "crime_violent") %>%
  mutate(prop_violent = violent/(violent+ not_violent)) %>%
  group_by(hour) %>%
  summarise(mean_prop_violent = mean(prop_violent, na.rm = T), 
            mean_violent = mean(violent), 
            mean_not_violent = mean(not_violent)) %>%
  ungroup()

data_plot_points <- rate_hour_stratified %>%
  mutate(crime_violent = ifelse(crime_violent == "not violent", "not_violent", crime_violent)) %>%
  pivot_wider(values_from = "count", names_from = "crime_violent") %>%
  mutate(prop_violent = violent/(violent+ not_violent))
  
p <- ggplot(data_plot_points, aes(x = factor(hour), y = prop_violent*100)) + 
  geom_violin(alpha = 0.8, colour = "grey90", ) +
  geom_line(data = data_plot_line, inherit.aes = F, aes(y = mean_prop_violent*100, x = hour + 1), colour = "red")+
  theme_classic() +
  scale_x_discrete(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                 labels = c("12am", "3am", "6am","9am", "12pm", "3pm", "6pm","9pm")) +
  labs(y = "Proportion of crimes which are violent (%)",
       x = "Hour of day")


p6a <- ggplot(data_plot, aes(x = hour, y = rate, colour = type)) + 
  geom_line() +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                     labels = c("12am", "3am", "6am","9am", "12pm", "3pm", "6pm","9pm")) +
  theme_classic() +
  labs(y = "Mean crime rate (crimes per hour)",
       x = "Hour of day")


p6b <- ggplot(data = rate_hour_stratified, aes(y = count, x = factor(hour), group = paste0(hour, crime_violent), fill = crime_violent)) +
  scale_fill_discrete(name = "Crime type")+
  scale_x_discrete(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                     labels = c("12am", "3am", "6am","9am", "12pm", "3pm", "6pm","9pm")) +
  geom_boxplot(outlier.alpha = 0.9, outlier.colour ="grey90") +
  theme_classic() +
  labs(y = "Crime rate (crimes per hour)",
       x = "Hour of day")

data_plot <- data_plot_line %>% 
  select(-mean_prop_violent) %>%
  pivot_longer(cols = c(mean_violent, mean_not_violent), names_to = "type", values_to = "rate")
  

figNum <- 6
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 6.375, width = 15, onefile=T)

grid.arrange(
  arrangeGrob(blank,
              arrangeGrob(
                arrangeGrob(textGrob("A",hjust=0,just = c("left"),x = unit(0.2, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol = 1, heights = c(1, 20)),
                p6a, 
                arrangeGrob(textGrob("B",hjust=0,just = c("left"),x = unit(0.2, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol = 1, heights = c(1, 20)),
                p6b, 
                blank, 
                nrow =1, 
                widths = c(1, 10, 1, 10, 1)),
              blank,
              ncol = 1,
              heights = c(1, 10, 1))
)

dev.off()




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




