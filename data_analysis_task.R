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
library(wesanderson)
library(DataExplorer)
library(lme4)
library(MASS)
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
data_subset <- data_subset %>% dplyr::select(-CrimeDate, - CrimeTime)

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
data_subset$crime_violent <- ifelse(data_subset$Description %in% violent_crime_types, "violent", "not_violent")

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
         weekend_ind = ifelse(weekday %in% c(1, 2), 1, 0),
         freddie_gray_ind = ifelse(day == 27 & month == "Apr" & year == 2015, 1, 0), 
         freddie_gray_five_days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 5, 1, 0),
         freddie_gray_twenty_days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 20, 1, 0),
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
         weekend_ind = ifelse(weekday %in% c(1, 2), 1, 0),
         freddie_gray_ind = ifelse(day == 27 & month == "Apr" & year == 2015, 1, 0), 
         freddie_gray_five_days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 5, 1, 0),
         freddie_gray__days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 20, 1, 0),
         days_in_month = days_in_month(datetime)) %>%
  left_join(count_day)

# Convert NA to 0 (no crimes recorded in hour)
rate_day$count[which(is.na(rate_hour$day))] <- 0

# Do same for stratified counts
rate_hour_stratified <- data.frame(datetime = rep(seq(min(data_subset$year_month_day_hour), max(data_subset$year_month_day_hour), by = 'hour'), 2),
                                   crime_violent = rep(c("not_violent", "violent"), each = length(seq(min(data_subset$year_month_day_hour), max(data_subset$year_month_day_hour), by = 'hour')))) %>%
  mutate(year = factor(year(datetime)),
         month = factor(month(datetime),
                        levels = seq(1, 12), 
                        labels = month.abb),
         day = day(datetime),
         weekday  = wday(datetime),
         weekend_ind = ifelse(weekday %in% c(1, 2), 1, 0),
         freddie_gray_ind = ifelse(day == 27 & month == "Apr" & year == 2015 & crime_violent == "not_violent", 1, 0), 
         freddie_gray_five_days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 5 & crime_violent == "not_violent", 1, 0),
         freddie_gray_twenty_days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 20 & crime_violent == "not_violent", 1, 0),
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
         
         freddie_gray_ind = ifelse(day == 27 & month == "Apr" & year == 2015, 1, 0), 
         freddie_gray_five_days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 5, 1, 0),
         freddie_gray_twenty_days = ifelse(abs(difftime(datetime, as.POSIXct("2015-04-27", format="%Y-%m-%d"), units = "days")) <= 20, 1, 0),
         weekend_ind = ifelse(weekday %in% c(1, 2), 1, 0),
         days_in_month = days_in_month(datetime)) %>%
  left_join(count_day_stratified)

# Convert NA to 0 (no crimes recorded in hour)
rate_day_stratified$count[which(is.na(rate_day_stratified$day))] <- 0

################################################################################
# EXPLORATORY DATA ANALYSIS                                                    #
################################################################################
pal_heatmap <- wes_palette("Zissou1", 100, type = "continuous")

col_scale_crime_type <- scale_colour_manual(values = c("not_violent" = wes_palette("Darjeeling1")[2], 
                                                       "violent" = wes_palette("Darjeeling1")[1]),
                                            labels = c("not_violent" = "Non-violent", 
                                                       "violent" = "Violent"), 
                                            name = "Crime type")
fill_scale_crime_type <- scale_fill_manual(values = c("not_violent" = wes_palette("Darjeeling1")[2], 
                                                       "violent" = wes_palette("Darjeeling1")[1]),
                                            labels = c("not_violent" = "Non-violent", 
                                                       "violent" = "Violent"), 
                                            name = "Crime type")


################################################################################
# Appendix Figure 1 - Crime rate by year and month
plot_missing(data_subset) + theme_classic()


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
  scale_fill_gradientn(colours = pal_heatmap, 
                       breaks = c(100, 120, 140, 160), 
                       limits = c(90, 160),
                       name = "Mean rate\n(crimes per day)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Month",
       y = "Year",
       title = "B",
       caption = "") +
  theme(axis.text = element_text(size = 12), 
        axis.title =element_text(size = 14), 
        legend.title = element_text(size = 10),
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

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 10, width = 12, onefile=T)

grid.arrange(
  arrangeGrob(
    blank,
    arrangeGrob(blank,
                fig1a,
                blank,
                fig1b,
                blank,
                nrow = 1, widths = c(.5, 10, .2, 12, .5)),
    blank, 
    arrangeGrob(blank, fig1c, blank, nrow = 1, widths = c(2, 20, 2)), 
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
       y = "Mean crime rate (crimes per hour)") 

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
       y = "Mean crime rate (crimes per hour)")
  
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
data_plot <- rate_day_stratified %>% filter(month %in% c("Mar", "Apr", "May")) %>%
  mutate(year_ind = ifelse(year == 2015, 1, 0))
data_plot$year_ind = factor(data_plot$year_ind)

fig5a <- ggplot(data_plot %>% filter(crime_violent == "violent"), aes(x = day, y = count, colour = year_ind, group = year))+
  facet_wrap(~month) +
  scale_colour_manual(values = c(wes_palette("FrenchDispatch")[3], wes_palette("FrenchDispatch")[2]),
                      labels = c("Other year", "2015"),
                      name = "")+
  labs(y = "Violent crime rate (crimes per day)", x = "Day of month")+
  geom_line(alpha = 0.7) +
  theme_classic()

fig5b <- ggplot(data_plot %>% filter(crime_violent == "not_violent"), aes(x = day, y = count, colour = year_ind, group = year))+
  facet_wrap(~month) +
  scale_colour_manual(values = c(wes_palette("FrenchDispatch")[3], wes_palette("FrenchDispatch")[2]),
                      labels = c("Other year", "2015"),
                      name = "")+
  labs(y = "Non-violent crime rate (crimes per day)", x = "Day of month")+
  geom_line(alpha = 0.7) +
  theme_classic()

figNum <- 5
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 10, width = 8.5, onefile=T)

grid.arrange(
  arrangeGrob(
    blank,
    fig5a,
    blank,
    heights = c(1, 10, 1)
  ),
  arrangeGrob(
    blank,
    fig5b,
    blank,
    heights = c(1, 10, 1)
  ),
  ncol = 1, 
  heights = c(1, 1))

dev.off()


################################################################################
# Figure 6 - Trends in violent and non-violent crime by month
data_plot <- rate_day_stratified %>%
  group_by(month, year, crime_violent) %>%
  summarise(mean = mean(count), 
            max = max(count),
            min = min(count)) %>%
  ungroup() %>%
  filter(!((month == "Sep") & (year == 2017))) %>%
  mutate(datetime = ymd(paste(year, month, "01", sep = "-")))

p6a <- ggplot(data_plot, aes(x = datetime, y = mean, colour = crime_violent, group = crime_violent)) + 
  geom_line() +
  geom_ribbon(aes(ymin = min, ymax = max, fill = crime_violent), alpha = 0.1, linetype = "dashed") +
  col_scale_crime_type +
  fill_scale_crime_type +
  theme_classic() +
  labs(y = "Crime rate (crimes per day)",
       x = "Month")

data_plot <- data_plot %>% select(month, year, crime_violent, mean) %>%
  pivot_wider(names_from = crime_violent, values_from = mean)
colnames(data_plot)[3:4] <- c("violent", "not_violent")
data_plot <- data_plot %>%
  mutate(diff = not_violent - violent)%>%
  mutate(datetime = ymd(paste(year, month, "01", sep = "-")))

p6b <- ggplot(data_plot, aes(x = datetime, y = diff, group = "1")) + 
  geom_line() +
  geom_hline(yintercept = mean(data_plot$diff), linetype = "dashed", colour = "grey50") +
  theme_classic() +
  labs(x = "Month", 
       y = "Difference between mean non-violent crime rate\n and mean violent crime rate")

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
# Figure 7 - Trends in violent crime by hour
data_plot <- rate_hour_stratified %>%
  group_by(hour, crime_violent) %>%
  summarise(rate = mean(count)) %>%
  ungroup()

p7a <- ggplot(data_plot, aes(x = hour, y = rate, colour = crime_violent)) + 
  geom_line() +
  col_scale_crime_type +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                     labels = c("12am", "3am", "6am","9am", "12pm", "3pm", "6pm","9pm")) +
  theme_classic() +
  labs(y = "Mean crime rate (crimes per hour)",
       x = "Hour of day")

legend <- cowplot::get_legend(p6a)
p7a <- p7a + theme(legend.position = "none")


p7b <- ggplot(data = rate_hour_stratified, aes(y = count, x = factor(hour), group = paste0(hour, crime_violent), fill = crime_violent)) +
  scale_fill_discrete(name = "Crime type")+
  scale_x_discrete(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                     labels = c("12am", "3am", "6am","9am", "12pm", "3pm", "6pm","9pm")) +
  geom_boxplot(outlier.alpha = 0.9, outlier.colour ="grey90") +
  theme_classic() +
  labs(y = "Crime rate (crimes per hour)",
       x = "Hour of day")

data_numb <- rate_hour_stratified %>%
  mutate(hour_weekday = paste(hour, weekday)) %>%
  select(hour_weekday, crime_violent) %>%
  group_by(hour_weekday, crime_violent) %>%
  summarise(numb_hours_in_hour_weekday = n()) 

data_plot <- rate_hour_stratified %>%
  mutate(hour_weekday = paste(hour, weekday)) %>%
  group_by(hour, weekday, hour_weekday, crime_violent) %>%
  summarise(tot_count = sum(count)) %>%
  left_join(data_numb, by = c("hour_weekday", "crime_violent")) %>%
  mutate(rate = tot_count/numb_hours_in_hour_weekday) %>%
  ungroup() 

data_plot$weekday <- factor(data_plot$weekday,
                            levels = c(2,3,4,5,6,7,1),
                            labels = as.character(wday(c(2:7, 1), label = TRUE)))

p7c <- ggplot(data_plot, aes(x = hour, y = rate, group = crime_violent, colour = crime_violent)) +
  geom_line()+
  col_scale_crime_type +
  facet_wrap(~weekday) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                   labels = c("12am", "", "6am","", "12pm", "", "6pm","")) +
  labs(y = "Mean crime rate (crimes per hour)",
       x = "Hour of day") +
  theme(legend.position = "none")

figNum <- 7
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 6.375, width = 15, onefile=T)

grid.arrange(
  arrangeGrob(blank,
              arrangeGrob(
                arrangeGrob(textGrob("A",hjust=0,just = c("left"),x = unit(0.2, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol = 1, heights = c(1, 20)),
                p7a, 
                legend,
                arrangeGrob(textGrob("B",hjust=0,just = c("left"),x = unit(0.2, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol = 1, heights = c(1, 20)),
                p7c, 
                blank, 
                nrow =1, 
                widths = c(1, 10, 1, 1, 10, 1)),
              blank,
              ncol = 1,
              heights = c(1, 10, 1))
)

dev.off()

################################################################################
# STATISTICAL ANALYSIS                                                         #
################################################################################

###########################################
data_modelling <-
  rate_hour_stratified %>% 
  mutate(crime_violent = ifelse(crime_violent == "violent", 1, 0)) %>%
  mutate(summer_month_ind = ifelse(month %in% c("Jun", "Jul", "Aug"), 1, 0))

data_modelling$hour <- factor(data_modelling$hour)
################################################################################

# Model 1
M1 <- glm(count ~ month + weekend_ind + hour + summer_month_ind:hour + weekend_ind:hour + crime_violent + crime_violent:month + crime_violent:hour + crime_violent:weekend_ind + crime_violent:summer_month_ind:hour + crime_violent:weekend_ind:hour + freddie_gray_ind, family = "quasipoisson", data = data_modelling)
model_summary <- summary(M1)


# ALternaive model 1
M2 <- glm(count ~ month + weekend_ind + hour + summer_month_ind:hour + weekend_ind:hour + crime_violent + crime_violent:month + crime_violent:hour + crime_violent:weekend_ind + crime_violent:summer_month_ind:hour + freddie_gray_ind, family = "quasipoisson", data = data_modelling)
model_summary2 <- summary(M2)

# ALternaive model 2
M3 <- glm(count ~ month + weekend_ind + hour + summer_month_ind:hour + weekend_ind:hour + crime_violent + crime_violent:month + crime_violent:hour + crime_violent:weekend_ind + crime_violent:weekend_ind:hour + freddie_gray_ind, family = "quasipoisson", data = data_modelling)
model_summary1 <- summary(M3)

### PLOT OF MODEL 1 COEFFICIENTS
coefficients <- model_summary$coefficients
estimates <- coefficients[, 1]
se <- coefficients[, 2]

# Combine the information into a data frame for easier plotting
coeff_df <- data.frame(
  Term = rownames(coefficients),
  Estimate = estimates,
  SE = se) %>%
  mutate(CI_upper = Estimate + 1.96*se,
         CI_lower = Estimate - 1.96*SE)

clean_terms <- coeff_df$Term %>%
  gsub(pattern = "hour", replacement = "Hour ") %>%      
  gsub(pattern = "weekend_ind", replacement = "Weekend") %>%      
  gsub(pattern = "month", replacement = "Month ") %>%               
  gsub(pattern = ":", replacement = " - ") %>%                      
  gsub(pattern = "_", replacement = " ") %>%                        
  gsub(pattern = "\\(Intercept\\)", replacement = "Intercept") %>%   
  gsub(pattern = "crime_violent", replacement = "Violent Crime") %>% 
  gsub(pattern = "crime violent", replacement = "Violent Crime") %>%  
  gsub(pattern = "as.Hour", replacement = "Hour")%>%   
  gsub(pattern = "freddie gray ind", replacement = "Freddie Gray Event")%>%   
  gsub(pattern = "summer Month  ind", replacement = "Summer Month")

coeff_df$Term <- clean_terms
coeff_df <- coeff_df[order(coeff_df$Term), ]

order <- c("Intercept",
           "Freddie Gray Event",
           paste0("Month ", month.abb[2:12]),
           paste0("Hour ", seq(1:23)), 
           paste0("Weekend"),
           paste0("Weekend - Hour ", seq(1:23)), 
           paste0("Hour ", seq(1:23), " - Summer Month"), 
           "Violent Crime",
           paste0("Month ", month.abb[2:12], " - Violent Crime"),
           paste0("Hour ", seq(1:23), " - Violent Crime"),
           "Weekend - Violent Crime",
           paste0("Weekend - Hour ", seq(1:23), " - Violent Crime"),
           paste0("Hour ", seq(1:23), " - Summer Month - Violent Crime")
)

coeff_df_reordered <- data.frame(Term = order) %>%
  left_join(coeff_df, by = "Term") 
coeff_df_reordered$y <- seq(1:nrow(coeff_df_reordered))
coeff_df_reordered$Term <- factor(coeff_df_reordered$Term, levels = rev(coeff_df_reordered$Term))

p1 <- ggplot(coeff_df_reordered %>% filter(y <=82), aes(x = Estimate, y = Term, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = 0.1) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() + 
  xlab("Coefficient Estimate") +
  ylab("Terms") +
  theme(axis.text.y = element_text(size = 8))

p2 <- ggplot(coeff_df_reordered %>% filter(y >=84, y < nrow(coeff_df_reordered)), aes(x = Estimate, y = Term, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = 0.1) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() + 
  xlab("Coefficient Estimate") +
  ylab("Terms") +
  theme(axis.text.y = element_text(size = 8))

figNum <- 8
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 10, width = 15, onefile=T)

grid.arrange(
  blank,
  arrangeGrob(blank,p1,blank, p2, blank, nrow = 1, widths = c(1, 10, 1, 10, 1)),
  blank,
  ncol = 1, heights = c(1, 20, 1)
)

dev.off()

### Model 1 predictions plot
crime_violent <- c(0, 1)
month <- c("Jan", "Feb", "Mar",
           "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
weekend_ind <- c(0, 1)
hour <- factor(0:23)

predict_df <- expand.grid(crime_violent = crime_violent,
                                       month = month,
                                       weekend_ind = weekend_ind,
                                       hour = hour) %>%
  mutate(freddie_gray_ind = 0)%>% 
  mutate(summer_month_ind = ifelse(month %in% c("Jun", "Jul", "Aug"), 1, 0))

predict_df$estimate <- exp(predict(M1, predict_df))
predict_df$weekend_ind <- ifelse(predict_df$weekend_ind == 1, "Weekend", "Weekday")
predict_df$crime_violent<- ifelse(predict_df$crime_violent == 1, "violent", "not_violent")

model_predictions <- ggplot(data = predict_df, aes(x = hour, y = estimate, colour = crime_violent, group = paste(crime_violent, weekend_ind), linetype = weekend_ind)) +
  geom_line() +
  col_scale_crime_type +
  scale_linetype_manual(values = c("Weekend" = "dotdash", "Weekday" = "solid"), name = "") +
  facet_wrap(~month) +
  theme_minimal() +
  scale_x_discrete(breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                     labels = c("12am", "", "6am","", "12pm", "", "6pm","")) +
  labs(y = "Estimated crime rate (crimes per hour)", 
       x = "Hour")

figNum <- 9
appendix <- F
figsuffix <- ""
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 8.6, width = 12, onefile=T)


grid.arrange(
  blank,
  arrangeGrob(blank,model_predictions, blank, nrow = 1, widths = c(1, 10, 1)),
  blank,
  ncol = 1, heights = c(1, 20, 1)
)

dev.off()


### DIAGNOSTIC PLOTS FOR MODEL 1
predicted_vals <- predict(M1, rate_hour_stratified%>% filter(!(freddie_gray_ind== 1)))
pearson_residuals <- M1$residuals

df <- data.frame(predicted_vals = predicted_vals,
                 pearson_residuals = pearson_residuals)

p <- ggplot(df, aes(x = predicted_vals, y = pearson_residuals)) +
  geom_point() +
  geom_smooth()+ 
  theme_classic()
  
p <- glm.diag.plots(M1)

figNum <- 2
appendix <- T
figsuffix <- " diagnostic plots model 1"
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 8.6, width = 8.6, onefile=T)
grid.arrange(
  blank,
  arrangeGrob(blank, glm.diag.plots(M1), blank, nrow = 1, widths = c(1, 10, 1)),
  blank,
  ncol = 1, heights = c(1, 20, 1)
)

dev.off()


### Diagnostic plots for Model 1 continued
M1 <- glm(count ~ month + weekend_ind + factor(hour) + weekend_ind:as.factor(hour) + crime_violent + crime_violent:month + crime_violent:factor(hour) + crime_violent:weekend_ind + crime_violent:weekend_ind:factor(hour) + freddie_gray_ind, family = "quasipoisson", data = rate_hour_stratified %>% filter(!(freddie_gray_ind== 1)))
p2 <- glm.diag.plots(M1)
figNum <- 3
appendix <- T
figsuffix <- " diagnostic plots model 1 with freddie gray points removed"
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 8.6, width = 8.6, onefile=T)
grid.arrange(
  blank,
  arrangeGrob(blank, glm.diag.plots(M1), blank, nrow = 1, widths = c(1, 10, 1)),
  blank,
  ncol = 1, heights = c(1, 20, 1)
)

dev.off()




