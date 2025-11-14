
library(ncdf4)
library(glmtools)
library(rlang)
library(dplyr)
library(ggplot2)
library(nml)
library(httpgd)
library(ggpubr)
library(languageserver)
library(lintr)
library(padr)
library(lubridate)
library(Metrics)
library(tidyr)
library(gridExtra)
library(reshape2)
library(patchwork)
library(wesanderson)
library(RColorBrewer)
library(streamgraph)
library(ggstream)
library(grid)
library(pracma)
library(paletteer)
library(plotly)
library(htmlwidgets)
library(zoo)

setwd('../FCR-GLM-methane/CH4_UA/model_files')

# 1) Calculating prior uncertainty range for parameters

# 2) Linear interpolation of data

#OXY
obs_oxy <- read.csv('field_data/CleanedObsOxy.csv') %>%
 mutate(DateTime= as.Date(DateTime, format = "%Y-%m-%d"))%>%
 filter(DateTime > "2017-02-26", DateTime < "2020-01-01", Depth != 9.2)
depths <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)

match_noise_oxy <- data.frame()
#match <- data.frame()

for(i in 1:length(depths)){
  
  
    match <- obs_oxy %>%
    dplyr::filter(Depth == depths[i])
    known_dates <- as.Date(match$DateTime, format="%Y-%m-%d")
    known_values <- match$OXY_oxy
      
    # Generate sequence of dates between the sampling dates
    interpolation_dates <- seq(from = min(known_dates), to = max(known_dates), by = "day")
      
    # Perform linear interpolation for each date
    interpolated_values <- approx(known_dates, known_values, xout = interpolation_dates)$y
      
    # Combine the dates and interpolated values into a data frame
    interpolated_data <- data.frame(DateTime = interpolation_dates, Value = interpolated_values) %>%
      mutate(rolling_avg = rollmean(Value, k=10, fill=NA, align='right')) %>%
      mutate(Difference = Value - rolling_avg) %>%
      mutate(Depth = round(depths[i], 1))
      
    match_noise_oxy <- rbind(match_noise_oxy, interpolated_data)
  
}
match_noise_oxy$DateTime <- as.Date(match_noise_oxy$DateTime, format="%Y-%m-%d")
obs_oxy$DateTime <- as.Date(obs_oxy$DateTime, format="%Y-%m-%d")
new_oxy_sd <- sd(match_noise_oxy$Difference, na.rm = TRUE)


#Methane
obs_ch4 <- read.csv('field_data/ch4_gas.csv') %>%
  mutate(DateTime= as.Date(DateTime, format = "%Y-%m-%d"))%>%
  filter(DateTime > "2017-02-26", DateTime < "2020-01-01")
depths <- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)

match_noise_ch4 <- data.frame()
#match <- data.frame()

for(i in 1:length(depths)){
  
  
    match <- obs_ch4 %>%
    dplyr::filter(Depth == depths[i])
    known_dates <- as.Date(match$DateTime, format="%Y-%m-%d")
    known_values <- match$CAR_ch4
      
    # Generate sequence of dates between the sampling dates
    interpolation_dates <- seq(from = min(known_dates), to = max(known_dates), by = "day")
      
    # Perform linear interpolation for each date
    interpolated_values <- approx(known_dates, known_values, xout = interpolation_dates)$y
      
    # Combine the dates and interpolated values into a data frame
    interpolated_data <- data.frame(DateTime = interpolation_dates, Value = interpolated_values) %>%
      mutate(rolling_avg = rollmean(Value, k=10, fill=NA, align='right')) %>%
      mutate(Difference = Value - rolling_avg) %>%
      mutate(Depth = round(depths[i], 1))
      
    match_noise_ch4 <- rbind(match_noise_ch4, interpolated_data)
  
}
match_noise_ch4$DateTime <- as.Date(match_noise_ch4$DateTime, format="%Y-%m-%d")
obs_ch4$DateTime <- as.Date(obs_ch4$DateTime, format="%Y-%m-%d")
new_ch4_sd <- sd(match_noise_ch4$Difference, na.rm = TRUE)


#Ebbulition
ebb_data <- read.csv("field_data/EDI_DATA_EBU_DIFF_DEPTH.csv")
ebb_data <- ebb_data %>%
  mutate(sed_Zone = case_when(
    Depth_m >= 2.5 & Depth_m < 4.5 ~ "zone 3",
    Depth_m >= 4.5 & Depth_m < 6.5 ~ "zone 2",
    Depth_m >= 6.5 & Depth_m < 9.8 ~ "zone 1",
    TRUE ~ "zone 4"  # Default case
  ))
ebb_data$Ebu_rate <- round(ebb_data$Ebu_rate, 4)
ebb_data$Site <- as.character(ebb_data$Site)
ebb_data$DateTime<- as.POSIXct(ebb_data$DateTime,format="%Y-%m-%d %H:%M:%S", tz= "Asia/Shanghai")
#ebb_data$DateTime_day <- as.Date(ebb_data$Datetime_hour)

ebb_data <- ebb_data %>%
  mutate(year = lubridate::year(DateTime)) %>%
  group_by(year) %>%
  filter(DateTime > as.POSIXct("2016-12-31 00:00:00", format="%Y-%m-%d %H:%M:%S",tz="Asia/Shanghai") & DateTime < as.POSIXct("2020-01-01 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai"))


ebb_data_range <- ebb_data %>%
  group_by(DateTime) %>%
  summarise(avg_per_transect = mean(Ebu_rate/16.0421459, na.rm=TRUE), max_per_transect = max(Ebu_rate/16.0421459, na.rm=TRUE), min_per_transect = min(Ebu_rate/16.0421459, na.rm=TRUE)) %>%
  na.omit()

ebb_data_range <- ebb_data_range %>%
  mutate(difference = max_per_transect - min_per_transect)

sd(ebb_data_range$difference)


#Diffusion2017
ebb_data1 <- read.csv("/Users/Kamilla/ZonesDiss_new/field_data/EDI_DATA_EBU_DIFF_DEPTH.csv") 
ebb_data1 <- ebb_data1 %>%
mutate(DateTime = as.POSIXct(ebb_data1$DateTime, format="%Y-%m-%d %H:%M", tz = "Asia/Shanghai")) %>%
mutate(Date = floor_date(DateTime, unit = "hour")) %>%
#mutate(Date = as.Date(DateTime)) %>%
mutate(Diff_rate_mmol = Diff_rate / 16.04) %>%
group_by(Transect, Date) %>%
summarise(avg_per_transect = mean(Diff_rate_mmol), max_per_transect = max(Diff_rate_mmol), min_per_transect = min(Diff_rate_mmol)) %>%
na.omit()

summary_stats <- ebb_data1 %>%
  group_by(Date) %>%
  summarise(
    overall_avg = mean(avg_per_transect, na.rm = TRUE),
    overall_min = min(min_per_transect, na.rm = TRUE),
    overall_max = max(max_per_transect, na.rm = TRUE)
  ) %>%
  mutate(year=year(Date))

summary_stats <- summary_stats %>%
  mutate(difference = overall_max - overall_min)

sd(summary_stats$difference)

#Diffusion 2018&2019

diff_dataset <- read.csv("/Users/Kamilla/ZonesDiss_new/field_data/UGGA_2018_2023.csv") 
diff_dataset1 <- diff_dataset %>%
  mutate(CH4_diff_mmolm2d = CH4Flux_umolCm2s * 86.4) %>%
  filter(as.Date(Date, format="%Y-%m-%d") <= "2020-01-01") %>%
  filter(Reservoir == "FCR", Site==50 | Site ==45) %>%
  mutate(DateTime = as.POSIXct(paste(Date, Start_time), format="%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")) %>%
  mutate(Start_time = hms(Start_time)) %>%
  group_by(Date) %>%
  summarise(avg_per_site = mean(CH4_diff_mmolm2d), max_per_site = max(CH4_diff_mmolm2d), min_per_site = min(CH4_diff_mmolm2d), avg_start_time_sec = mean(as.numeric(Start_time)))  

diff_dataset1 <- diff_dataset1 %>%
  mutate(difference = max_per_site - min_per_site)

sd(diff_dataset1$difference)

1/3 + 2/3 * 0.81
