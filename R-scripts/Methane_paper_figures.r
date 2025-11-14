#Loading packages
library(ncdf4)
library(glmtools)
library(rlang)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(streamgraph)
library(lubridate)
library(ggstream)
library(wesanderson)
library(ggpubr)
library(grid)
library(patchwork)
library(ggpattern)


#Setting working directory
setwd('.../FCR-GLM-methane/FCR_model_setup')
sim_folder <- getwd()
output <- nc_open("output/output.nc")
nc_file <- file.path(sim_folder, 'output/output.nc')

#Colours
library(wesanderson)
mycolours <- wes_palette(n=5, name="Moonrise3")

#Simulation period
start_date <- as.POSIXct("2017-02-27 01:00:00", tz = "Etc/GMT--8") 
end_date <- as.POSIXct("2019-12-31 00:00:00", tz = "Etc/GMT--8") 

hourly_vector <- seq(from = start_date, to = end_date, by = "hour")

#########################################FIGURE 3: DISSOLVED CH4 VALIDATION PLOT########################################################################

#Methane concentration
var="CH4_ch4"
obs_methane<-read.csv('field_data/observed_ch4.csv') %>%
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), DateTime_H = as.POSIXct(DateTime_H, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) 

depths<- unique(obs_methane$Depth)

new_ch4 <- get_var(nc_file, "CH4_ch4", reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("CH4_ch4_"), names_to="Depth", names_prefix="CH4_ch4_", values_to = "CH4_ch4") %>%
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"))

new_ch4 <- new_ch4 %>%
  mutate(DateTime_H = DateTime)

#Plot 

plot <- vector('list')

for(i in 1:length(depths)){
  
  
  plot[[i]] <- obs_methane %>%
    filter(Depth == depths[i], DateTime > as.POSIXct("2017-02-27 01:00:00", tz = "Etc/GMT--8")) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = ch4_obs, colour="Observed")) +
    ggplot2::geom_point(pch=21, size=2, fill ="#85D4E3")+
    geom_line(data=filter(new_ch4, Depth==depths[i]), aes(x=DateTime, y=CH4_ch4, colour="Modelled"), linewidth=1)+
    ggplot2::ggtitle(paste("Depth:", depths[i], "m"))+
    xlab("Date")+
    ylab(expression(Methane~(mmol/m^{3})))+
    scale_colour_manual(
    values = c("Modelled" = "#85D4E3", "Observed" = "black")
  )+
  ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 18),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm")
  )
  
}
plot

combinedPlot <- wrap_plots(plot, ncol = 2) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot

ggsave("Plots/Figure3_validation_CH4.png",
       plot = combinedPlot,
       width = 400, 
       height = 300,
       units = "mm")

#########################################FIGURE 4: STACKED TIMESERIES PLOTS########################################################################

#Modeled sediment bubble flux for each zone
ch4_ebb <-  ncvar_get(output, "CH4_ch4s_ebb_Z")
ch4_ebb_sed_zones <- as.data.frame(t(ch4_ebb)) %>%
mutate(Date = hourly_vector) %>%
select(-5) %>%
rename(zone_1 = V1, zone_2 = V2, zone_3 = V3, zone_4 = V4) 

ch4_ebb_sed_zones <- ch4_ebb_sed_zones %>%
  mutate(year = lubridate::year(Date)) %>%
  group_by(year)

ch4_ebb_sed_zones <- ch4_ebb_sed_zones %>%
   pivot_longer(cols= c("zone_1", "zone_2", "zone_3", "zone_4"), names_to = "zones", values_to = "ch4_ebb")

#Get daily fluxes
ch4_ebb_sed_zones$Date <- as.POSIXct(ch4_ebb_sed_zones$Date, format = "%Y-%m-%d %H:%M:%S")

ch4_ebb_sed_zones <- ch4_ebb_sed_zones %>%
  mutate(Date_only = as.Date(Date))

# Calculate the daily average flux for each zone
daily_avg_ebb <- ch4_ebb_sed_zones %>%
  group_by(Date_only, zones) %>%
  summarise(daily_avg_ch4_ebb = mean(ch4_ebb, na.rm = TRUE))%>%
  mutate(year = lubridate::year(Date_only)) %>%
  group_by(year)

#Modeled sediment diffusion for each zone
ch4_diff <-  ncvar_get(output, "CH4_ch4s_diff_Z")
ch4_diff_sed_zones <- as.data.frame(t(ch4_diff)) %>%
mutate(Date = hourly_vector) %>%
select(-5) %>%
rename(zone_1 = V1, zone_2 = V2, zone_3 = V3, zone_4 = V4)

ch4_diff_sed_zones <- ch4_diff_sed_zones %>%
mutate(year = lubridate::year(Date)) %>%
group_by(year)

ch4_diff_sed_zones <- ch4_diff_sed_zones %>%
   pivot_longer(cols= c("zone_1", "zone_2", "zone_3", "zone_4"), names_to = "zones", values_to = "ch4_diff")

#Get daily fluxes
ch4_diff_sed_zones$Date <- as.POSIXct(ch4_diff_sed_zones$Date, format = "%Y-%m-%d %H:%M:%S") 

ch4_diff_sed_zones <- ch4_diff_sed_zones %>%
  mutate(Date_only = as.Date(Date))

# Calculate the daily average flux for each zone
daily_avg_diff <- ch4_diff_sed_zones %>%
  group_by(Date_only, zones) %>%
  summarise(daily_avg_ch4_diff = mean(ch4_diff, na.rm = TRUE))%>%
  mutate(year = lubridate::year(Date_only)) %>%
  group_by(year)

#Stacked plots for the 3 simulation years
#2017
sed_flux_2017 <- ggplot(data=filter(daily_avg_ebb, year=="2017"), aes(x=rev(Date_only), y=daily_avg_ch4_ebb, fill = forcats::fct_rev(zones))) + 
geom_area()+
geom_area(data=filter(daily_avg_diff, year=="2017"), aes(x=rev(Date_only), y=-daily_avg_ch4_diff, fill=forcats::fct_rev(zones))) +
geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
annotate("text", x = as.Date("2018-02-01"), y = -2, label = "Diffusion", size = 10, color = "black", family="Helvetica") +
annotate("text", x = as.Date("2018-02-01"), y = 2, label = "Ebullition", size = 10, color = "black", family= "Helvetica") +
annotate("text", x = as.Date("2017-08-15"), y = -10, label = "Oxygenation ON", size = 8, color = "black", angle = 90, family= "Helvetica") +
coord_flip() +
scale_fill_manual(name = "Sediment zones",
                    values = c("zone_1" = "#1E2F53", "zone_2" = "#40647E", "zone_3" = "#5990B3", "zone_4" = "#ADC7B6"),
                    labels = c("zone_1" = "Zone 1", "zone_2" = "Zone 2", "zone_3" = "Zone 3", "zone_4" = "Zone 4"))+
xlim(c(as.Date("2017-03-01"), as.Date("2018-01-01")))+
scale_x_date(breaks = c(ymd("2017-12-01", "2017-09-01", "2017-06-01", "2017-03-01")), date_labels = c("Mar 2017", "Jun 2017", "Sep 2017", "Dec 2017"))+
scale_y_continuous(
    breaks = c(-10, -5, 0, 5),  
    labels = c("10", "5", "0", "5"),  
    limits = c(-10, 5)
  ) +
guides(fill = guide_legend(reverse=TRUE))+
labs(y = expression(bold(Methane~sediment~release~(mmol/m^{2}/day))), x="Date") +
ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 18, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm")
)
  sed_flux_2017


#2018
sed_flux_2018 <- ggplot(data=filter(daily_avg_ebb, year=="2018"), aes(x=rev(Date_only), y=daily_avg_ch4_ebb, fill = forcats::fct_rev(zones))) + 
geom_area()+
geom_area(data=filter(daily_avg_diff, year=="2018"), aes(x=rev(Date_only), y=-daily_avg_ch4_diff, fill=forcats::fct_rev(zones))) +
geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
annotate("text", x = as.Date("2019-02-01"), y = -2, label = "Diffusion", size = 10, color = "black", family="Helvetica") +
annotate("text", x = as.Date("2019-02-01"), y = 2, label = "Ebullition", size = 10, color = "black", family="Helvetica") +
annotate("text", x = as.Date("2018-07-15"), y = -10, label = "Oxygenation OFF after late July", size = 8, color = "black", angle = 90, family="Helvetica") +
xlim(c(as.Date("2018-01-01"), as.Date("2019-01-01")))+
scale_x_date(breaks = c(ymd("2019-01-01", "2018-10-01", "2018-07-01", "2018-04-01", "2018-01-01")), date_labels = c("Jan 2018", "Apr 2018", "Jul 2018", "Oct 2018", "Jan 2019"))+
coord_flip() +
scale_fill_manual(name = "Sediment zones",
                  values = c("zone_1" = "#1E2F53", "zone_2" = "#40647E", "zone_3" = "#5990B3", "zone_4" = "#ADC7B6"),
                  labels = c("zone_1" = "Zone 1", "zone_2" = "Zone 2", "zone_3" = "Zone 3", "zone_4" = "Zone 4"))+
scale_y_continuous(
    breaks = c(-10, -5, 0, 5),  
    labels = c("10", "5", "0", "5"),  
    limits = c(-10, 5)
  ) +
guides(fill = guide_legend(reverse=TRUE))+
labs(y = expression(bold(Methane~sediment~release~(mmol/m^{2}/day))), x="Date") +
ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm")
  )
sed_flux_2018

#2019
sed_flux_2019<- ggplot(data=filter(daily_avg_ebb, year=="2019"), aes(x=rev(Date_only), y=daily_avg_ch4_ebb, fill = forcats::fct_rev(zones))) + 
geom_area()+
geom_area(data=filter(daily_avg_diff, year=="2019"), aes(x=rev(Date_only), y=-daily_avg_ch4_diff, fill=forcats::fct_rev(zones))) +
geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
annotate("text", x = as.Date("2020-02-01"), y = -2, label = "Diffusion", size = 10, color = "black", family="Helvetica") +
annotate("text", x = as.Date("2020-02-01"), y = 2, label = "Ebullition", size = 10, color = "black", family="Helvetica") +
annotate("text", x = as.Date("2019-07-15"), y = -10, label = "Oxygenation ON & OFF", size = 8, color = "black", angle = 90, family="Helvetica") +
coord_flip() +
xlim(c(as.Date("2019-01-01"), as.Date("2020-01-01")))+
scale_x_date(breaks = c(ymd("2020-01-01", "2019-10-01", "2019-07-01", "2019-04-01", "2019-01-01")), date_labels = c("Jan 2019", "Apr 2019", "Jul 2019", "Oct 2019", "Jan 2020"))+
scale_fill_manual(name = "Sediment zones",
                  values = c("zone_1" = "#1E2F53", "zone_2" = "#40647E", "zone_3" = "#5990B3", "zone_4" = "#ADC7B6"),
                  labels = c("zone_1" = "Zone 1", "zone_2" = "Zone 2", "zone_3" = "Zone 3", "zone_4" = "Zone 4")) +
scale_y_continuous(
    breaks = c(-10, -5, 0, 5), 
    labels = c("10", "5", "0", "5"),  
    limits = c(-10, 5)
  ) +
guides(fill = guide_legend(reverse=TRUE))+
labs(y = expression(Methane~sediment~release~(mmol/m^{2}/day))) +
ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm")
  )
sed_flux_2019

#Combine plots
sed_flux_combined <- ggarrange(sed_flux_2017, sed_flux_2018, sed_flux_2019, ncol=1, nrow=3, common.legend=TRUE) +
 theme(plot.margin = unit(c(0.2, 0.2, 0.2, 2), "cm"))
 sed_flux_combined

#Add arrow
arrow_x <- -0.02  
arrow_y <- 0.95
arrow_text <- "Time"
sed_flux_combined1 <- sed_flux_combined+
  annotation_custom(
    grob = grobTree(linesGrob(x = c(arrow_x, arrow_x), y = c(arrow_y, arrow_y-0.9),
                     arrow = arrow(type = "open", length = unit(0.1, "inches")),
                     gp = gpar(col = "black", lwd = 2)),
          textGrob(label = arrow_text, x = arrow_x - 0.02, y = arrow_y - 0.4,
               hjust = 1, vjust = 0.5, gp = gpar(col = "black", fontface = "bold", fontsize = 20), rot=90)
  ),
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf
  )
sed_flux_combined1


ggsave("Plots/Figure4_sedflux_CH4.png",
       plot = sed_flux_combined1,
       width = 250, 
       height = 450,
       units = "mm")


#########################################FIGURE 5: EMISSION SUMMARY########################################################################

#Outdates timezone handling function
handle_timezone <- function(dt) {
  # Check if the timezone is Etc/GMT--8 and convert it
  if (attr(dt, "tzone") == "Etc/GMT--8") {
    # Convert to a valid timezone (e.g., Asia/Shanghai)
    dt <- with_tz(dt, tzone = "Asia/Shanghai")
  }
  return(dt)
}

#Observed diffusion data from 2017
ebb_data1 <- read.csv("field_data/EDI_DATA_EBU_DIFF_DEPTH.csv") 
ebb_data1 <- ebb_data1 %>%
mutate(DateTime = as.POSIXct(ebb_data1$DateTime, format="%Y-%m-%d %H:%M", tz = "Asia/Shanghai")) %>%
mutate(Date = floor_date(DateTime, unit = "hour")) %>%
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

#Modeled diffusion
ch4_atm <- get_var(nc_file, "CH4_ch4_atm")
ch4_atm$DateTime <- as.POSIXct(ch4_atm$DateTime, format="%Y-%m-%d %H:%M")
ch4_atm$DateTime <- handle_timezone(ch4_atm$DateTime)

ch4_atm <- ch4_atm %>%
  mutate(year = lubridate::year(DateTime)) %>%
  filter(
    (year == 2017 & DateTime >= as.POSIXct("2017-05-01 00:00:00", tz="Asia/Shanghai") & DateTime < as.POSIXct("2017-11-08 23:00:00", tz="Asia/Shanghai"))) %>%#|
      rename(Date = DateTime) 

#Interpolate observations for plotting
summary_stats_interp <- data.frame(
         Date = as.POSIXct(ch4_atm$Date, format = "%Y-%m-%d %H:%M:%S"), 
         interp_min = approx(summary_stats$Date, summary_stats$overall_min, ch4_atm$Date, method = "linear")$y,
         interp_max = approx(summary_stats$Date, summary_stats$overall_max, ch4_atm$Date, method = "linear")$y,
         interp_avg = approx(summary_stats$Date, summary_stats$overall_avg, ch4_atm$Date, method = "linear")$y
         )

#merge modelled and observed datasets
merged_df1 <- merge(ch4_atm, summary_stats_interp, by = "Date", all = TRUE) %>%
  mutate(year = year(Date))

#Plot of diffusive flux in 2017
diff_transect <- ggplot(data=filter(merged_df1, year == 2017), aes(x=Date, y=CH4_ch4_atm, colour="Modelled"))+
geom_line(linewidth=1)+
geom_ribbon(data=filter(merged_df1, year==2017, Date > as.POSIXct("2017-05-15 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai"), Date < as.POSIXct("2017-10-30 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai")), aes(x=Date, ymin=interp_min, ymax=interp_max, fill="Observed_range"), alpha = 0.4, colour=NA) + 
geom_line(data=filter(merged_df1, year==2017, Date > as.POSIXct("2017-05-15 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai"), Date < as.POSIXct("2017-10-30 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai")), aes(x=Date, y=interp_avg, colour="Observed_avg"), linewidth = 0.5)+
geom_point(data=filter(summary_stats, year==2017), aes(x=Date, y=overall_avg, colour="Observed_points"))+
ylab(expression(Diffusive~flux~(mmol/m^{2}/day))) + 
ylim(c(0, 25))+
scale_x_datetime(
    breaks = seq(as.POSIXct("2017-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), as.POSIXct("2017-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), by = "2 months"),
    name = "Date",
    date_labels = "%b %Y" 
) +
scale_colour_manual(
    values = c("Modelled" = "royalblue4", "Observed_avg" = mycolours[3], "Observed_points" = mycolours[3]),
    name = "Legend",
    labels = c("Modelled" = "Modelled", "Observed_avg" = "Observed mean", "Observed_points" = "Observed points")
  ) +
  scale_fill_manual(
    values = c("Observed_range" = mycolours[3]), 
    name = "Legend",
    labels = c("Observed_range" = "Observed range")
  ) +
theme_light()+
ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 14, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    legend.position = "top"
  )
diff_transect


# Function to convert "12H 0M 3S" format to "HH:MM:SS"
convert_to_time <- function(time_str) {
  # Extract hours, minutes, and seconds using regular expressions
  hours <- as.numeric(gsub("H.*", "", time_str))
  minutes <- as.numeric(gsub(".*H (.*)M.*", "\\1", time_str))
  seconds <- as.numeric(gsub(".*M (.*)S", "\\1", time_str))
  
  # Round seconds to the nearest integer to avoid decimal values
  seconds <- round(seconds)
  
  # Debugging: print extracted values
  print(paste("Hours:", hours, "Minutes:", minutes, "Seconds:", seconds))
  
  # Return the time in "HH:MM:SS" format, ensuring proper numeric conversion
  return(sprintf("%02d:%02d:%02d", hours, minutes, seconds))
}


#Observed diffusion flux data for 2018 and 2019 (UGGA)

diff_dataset <- read.csv("field_data/UGGA_2018_2023.csv") 
diff_dataset1 <- diff_dataset %>%
  mutate(CH4_diff_mmolm2d = CH4Flux_umolCm2s * 86.4) %>%
  filter(as.Date(Date, format="%Y-%m-%d") <= "2020-01-01") %>%
  filter(Reservoir == "FCR", Site==50) %>%
  mutate(DateTime = as.POSIXct(paste(Date, Start_time), format="%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")) %>%
  mutate(Start_time = hms(Start_time)) %>%
  group_by(Date) %>%
  summarise(avg_per_site = mean(CH4_diff_mmolm2d), max_per_site = max(CH4_diff_mmolm2d), min_per_site = min(CH4_diff_mmolm2d), avg_start_time_sec = mean(as.numeric(Start_time)))  %>%
  mutate(avg_start_time = seconds_to_period(avg_start_time_sec)) %>%
  mutate(avg_start_time = format(avg_start_time, "%H:%M:%S")) %>%
  mutate(avg_start_time = sapply(avg_start_time, convert_to_time)) %>%
  mutate(DateTime = floor_date(as.POSIXct(paste(Date, avg_start_time), format="%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai"), unit="hour")) %>%
  mutate(year = year(DateTime))


#Modelled diffusion
ch4_atm <- get_var(nc_file, "CH4_ch4_atm")
ch4_atm$DateTime <- as.POSIXct(ch4_atm$DateTime, format="%Y-%m-%d %H:%M")
ch4_atm$DateTime <- handle_timezone(ch4_atm$DateTime)

ch4_atm <- ch4_atm %>%
  mutate(year = lubridate::year(DateTime)) %>%
  filter(
    (year == 2018 & DateTime >= as.POSIXct("2018-05-01 00:00:00", tz="Asia/Shanghai") & DateTime < as.POSIXct("2018-11-08 23:00:00", tz="Asia/Shanghai")) |
     (year == 2019 & DateTime >= as.POSIXct("2019-05-01 00:00:00", tz="Asia/Shanghai") & DateTime < as.POSIXct("2019-11-08 23:00:00", tz="Asia/Shanghai"))
    ) 

#Interpolating observed data for plotting
summary_stats_interp <- data.frame(
         DateTime = as.POSIXct(ch4_atm$DateTime, format = "%Y-%m-%d %H:%M:%S"),  
         interp_min = approx(diff_dataset1$DateTime, diff_dataset1$min_per_site, ch4_atm$DateTime, method = "linear")$y,
         interp_max = approx(diff_dataset1$DateTime, diff_dataset1$max_per_site, ch4_atm$DateTime, method = "linear")$y,
         interp_avg = approx(diff_dataset1$DateTime, diff_dataset1$avg_per_site, ch4_atm$DateTime, method = "linear")$y
         )

#Merging modelled and observed data
merged_df2 <- merge(ch4_atm, summary_stats_interp, by = "DateTime", all = TRUE) %>%
  mutate(year = year(DateTime))

#Diffusion plot for 2018
diff_transect2 <- ggplot(data=filter(merged_df2, year == 2018), aes(x=DateTime, y=CH4_ch4_atm, colour="Modelled"))+
geom_line(linewidth=1)+
geom_ribbon(data=filter(merged_df2, year==2018, DateTime > as.POSIXct("2018-05-07 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai"), DateTime < as.POSIXct("2018-10-30 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai")), aes(x=DateTime, ymin=interp_min, ymax=interp_max, fill="Observed_range"), alpha = 0.4, colour=NA) + 
geom_line(data=filter(merged_df2, year==2018, DateTime > as.POSIXct("2018-05-07 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai"), DateTime < as.POSIXct("2018-10-30 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai")), aes(x=DateTime, y=interp_avg, colour="Observed_avg"), linewidth = 0.5)+
geom_point(data=filter(diff_dataset1, year==2018), aes(x=DateTime, y=avg_per_site, colour="Observed_points"))+
ylab(expression(Diffusive~flux~(mmol/m^{2}/day))) + 
ylim(c(0, 25))+
scale_x_datetime(
    breaks = seq(as.POSIXct("2018-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), as.POSIXct("2018-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), by = "2 months"),
    name = "Date",
    date_labels = "%b %Y" 
) +
scale_colour_manual(
    values = c("Modelled" = "royalblue4", "Observed_avg" = mycolours[3], "Observed_points" = mycolours[3]),
    name = "Legend",
    labels = c("Modelled" = "Modelled", "Observed_avg" = "Observed mean", "Observed_points" = "Observed points")
  ) +
  scale_fill_manual(
    values = c("Observed_range" = mycolours[3]), # color for the observed ribbon
    name = "Legend",
    labels = c("Observed_range" = "Observed range")
  ) +
theme_light()+
ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    legend.position = "none"
  )
diff_transect2

#Diffusion plot for 2019
diff_transect3 <- ggplot(data=filter(merged_df2, year == 2019), aes(x=DateTime, y=CH4_ch4_atm, colour="Modelled"))+
geom_line(linewidth=1)+
geom_ribbon(data=filter(merged_df2, year==2019, DateTime > as.POSIXct("2019-05-20 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai"), DateTime < as.POSIXct("2019-10-30 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai")), aes(x=DateTime, ymin=interp_min, ymax=interp_max, fill="Observed_range"), alpha = 0.4, colour=NA) + 
geom_line(data=filter(merged_df2, year==2019, DateTime > as.POSIXct("2019-05-20 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai"), DateTime < as.POSIXct("2019-10-30 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Asia/Shanghai")), aes(x=DateTime, y=interp_avg, colour="Observed_avg"), linewidth = 0.5)+
geom_point(data=filter(diff_dataset1, year==2019), aes(x=DateTime, y=avg_per_site, colour="Observed_points"))+
ylab(expression(Diffusive~flux~(mmol/m^{2}/day))) + 
ylim(c(0, 25))+
scale_x_datetime(
    breaks = seq(as.POSIXct("2019-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), as.POSIXct("2019-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), by = "2 months"),
    name = "Date",
    date_labels = "%b %Y", 
) +
scale_colour_manual(
    values = c("Modelled" = "royalblue4", "Observed_avg" = mycolours[3], "Observed_points" = mycolours[3]),
    name = "Legend",
    labels = c("Modelled" = "Modelled", "Observed_avg" = "Observed mean", "Observed_points" = "Observed points")
  ) +
  scale_fill_manual(
    values = c("Observed_range" = mycolours[3]), # color for the observed ribbon
    name = "Legend",
    labels = c("Observed_range" = "Observed range")
  ) +
theme_light()+
ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    legend.position = "none"
  )
diff_transect3

#Observed ebullition data 
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
ebb_data$DateTime<- as.POSIXct(ebb_data$DateTime,format="%Y-%m-%d %H:%M:%S", tz= "Etc/GMT--8")

ebb_data <- ebb_data %>%
  mutate(year = lubridate::year(DateTime)) %>%
  group_by(year) %>%
  filter(DateTime > as.POSIXct("2016-12-31 00:00:00", format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT--8") & DateTime < as.POSIXct("2020-01-01 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"))


ebb_data_range <- ebb_data %>%
  group_by(DateTime) %>%
  summarise(avg_per_transect = mean(Ebu_rate/16.0421459, na.rm=TRUE), max_per_transect = max(Ebu_rate/16.0421459, na.rm=TRUE), min_per_transect = min(Ebu_rate/16.0421459, na.rm=TRUE)) %>%
  na.omit()


#Modeled ebullition
ch4_ebb_model <- glmtools::get_var(nc_file, "BUB_bub_fluxes_Wch4") 
ch4_ebb <- ch4_ebb_model %>%
  mutate(year = lubridate::year(DateTime)) %>%
  filter(
    (year == 2017 & DateTime >= as.POSIXct("2017-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8") & DateTime < as.POSIXct("2017-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) |
      (year == 2018 & DateTime >= as.POSIXct("2018-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8") & DateTime < as.POSIXct("2018-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) |
      (year == 2019 & DateTime >= as.POSIXct("2019-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8") & DateTime < as.POSIXct("2019-11-08 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"))
  ) 
  
#Interpolating observed data for plotting
summary_ebb_interp <- data.frame(
         DateTime = as.POSIXct(ch4_ebb$DateTime, format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), 
         interp_min = approx(ebb_data_range$DateTime, ebb_data_range$min_per_transect, ch4_ebb$DateTime, method = "linear")$y,
         interp_max = approx(ebb_data_range$DateTime, ebb_data_range$max_per_transect, ch4_ebb$DateTime, method = "linear")$y,
         interp_avg = approx(ebb_data_range$DateTime, ebb_data_range$avg_per_transect, ch4_ebb$DateTime, method = "linear")$y
         )

#Merge modelled and observed data
merged_df <- merge(ch4_ebb, summary_ebb_interp, by = "DateTime", all = TRUE) %>%
  mutate(year = year(DateTime))

ebb_data_range <- ebb_data_range %>%
  mutate(year = lubridate::year(DateTime))

#Plot ebullition 2017
ebb_transect <- ggplot(data=filter(merged_df, year == 2017), aes(x=DateTime, y=BUB_bub_fluxes_Wch4, colour="Modelled"))+
geom_line(linewidth=1)+
geom_ribbon(data=filter(merged_df, year==2017, DateTime > as.POSIXct("2017-05-07 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), DateTime < as.POSIXct("2017-10-23 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")), aes(x=DateTime, ymin=interp_min, ymax=interp_max, fill="Observed_range"), alpha = 0.4, colour=NA) + 
geom_line(data=filter(merged_df, year==2017, DateTime > as.POSIXct("2017-05-07 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), DateTime < as.POSIXct("2017-10-23 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")), aes(x=DateTime, y=interp_avg, colour="Observed_avg"), linewidth = 0.5)+
geom_point(data=filter(ebb_data_range, year==2017), aes(x=DateTime, y=avg_per_transect, colour="Observed_points"))+
ylab(expression(Ebullitive~flux~(mmol/m^{2}/day))) + 
ylim(0, 25)+
scale_x_datetime(
    breaks = seq(as.POSIXct("2017-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), as.POSIXct("2017-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), by = "2 months"),
    name = "Date",
    date_labels = "%b %Y" 
) +
scale_colour_manual(
    values = c("Modelled" = "tomato4", "Observed_avg" = mycolours[2], "Observed_points" = mycolours[2]),
    name = "Legend",
    labels = c("Modelled" = "Modelled", "Observed_avg" = "Observed mean", "Observed_points" = "Observed points")
  ) +
  scale_fill_manual(
    values = c("Observed_range" = mycolours[2]), 
    name = "Legend",
    labels = c("Observed_range" = "Observed range")
  ) +
theme_light()+
ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 14, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    legend.position = "top"
  )
ebb_transect

#Plot ebullition 2018
ebb_transect2 <- ggplot(data=filter(merged_df, year == 2018), aes(x=DateTime, y=BUB_bub_fluxes_Wch4, colour="Modelled"))+
geom_line(linewidth=1)+
geom_ribbon(data=filter(merged_df, year==2018, DateTime > as.POSIXct("2018-05-07 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), DateTime < as.POSIXct("2018-10-29 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")), aes(x=DateTime, ymin=interp_min, ymax=interp_max, fill="Observed_range"), alpha = 0.4, colour=NA) + 
geom_line(data=filter(merged_df, year==2018, DateTime > as.POSIXct("2018-05-07 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), DateTime < as.POSIXct("2018-10-29 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")), aes(x=DateTime, y=interp_avg, colour="Observed_avg"), linewidth = 0.5)+
geom_point(data=filter(ebb_data_range, year==2018), aes(x=DateTime, y=avg_per_transect, colour="Observed_points"))+
ylab(expression(Ebullitive~flux~(mmol/m^{2}/day))) + 
ylim(0, 25)+
scale_x_datetime(
    breaks = seq(as.POSIXct("2018-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), as.POSIXct("2018-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), by = "2 months"),
    name = "Date",
    date_labels = "%b %Y" 
) +
scale_colour_manual(
    values = c("Modelled" = "tomato4", "Observed_avg" = mycolours[2], "Observed_points" = mycolours[2]),
    name = "Legend",
    labels = c("Modelled" = "Modelled", "Observed_avg" = "Observed mean", "Observed_points" = "Observed points")
  ) +
  scale_fill_manual(
    values = c("Observed_range" = mycolours[2]), 
    name = "Legend",
    labels = c("Observed_range" = "Observed range")
  ) +
theme_light()+
ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    legend.position = "none"
  )
ebb_transect2

#Plot ebullition 2019
ebb_transect3 <- ggplot(data=filter(merged_df, year == 2019), aes(x=DateTime, y=BUB_bub_fluxes_Wch4, colour="Modelled"))+
geom_line(linewidth=1)+
geom_ribbon(data=filter(merged_df, year==2019, DateTime > as.POSIXct("2019-05-27 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), DateTime < as.POSIXct("2019-11-07 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")), aes(x=DateTime, ymin=interp_min, ymax=interp_max, fill="Observed_range"), alpha = 0.4, colour=NA) + 
geom_line(data=filter(merged_df, year==2019, DateTime > as.POSIXct("2019-05-27 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), DateTime < as.POSIXct("2019-11-07 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")), aes(x=DateTime, y=interp_avg, colour="Observed_avg"), linewidth = 0.5)+
geom_point(data=filter(ebb_data_range, year==2019), aes(x=DateTime, y=avg_per_transect, colour="Observed_points"))+
ylab(expression(Ebullitive~flux~(mmol/m^{2}/day))) + 
ylim(0, 25)+
scale_x_datetime(
    breaks = seq(as.POSIXct("2019-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), as.POSIXct("2019-11-08 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"), by = "2 months"),
    name = "Date",
    date_labels = "%b %Y", 
) +
scale_colour_manual(
    values = c("Modelled" = "tomato4", "Observed_avg" = mycolours[2], "Observed_points" = mycolours[2]),
    name = "Legend",
    labels = c("Modelled" = "Modelled", "Observed_avg" = "Observed mean", "Observed_points" = "Observed points")
  ) +
  scale_fill_manual(
    values = c("Observed_range" = mycolours[2]), # color for the observed ribbon
    name = "Legend",
    labels = c("Observed_range" = "Observed range")
  ) +
theme_light()+
ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12, colour="black"),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    legend.position = "none"
  )
  

ebb_transect3


combined_all <- diff_transect + ebb_transect + diff_transect2 + ebb_transect2 + diff_transect3 + ebb_transect3 + plot_layout(ncol = 2)
combined_all

ggsave("Plots/Figure5_emission_summary.png",
       plot = combined_all,
       width = 400, 
       height = 400,
       units = "mm")

#########################################FIGURE 6: BUDGET PLOT########################################################################

#sediment area
zarea <- ncvar_get(output, "zarea")
zarea_zones <- as.data.frame(t(zarea)) %>%
mutate(Date = hourly_vector) %>%
select(-5) %>%
rename(zone_1 = V1, zone_2 = V2, zone_3 = V3, zone_4 = V4)

#Sediment fluxes
#Diffusion
ch4_diff <-  ncvar_get(output, "CH4_ch4s_diff_Z")
ch4_diff_sed_zones <- as.data.frame(t(ch4_diff)) %>%
mutate(Date = hourly_vector) %>%
select(-5) %>%
rename(zone_1 = V1, zone_2 = V2, zone_3 = V3, zone_4 = V4)

#newcode
ch4_diff_summary <- ch4_diff_sed_zones %>%
  mutate(Date = as.Date(Date)) %>%  # Extract the date (drop time)
  group_by(Date) %>%
  summarise(across(c(zone_1, zone_2, zone_3, zone_4), ~ mean(.x, na.rm = TRUE))) %>%
  rowwise() %>%
  mutate(mean_value = mean(c_across(c(zone_1, zone_2, zone_3, zone_4)), na.rm = TRUE)) %>%
  ungroup()

ch4_diff_summary1 <- ch4_diff_summary %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(annual_mean = mean(mean_value, na.rm=TRUE), annual_min = min(mean_value, na.rm= TRUE), annual_max = max(mean_value, na.rm=TRUE))
  


#converting rates to hourly (from daily) and from mmol to mol
ch4_diff_sed_zones <- ch4_diff_sed_zones %>%
  mutate(zone1_Marea = 0.001 * (zone_1/24) * zarea_zones$zone_1, zone2_Marea = 0.001 * (zone_2/24) * zarea_zones$zone_2, zone3_Marea = 0.001 * (zone_3/24) * zarea_zones$zone_3, zone4_Marea = 0.001 * (zone_4/24) * zarea_zones$zone_4)

  
#Ebullition
ch4_ebb <-  ncvar_get(output, "CH4_ch4s_ebb_Z")
ch4_ebb_sed_zones <- as.data.frame(t(ch4_ebb)) %>%
mutate(Date = hourly_vector) %>%
select(-5) %>%
rename(zone_1 = V1, zone_2 = V2, zone_3 = V3, zone_4 = V4)

#converting rates to hourly (from daily) and from mmol to mol
ch4_ebb_sed_zones <- ch4_ebb_sed_zones %>%
  mutate(zone1_Marea = 0.001 * (zone_1/24) * zarea_zones$zone_1, zone2_Marea = 0.001 * (zone_2/24) * zarea_zones$zone_2, zone3_Marea = 0.001 * (zone_3/24) * zarea_zones$zone_3, zone4_Marea = 0.001 * (zone_4/24) * zarea_zones$zone_4)
  

#Atmospheric fluxes

#Surface area
zarea_zones$surface_area <- zarea_zones$zone_1 + zarea_zones$zone_2 + zarea_zones$zone_3 + zarea_zones$zone_4

#Diffusion [mmol/m2/d]
ch4_atm <- get_var(nc_file, "CH4_ch4_atm")

#converting rates to hourly (from daily) and from mmol to mol
ch4_atm <- ch4_atm %>%
  mutate(ch4_hourly = 0.001 * zarea_zones$surface_area * (CH4_ch4_atm/24))

#Ebullition [mmol/m2/d]
ch4_ebb_atm <- glmtools::get_var(nc_file, "BUB_bub_fluxes_Wch4") 

#converting rates to hourly (from daily) and from mmol to mol
ch4_ebb_atm <- ch4_ebb_atm %>%
  mutate(ch4_ebb_hourly = 0.001 * zarea_zones$surface_area * (BUB_bub_fluxes_Wch4/24))

#Oxidation
DateTime <- glmtools::get_var(nc_file, "CH4_ch4ox") 
ch4_oxidation <- ncvar_get(output, "CH4_ch4ox") #mmol/m3/day
volume <- ncvar_get(output, "V") #m3

#converting rates to hourly (from daily) and from mmol to mol
ch4_oxidation_budget <- as.data.frame((ch4_oxidation/24) * volume * 0.001)  %>%
summarise(across(everything(), ~ sum(., na.rm=TRUE)))

ch4_oxidation_budget_df <- data.frame(
  DateTime = DateTime$DateTime,
  CH4_oxid_molperday = t(ch4_oxidation_budget)
)

#Dissolution
dissolution <- ncvar_get(output, "BUB_gas_exchange_Wch4") #mmol/m3/day

#converting rates to hourly (from daily) and from mmol to mol
ch4_dissolution_budget <- as.data.frame((dissolution/24) * volume * 0.001)  %>%
summarise(across(everything(), ~ sum(., na.rm=TRUE)))

ch4_dissolution_budget_df <- data.frame(
  DateTime = DateTime$DateTime,
  CH4_diss_molperday = t(ch4_dissolution_budget)
)

#Storage
ch4_conc <- ncvar_get(output, "CH4_ch4") 

#mmol to mol
ch4_storage <- as.data.frame(ch4_conc * volume * 0.001) %>%
summarise(across(everything(), ~ sum(., na.rm=TRUE)))

ch4_storage_df <- data.frame(
  DateTime = DateTime$DateTime,
  CH4_storage_molperday = t(ch4_storage)
)

#hourly dataframe
master_df_hourly <- data.frame(
  DateTime = as.POSIXct(ch4_dissolution_budget_df$DateTime),
  total_diss = ch4_dissolution_budget_df$CH4_diss_molperday,
  total_oxid = ch4_oxidation_budget_df$CH4_oxid_molperday,
  total_diff_atm = ch4_atm$ch4_hourly,
  total_diff_sed = ch4_diff_sed_zones$zone1_Marea + ch4_diff_sed_zones$zone2_Marea + ch4_diff_sed_zones$zone3_Marea + ch4_diff_sed_zones$zone4_Marea,
  ch4_conc = ch4_storage_df$CH4_storage_molperday,
  ch4_balance = ch4_dissolution_budget_df$CH4_diss_molperday + (ch4_diff_sed_zones$zone1_Marea + ch4_diff_sed_zones$zone2_Marea + ch4_diff_sed_zones$zone3_Marea + ch4_diff_sed_zones$zone4_Marea) - ch4_atm$ch4_hourly - ch4_oxidation_budget_df$CH4_oxid_molperday,
  total_ebb_sed = ch4_ebb_sed_zones$zone1_Marea + ch4_ebb_sed_zones$zone2_Marea + ch4_ebb_sed_zones$zone3_Marea + ch4_ebb_sed_zones$zone4_Marea,
  total_ebb_atm = ch4_ebb_atm$ch4_ebb_hourly,
  ebb_neg_diss = (ch4_ebb_sed_zones$zone1_Marea + ch4_ebb_sed_zones$zone2_Marea + ch4_ebb_sed_zones$zone3_Marea + ch4_ebb_sed_zones$zone4_Marea) - ch4_dissolution_budget_df$CH4_diss_molperday
)


for (i in 2:nrow(master_df_hourly)) {
  master_df_hourly$storage_diff[i] <- master_df_hourly$ch4_conc[i] - master_df_hourly$ch4_conc[i-1]
}

#Check
#ggplot(data = master_df_hourly, aes(x=DateTime, y=ch4_balance))+
#  geom_line()+
#  geom_point(data = master_df_hourly, aes(x=DateTime, y=storage_diff), colour="red", alpha = 0.1)

master_df_hourly <- master_df_hourly %>%
  mutate(year = year(DateTime), month= month(DateTime))

#Monthly budget
monthly_budget <- master_df_hourly %>%
  group_by(year, month) %>%
  summarize(total_diss = sum(total_diss), total_oxid= sum(total_oxid), total_diff_atm= sum(total_diff_atm), total_diff_sed = sum(total_diff_sed), total_ebb_sed = sum(total_ebb_sed), total_ebb_atm = sum(total_ebb_atm), ebb_neg_diss = sum(ebb_neg_diss)) 

monthly_budget <- monthly_budget %>%
  mutate(net_release = total_diff_atm + total_ebb_atm) %>%
  mutate(net_accumulation = total_diss - total_oxid - total_diff_atm + total_diff_sed) %>%
  mutate(yearmonth = sprintf("%04d-%02d", year, month)) 
#write.csv(monthly_budget, "master_df.csv", row.names=FALSE)

master_df <- monthly_budget
master_df <- master_df %>%
 mutate(total_oxid = - total_oxid, total_ebb_atm = - total_ebb_atm, total_diff_atm = - total_diff_atm)

master_df_processes <- master_df %>%
pivot_longer(cols= c("total_ebb_atm", "total_diff_atm", "ebb_neg_diss", "total_diss", "total_diff_sed", "total_oxid", "net_release", "net_accumulation"), names_to = "processes", values_to = "methane") %>%
filter(year > "2016")
master_df_processes$yearmonth <- as.Date(paste0(master_df_processes$yearmonth, "-01"), format = "%Y-%m-%d")
master_df$yearmonth <- as.Date(paste0(master_df$yearmonth, "-01"), format = "%Y-%m-%d")

master_df_processes$processes <- factor(master_df_processes$processes, 
                                         levels = c("total_diff_sed", "total_diss", "ebb_neg_diss", "total_diff_atm",
                                                     "total_ebb_atm", "total_oxid",
                                                    "net_release", "net_accumulation"))
master_df_processes$pattern_fill <- ifelse(master_df_processes$processes == "total_diss", "stripe", "none")



#Stacked barplot
budget_plot <- ggplot(data= subset(master_df_processes, processes %in% c("total_ebb_atm", "total_diff_atm", "ebb_neg_diss", "total_diff_sed", "total_oxid", "total_prod")), aes(fill=processes, y=methane, x=yearmonth)) + 
    geom_bar(position="stack", stat="identity", colour="black", linewidth = 0.3)+
    geom_bar_pattern(
    data = subset(master_df_processes, processes %in% c("total_diss")),
    aes(fill=processes, y=methane, x=yearmonth),
    pattern = "circle",
    position = "stack", 
    stat = "identity", 
    colour = "black", 
    linewidth = 0.15, 
    #aes(data = subset(master_df_processes, processes %in% c("total_diss"))),  # Direct pattern mapping
    pattern_density = 0.15,  #0.1 Adjust pattern density
    pattern_angle = 60,  # Set the angle of stripes or lines
    pattern_spacing = 0.01 #0.01
  ) + 
    scale_x_date(
    name = "Date",
    breaks = seq.Date(from = min(master_df_processes$yearmonth), to = max(master_df_processes$yearmonth), by = "6 months"),
    labels = scales::date_format("%Y %b"), # Formats as "YYYY Mon"
    limits = c(as.Date("2017-01-01"), as.Date("2020-01-01"))
  ) +
    geom_line(data=filter(master_df_processes, processes=="net_release"), aes(x=yearmonth, y=methane, color = "net_release"), linewidth = 1, group = 1, lty=2) +
    geom_line(data=filter(master_df_processes, processes=="net_accumulation"), aes(x=yearmonth, y=methane, color = "net_accumulation"), linewidth = 1, group = 1) +
    #geom_line(data=filter(master_df_processes, processes=="net_accumulation"), aes(x=yearmonth, y=methane, color = "net_accumulation"), linewidth = 1, group = 1, lty=2) +
    #scale_x_discrete(breaks = unique(master_df$yearmonth)[seq(1, length(unique(master_df$yearmonth)), by = 5)],
    #               labels = unique(master_df$yearmonth)[seq(1, length(unique(master_df$yearmonth)), by = 5)])+
    scale_fill_manual(values = c("total_ebb_atm" = "#972D1580",#"#F4B5BD",#col[1],
                                 "total_diff_atm" = "#FAD77B98",#"#9C964A", # col[9], 
                                 "ebb_neg_diss" = "#F4B5BD70", #col[4], 
                                 "total_diss" = "#F4B5BD70", 
                                 "total_diff_sed" = "#9C964A70", 
                                 "total_oxid" = "#A9A9A990", 
                                 "total_prod" = "#FAD77B"),
                      labels=c("total_ebb_atm" = expression(CH[4]~"emission via ebullition"), 
                               "total_diff_atm"= expression(CH[4]~"emission via diffusion"), 
                               "ebb_neg_diss"= expression("Sediment"~CH[4]~"ebullition"),
                               "total_diss" = expression(CH[4]~"dissolution"),
                               "total_diff_sed" = expression("Sediment"~CH[4]~"diffusion"),
                               "total_oxid" = expression(CH[4]~"oxidation"),
                               "total_prod" = expression("Oxic"~CH[4]~"production"))) + 
    scale_colour_manual(values = c("net_release" = "black", "net_accumulation" = "black"),
                        labels = c("net_release" = expression("Net"~CH[4]~"release"), "net_accumulation" = expression("Net"~CH[4]~"accumulation")),
                        guide = guide_legend(ncol = 1))+
    #scale_linetype_manual(#values = c("net_release" = "solid", "net_accumulation" = "dashed"), 
                          #guide = guide_legend(override.aes = list(linetype = c("solid"))))+
    #scale_colour_manual(values = c("net_release" = "black", "net_accumulation" = "black"),
    #                    labels = c("net_release" = "Net CH4 release", "net_accumulation" = "Net CH4 storage"),
    #                    guide = guide_legend(ncol = 1))+
    #scale_linetype_manual(#values = c("net_release" = "solid", "net_accumulation" = "dashed"), 
    #                      guide = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))+
    #scale_y_continuous(labels = function(x) abs(x), limits = c(-13000, 13000))+
    #scale_y_continuous(limits = c(-10000, 10000))+
    theme_light()+
    labs(x="Month", y="Methane (Moles)") +
    ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),    
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),    
    legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(size=18, family="Helvetica", colour="black"),
    plot.margin = margin(t = 10, r = 10, b = 25, l = 10, unit = "pt"),
    legend.position = "top",
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    #panel.grid = element_blank(),
    strip.text = element_text(face="bold", size = 12),
    legend.spacing.y = unit(0,"mm")
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5)+
  geom_text(aes(x = as.Date("2017-04-01"), y = 7000, label = "Sources"), 
              #angle = 180, 
              hjust = 1, 
              vjust = 1.5, 
              size = 10,  # Adjust text size as needed
              color = "black")+
              #fontface = "bold")+
  geom_text(aes(x = as.Date("2017-02-22"), y = -6000, label = "Sinks"), 
              #angle = 90, 
              hjust = 1, 
              vjust = 1.5, 
              size = 10,  # Adjust text size as needed
              color = "black")
              #fontface = "bold")
  budget_plot

ggsave("Plots/Figure6_monthly_budget.png",
       plot = budget_plot,
       width = 400, 
       height = 300, 
       units = "mm")


master_df_processes1 <- master_df_processes %>%
  filter(processes =="total_diff_sed")

#Stats 

master_df_stats <- master_df %>%
  mutate(diss_perc = (total_diss / total_ebb_sed)*100)

min(master_df_stats$diss_perc, na.rm=TRUE)
max(master_df_stats$diss_perc, na.rm=TRUE)


#dissolution is 8.67 - 10.84 % of ebullition

master_df_stats <- master_df %>%
  mutate(ox_perc = ((total_oxid/(total_diff_sed + total_diss))*100)) %>%
  mutate(ebb = total_ebb_sed - total_diss + total_ebb_atm)

min(master_df_stats$ox_perc, na.rm=TRUE)
max(master_df_stats$ox_perc, na.rm=TRUE)
# oxidation is removing up to 65% of dissolved methane (anywhere between 20 and 65%)

year_sum <- master_df %>%
group_by(year) %>%
summarise(ebb_yearly = sum(total_ebb_atm), diff_yearly = sum(total_diff_atm)) %>%
mutate(ratio = ebb_yearly/(ebb_yearly+diff_yearly))

#Ebullition is responsible for 62% (2017), 66% (2018), 67% (2019) of yearly emissions for the three simulation years

#########################################FIGURE 7: UNCERTAINTY PLOT########################################################################

setwd('/FCR-GLM-methane/CH4_UA/results')
#PRIOR
obs_0<- read.csv("glm3_reweight_ies.0.obs.csv")

obs_diff <- read.csv("field_data/obs_diffusion_combined.csv") %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(DateTime < as.POSIXct("2019-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"))

#diff prior 
obs_diff_prior <- obs_0 %>% 
  dplyr:: select(grep("diff", names(obs_0))) %>%
  mutate(realizations = obs_0$real_name) %>%
  select(realizations, everything())

folder_path <- getwd()
# Create a vector of file names from ftx_0 to ftx_154
file_names <- paste0("ftx_", 0:154, ".ebb_diff_fluxes.csv")

# Full file paths
full_file_paths <- file.path(folder_path, file_names)

# Read the CSV files into a list
csv_list <- lapply(full_file_paths, read.csv)

# Combine all data into one dataframe
combined_data <- do.call(rbind, csv_list)

combined_data <- combined_data %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) 

filtered_diff_data <- combined_data %>%
  filter(DateTime %in% obs_diff$DateTime) 

#Filtered ebb data is the csv files
filtered_diff_data <- filtered_diff_data %>%
  mutate(run_id = rep(c(0:154), each = 37)) %>%
  select(DateTime, CH4_ch4_atm, run_id)

filtered_diff_data$CH4_ch4_atm <- as.numeric(filtered_diff_data$CH4_ch4_atm)
filtered_diff_data$CH4_ch4_atm <- round(filtered_diff_data$CH4_ch4_atm, 5)


#Find the right csv files
matching_rows <- NULL
count <- NULL
result_df <- data.frame(row_num = integer(0), run_id = integer(0), count = integer(0))

for (i in 1:nrow(obs_diff_prior)) {
  cat("Processing iteration", i, "of", nrow(obs_diff_prior), "\n")
  diff_run_id <- as.numeric(obs_diff_prior[i, -1])
  diff_run_id <- round(diff_run_id, 5)
  for (j in 0:154) {
    run_id_filter <- filtered_diff_data %>%
      filter(run_id == j)
      matching_rows <- which(abs(run_id_filter$CH4_ch4_atm - diff_run_id)< 1e-4)
      count <- length(matching_rows)

      if (count > 30) {
        result_df <- rbind(result_df, data.frame(row_num = i, run_id = j, count = count))
       }

  }
}

run_id_summary <- result_df %>%
  group_by(run_id) %>%
  summarise(count = n(), .groups = "drop")

#POSTERIOR
obs_3<- read.csv("glm3_reweight_ies.3.obs.csv")

obs_diff <- read.csv("field_data/obs_diffusion_combined.csv") %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(DateTime < as.POSIXct("2019-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"))

#diff prior 
obs_diff_posterior <- obs_3 %>% 
  dplyr:: select(grep("diff", names(obs_3))) %>%
  mutate(realizations = obs_3$real_name) %>%
  select(realizations, everything())

folder_path <- getwd()
# Create a vector of file names from ftx_647 to ftx_898
file_names <- paste0("ftx_", 657:898, ".ebb_diff_fluxes.csv") #657

# Full file paths
full_file_paths <- file.path(folder_path, file_names)

# Read the CSV files into a list
csv_list <- lapply(full_file_paths, read.csv)

# Combine all the data into one dataframe
combined_data_post <- do.call(rbind, csv_list)

combined_data_post <- combined_data_post %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) 

filtered_diff_data_post <- combined_data_post %>%
  filter(DateTime %in% obs_diff$DateTime) 

#Filtered ebb data is the csv files
filtered_diff_data_post <- filtered_diff_data_post %>%
  mutate(run_id = rep(c(657:898), each = 37)) %>%
  select(DateTime, CH4_ch4_atm, run_id)

filtered_diff_data_post$CH4_ch4_atm <- as.numeric(filtered_diff_data_post$CH4_ch4_atm)
filtered_diff_data_post$CH4_ch4_atm <- round(filtered_diff_data_post$CH4_ch4_atm, 5)


#Find the right csv files
matching_rows <- NULL
count <- NULL
result_df_post <- data.frame(row_num = integer(0), run_id = integer(0), count = integer(0))

for (i in 1:nrow(obs_diff_posterior)) {
  cat("Processing iteration", i, "of", nrow(obs_diff_posterior), "\n")
  diff_run_id <- as.numeric(obs_diff_posterior[i, -1])
  diff_run_id <- round(diff_run_id, 5)
  for (j in 657:898) {
    run_id_filter <- filtered_diff_data_post %>%
      filter(run_id == j)
      matching_rows <- which(abs(run_id_filter$CH4_ch4_atm - diff_run_id)< 1e-4)
      count <- length(matching_rows)

      if (count > 30) {
        result_df_post <- rbind(result_df_post, data.frame(row_num = i, run_id = j, count = count))
       }

  }
}

run_id_summary_post <- result_df_post %>%
  group_by(run_id) %>%
  summarise(count = n(), .groups = "drop")


#Timeseries plot, now that csv files are identified
#prior
combined_data_final <- combined_data %>%
 mutate(run_id = rep(c(0:154), each = 24888)) %>%
 filter(run_id %in% result_df$run_id) 

combined_data_final$Year <- format(as.Date(combined_data_final$DateTime), "%Y")

combined_data_avg <- combined_data_final %>%
  mutate(Date = as.Date(DateTime)) %>%  # Extract date from timestamp
  group_by(Date, run_id) %>%                    # Group by the date
  summarise(daily_avg = mean(CH4_ch4_atm), daily_avg_ebb = mean(ebb_flux), .groups = "drop")

combined_data_avg$Year <- format(as.Date(combined_data_avg$Date), "%Y")
combined_data_avg$month <- as.integer(format(as.Date(combined_data_avg$Date), "%m"))


combined_data_avg$Date <- as.Date(combined_data_avg$Date, format="%Y-%m-%d")

combined_data_avg <- combined_data_avg %>%
  filter(Year %in% c(2017, 2018))%>%
  filter(month %in% c(03:12))

#posterior
combined_data_post_final <- combined_data_post %>%
 mutate(run_id = rep(c(657:898), each = 24888)) %>%
 filter(run_id %in% result_df_post$run_id)

combined_data_post_final$Year <- format(as.Date(combined_data_post_final$DateTime), "%Y")

combined_data_post_avg <- combined_data_post_final %>%
  mutate(Date = as.Date(DateTime)) %>%  # Extract date from timestamp
  group_by(Date, run_id) %>%                    # Group by the date
  summarise(daily_avg = mean(CH4_ch4_atm), daily_avg_ebb = mean(ebb_flux), .groups = "drop") 

combined_data_post_avg$Year <- format(as.Date(combined_data_post_avg$Date), "%Y")
combined_data_post_avg$month <- as.integer(format(as.Date(combined_data_post_avg$Date), "%m"))


combined_data_post_avg$Date <- as.Date(combined_data_post_avg$Date, format="%Y-%m-%d")

combined_data_post_avg <- combined_data_post_avg %>%
  filter(Year %in% c(2017, 2018)) %>%
  filter(month %in% c(03:12))

#obs
obs_diff$Date <- as.Date(obs_diff$DateTime, format="%Y-%m-%d")
obs_diff$Year <- format(as.Date(obs_diff$Date), "%Y")

obs_diff <- obs_diff %>%
  filter(Year %in% c(2017, 2018))


#Plot diff
intervals = 1:19/20
plot1<- ggplot2::ggplot(data=combined_data_avg, aes(x=Date, y=daily_avg))+
  ggfan::geom_fan(intervals=intervals)+
  scale_fill_gradient(name="Prior", low="#011f4b", high="#b3cde0", space="Lab")+
  #ylim(0,80)+
  ggnewscale::new_scale_fill()+
  ggfan::geom_fan(data=combined_data_post_avg, mapping=aes(x=Date, y=daily_avg), intervals=intervals, alpha=0.6)+
  scale_fill_gradient(name="Posterior", low="red", high="pink", space="Lab") +
  #guides(fill = guide_colourbar(ticks.colour = "black", ticks.linewidth=0.75))+
  geom_point(data=obs_diff, mapping=aes(x=Date, y=avg_ch4_diff, colour="Observations"), pch=10, size=1.5) +
  #geom_line(data=means_by_variable, mapping=aes(x=DateTime, y=mean_value, colour="black"))+
  #ggtitle("Prior vs. Posterior")+
  ylab(Diffusion~(mmol/m^{2}/day))+
  xlab("Date")+
  scale_x_date(
    breaks = seq(from = as.Date("2017-03-01", format="%Y-%m-%d"), 
                 to = as.Date("2018-12-31", format="%Y-%m-%d"), 
                 by = "2 months"),  # Break every 2 months based on data range
    date_labels = "%b"  # Format labels as "May 2017", "Jul 2017"
    #name = "Date"
  ) +
  scale_colour_manual(
    values = c("Observations"="black"), name="", guide = guide_legend(order = 1, override.aes = list(size = 3))
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 14, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_text(size= 16, family = "Helvetica", colour="black"),
    legend.position="left",
    #legend.position = "top",
    legend.key.size = unit(0.9, "cm"),
    #legend.box.background = element_rect(color = "black", linewidth = 0.8),
    axis.text = element_text(size=16, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    strip.text = element_text(size = 18, family = "Helvetica", colour="black")
  )+ 
  facet_wrap(~ Year, scales = "free_x")

plot1

#obs
obs_ebb <- read.csv("field_data/observed_ebullition.csv") %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(DateTime < as.POSIXct("2019-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  slice(-25)
obs_ebb$Date <- as.Date(obs_ebb$DateTime, format="%Y-%m-%d")
obs_ebb$Year <- format(as.Date(obs_ebb$Date), "%Y")

obs_ebb <- obs_ebb %>%
  filter(Year %in% c(2017, 2018))

#Plot ebb
intervals = 1:19/20
plot2<- ggplot2::ggplot(data=combined_data_avg, aes(x=Date, y=daily_avg_ebb))+
  ggfan::geom_fan(intervals=intervals)+
  scale_fill_gradient(name="Prior", low="#011f4b", high="#b3cde0", space="Lab")+  #ylim(0,80)+
  ggnewscale::new_scale_fill()+
  ggfan::geom_fan(data=combined_data_post_avg, mapping=aes(x=Date, y=daily_avg_ebb), intervals=intervals, alpha=0.6)+
  scale_fill_gradient(name="Posterior", low="red", high="pink") +
  geom_point(data=obs_ebb, mapping=aes(x=Date, y=avg_ch4_ebb, colour="Observations"), pch=10, size=1.5) +
  #geom_line(data=means_by_variable, mapping=aes(x=DateTime, y=mean_value, colour="black"))+
  #ggtitle("Prior vs. Posterior")+
  ylab(Ebullition~(mmol/m^{2}/day))+
  xlab("Date")+
  scale_x_date(
    breaks = seq(from = as.Date("2017-03-01", format="%Y-%m-%d"), 
                 to = as.Date("2018-12-31", format="%Y-%m-%d"), 
                 by = "2 months"),  # Break every 2 months based on data range
    date_labels = "%b"  # Format labels as "May 2017", "Jul 2017"
    #name = "Date"
  ) +
  scale_colour_manual(
    values = c("Observations"="black"), name="", guide = guide_legend(order = 1, override.aes = list(size = 3))
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.position = "left",
    legend.key.size = unit(0.9, "cm"),
    legend.text = ggplot2::element_text(size= 14, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_text(size= 16, family = "Helvetica", colour="black"),
    axis.text = element_text(size=16, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    strip.text = element_text(size = 18, family = "Helvetica", colour="black")
  )+ 
  facet_wrap(~ Year, scales = "free_x")

plot2

plot_arranged1 <- ggarrange(plot1, plot2, ncol=1, nrow=2, common.legend=TRUE, heights = c(10, 10), legend = "top")
plot_arranged1

#Density plots: Diffusion
#Prior
combined_data_final$month <- as.integer(format(as.Date(combined_data_final$DateTime), "%m"))

combined_data_sum <- combined_data_final %>%
  filter(Year %in% c(2017, 2018))%>%
  filter(month %in% c(03:12)) %>%
  mutate(Date = as.Date(DateTime)) %>%  # Extract date from timestamp
  group_by(run_id, Year) %>%                    # Group by the date
  summarise(yearly_sum = sum(CH4_ch4_atm/24), yearly_sum_ebb = sum(ebb_flux/24), .groups = "drop")

#convert from mmol/m2/Y to g/m2/Y
combined_data_sum <- combined_data_sum %>%
  mutate(yearly_sum = yearly_sum*(16.04/1000), yearly_sum_ebb = yearly_sum_ebb*(16.04/1000))

#Posterior
combined_data_post_final$month <- as.integer(format(as.Date(combined_data_post_final$DateTime), "%m"))

combined_data_post_sum <- combined_data_post_final %>%
  filter(Year %in% c(2017, 2018))%>%
  filter(month %in% c(03:12)) %>%
  mutate(Date = as.Date(DateTime)) %>%  # Extract date from timestamp
  group_by(run_id, Year) %>%                    # Group by the date
  summarise(yearly_sum = sum(CH4_ch4_atm/24), yearly_sum_ebb = sum(ebb_flux/24), .groups = "drop")

#convert from mmol/m2/Y to g/m2/Y
combined_data_post_sum <- combined_data_post_sum %>%
  mutate(yearly_sum = yearly_sum*(16.04/1000), yearly_sum_ebb = yearly_sum_ebb*(16.04/1000))

density_plot_post <- ggplot(combined_data_post_sum, aes(x = yearly_sum, fill=Year, colour = Year)) +
  geom_density(alpha = 0.1, linewidth=1) +  # Adjust transparency to see both distributions
  labs(x = Yearly~diffusive~flux~(g/m^{2}/yr),
       y = "Posterior probability density") +
  theme_minimal() +
  scale_fill_manual(values = c("#FD6467", "#5B1A18")) +  # Adjust colors
  scale_color_manual(values = c("#FD6467", "#5B1A18"))+
  ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 14, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_text(size= 16, family = "Helvetica", colour="black"),
    legend.position = c(0.85, 0.85),
    legend.box.background = element_rect(color = "black", linewidth = 0.8),
    axis.text = element_text(size=16, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    strip.text = element_text(size = 18, family = "Helvetica", colour="black")
  )
density_plot_post

#Density plots: Ebullition
#Prior

#Posterior
density_plot_post_ebb <- ggplot(combined_data_post_sum, aes(x = yearly_sum_ebb, colour=Year, fill = Year)) +
  geom_density(alpha = 0.1, linewidth=1) +  # Adjust transparency to see both distributions
  labs(x = Yearly~ebullitive~flux~(g/m^{2}/yr),
       y = "Posterior probability density") +
  theme_minimal() +
  scale_fill_manual(values = c("#FD6467", "#5B1A18")) +  # Adjust colors
  scale_color_manual(values = c("#FD6467", "#5B1A18"))+
  ggplot2::theme_light() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
    legend.text = ggplot2::element_text(size= 14, family = "Helvetica", colour="black"), 
    legend.title = ggplot2::element_text(size= 16, family = "Helvetica", colour="black"),
    legend.position = c(0.85, 0.85),
    legend.box.background = element_rect(color = "black", linewidth = 0.8),
    axis.text = element_text(size=16, family="Helvetica", colour="black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill="gray88"),
    axis.ticks.x = element_line(colour="black", linewidth = 1), 
    axis.ticks.y = element_line(colour="black", linewidth = 1), 
    axis.ticks.length = unit(2, "mm"),
    strip.text = element_text(size = 18, family = "Helvetica", colour="black")
  )
density_plot_post_ebb

plot_arranged2 <- ggarrange(NULL, density_plot_post, density_plot_post_ebb, ncol=1, nrow=3, heights=c(1.5, 11, 11)) 
plot_arranged2


plot_arranged <- ggarrange(plot_arranged1, plot_arranged2, ncol=2, nrow=1, widths = c(2, 1))
plot_arranged
ggsave("Plots/Figure7_uncertainty.png",
       plot = plot_arranged,
       dpi=500,
       width = 400, 
       height = 300, 
       units = "mm")