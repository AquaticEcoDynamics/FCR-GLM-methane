library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(ggpubr)

setwd("../FCR-GLM-methane/CH4_UA/results/")

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

# combine all the data into one data frame
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

#Match csvs that include all timesteps to UA result csv
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
# Create a vector of file names from ftx_0 to ftx_154
file_names <- paste0("ftx_", 657:898, ".ebb_diff_fluxes.csv") #657

# Full file paths
full_file_paths <- file.path(folder_path, file_names)

# Read the CSV files into a list
csv_list <- lapply(full_file_paths, read.csv)

# If you want to combine all the data into one data frame, use do.call with rbind
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

#Match csvs that include all timesteps to UA result csv
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


#Timeseries plot
#posterior diffusion
combined_data_final <- combined_data %>%
 mutate(run_id = rep(c(0:154), each = 24888)) %>%
 filter(run_id %in% result_df$run_id) 

combined_data_final$Year <- format(as.Date(combined_data_final$DateTime), "%Y")

combined_data_avg <- combined_data_final %>%
  mutate(Date = as.Date(DateTime)) %>%  
  group_by(Date, run_id) %>%                    
  summarise(daily_avg = mean(CH4_ch4_atm), daily_avg_ebb = mean(ebb_flux), .groups = "drop")

combined_data_avg$Year <- format(as.Date(combined_data_avg$Date), "%Y")
combined_data_avg$month <- as.integer(format(as.Date(combined_data_avg$Date), "%m"))


combined_data_avg$Date <- as.Date(combined_data_avg$Date, format="%Y-%m-%d")

combined_data_avg <- combined_data_avg %>%
  filter(Year %in% c(2017, 2018))%>%
  filter(month %in% c(03:12))

#posterior ebullition
combined_data_post_final <- combined_data_post %>%
 mutate(run_id = rep(c(657:898), each = 24888)) %>%
 filter(run_id %in% result_df_post$run_id)

combined_data_post_final$Year <- format(as.Date(combined_data_post_final$DateTime), "%Y")

combined_data_post_avg <- combined_data_post_final %>%
  mutate(Date = as.Date(DateTime)) %>%  
  group_by(Date, run_id) %>%                    
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
  ylim(0,150)+
  ggnewscale::new_scale_fill()+
  ggfan::geom_fan(data=combined_data_post_avg, mapping=aes(x=Date, y=daily_avg), intervals=intervals, alpha=0.6)+
  scale_fill_gradient(name="Posterior", low="red", high="pink", space="Lab") +
  geom_point(data=obs_diff, mapping=aes(x=Date, y=avg_ch4_diff, colour="Observations"), pch=10, size=1.5) +
  ylab(Diffusion~(mmol/m^{2}/day))+
  xlab("Date")+
  scale_x_date(
    breaks = seq(from = as.Date("2017-03-01", format="%Y-%m-%d"), 
                 to = as.Date("2018-12-31", format="%Y-%m-%d"), 
                 by = "2 months"),  
    date_labels = "%b"  
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
    legend.key.size = unit(0.9, "cm"),
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
  ylab(Ebullition~(mmol/m^{2}/day))+
  xlab("Date")+
  scale_x_date(
    breaks = seq(from = as.Date("2017-03-01", format="%Y-%m-%d"), 
                 to = as.Date("2018-12-31", format="%Y-%m-%d"), 
                 by = "2 months"),  
    date_labels = "%b"  
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

#Data manipulation for density plots
combined_data_post_final$month <- as.integer(format(as.Date(combined_data_post_final$DateTime), "%m"))

combined_data_post_sum <- combined_data_post_final %>%
  filter(Year %in% c(2017, 2018))%>%
  filter(month %in% c(03:12)) %>%
  mutate(Date = as.Date(DateTime)) %>%  
  group_by(run_id, Year) %>%                    
  summarise(yearly_sum = sum(CH4_ch4_atm/24), yearly_sum_ebb = sum(ebb_flux/24), .groups = "drop")

#convert from mmol/m2/Y to g/m2/Y
combined_data_post_sum <- combined_data_post_sum %>%
  mutate(yearly_sum = yearly_sum*(16.04/1000), yearly_sum_ebb = yearly_sum_ebb*(16.04/1000))

#Density plots: Diffusion
#Posterior
density_plot_post <- ggplot(combined_data_post_sum, aes(x = yearly_sum, fill=Year, colour = Year)) +
  geom_density(alpha = 0.1, linewidth=1) +  
  labs(x = Yearly~diffusive~flux~(g/m^{2}/yr),
       y = "Posterior probability density") +
  ylim(0, 0.35)+
  theme_minimal() +
  scale_fill_manual(values = c("#FD6467", "#5B1A18")) +  
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

#Posterior
density_plot_post_ebb <- ggplot(combined_data_post_sum, aes(x = yearly_sum_ebb, colour=Year, fill = Year)) +
  geom_density(alpha = 0.1, linewidth=1) +  
  labs(x = Yearly~ebullitive~flux~(g/m^{2}/yr),
       y = "Posterior probability density") +
  ylim(0, 0.35)+
  theme_minimal() +
  scale_fill_manual(values = c("#FD6467", "#5B1A18")) +  
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

ggsave("/Users/22668839/Desktop/Publications/Methane_model/Plots/first_draft/Figure7_updated.png",
       plot = plot_arranged,
       dpi=500,
       width = 400, 
       height = 300,
       units = "mm")


#Calculations
combined_data_avg_stats <- combined_data_avg %>%
 group_by(Year, run_id) %>%
 summarise(total_diff = sum(daily_avg, na.rm=TRUE), total_ebb = sum(daily_avg_ebb), avg_ebb = mean(daily_avg, na.rm=TRUE), avg_diff = mean(daily_avg_ebb, na.rm=TRUE), .groups = 'drop')

#ebb
 flux_summary_by_year <- combined_data_avg_stats %>%
  group_by(Year) %>%
  summarise(
    mean_flux = mean(avg_ebb),
    ci_lower = quantile(avg_ebb, 0.05),
    ci_upper = quantile(avg_ebb, 0.95),
    lower_pct = 100 * (mean_flux - quantile(avg_ebb, 0.05)) / mean_flux,
    upper_pct = 100 * (quantile(avg_ebb, 0.95) - mean_flux) / mean_flux,
    .groups = 'drop'
  )

mean_flux <- mean(combined_data_avg_stats$avg_ebb)
ci_flux <- quantile(combined_data_avg_stats$avg_ebb, c(0.05, 0.95))
lower_pct <- 100 * (mean_flux - ci_flux[1]) / mean_flux
upper_pct <- 100 * (ci_flux[2] - mean_flux) / mean_flux

print(lower_pct)
print(upper_pct)
