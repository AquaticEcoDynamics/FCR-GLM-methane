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

#SEDIMENT MODEL PLOTS

variables <- c("CH4_poc_sed_Z", "CH4_poc_resp_Z", "CH4_poc_set_Z", "CH4_ch4s_prod_Z", "CH4_ch4_sed_Z", "CH4_ch4s_diff_Z", "CH4_ch4s_oxid_Z", "CH4_ch4s_ebb_Z")
# I deleted "CH4_ch4_crit_Z"
y_labels <- list(
  CH4_poc_sed_Z = expression(POC~concentration~(mmol~m^{-2})),
  CH4_poc_resp_Z = expression(POC~respiration~(mmol~m^{-2}~d^{-1})),
  CH4_poc_set_Z = expression(POC~settling~(mmol~m^{-2}~d^{-1})),
  CH4_ch4s_prod_Z = expression(CH[4]~production~(mmol~m^{-2}~d^{-1})),
  CH4_ch4_sed_Z = expression(CH[4]~concentration~(mmol~m^{-2})),
  CH4_ch4s_diff_Z = expression(CH[4]~diffusive~flux~(mmol~m^{-2}~d^{-1})),
  CH4_ch4s_oxid_Z = expression(CH[4]~oxidation~(mmol~m^{-2}~d^{-1})),
  CH4_ch4s_ebb_Z = expression(CH[4]~ebullition~(mmol~m^{-2}~d^{-1}))
)

#Empty list
plot_list <- list()

# Loop through the variables
for (var in variables) {
  
  # Get var from .nc
  variable_data <- ncvar_get(output, var)
  
  #create data frame
  variable_zones <- as.data.frame(t(variable_data)) %>%
    mutate(Date = hourly_vector) %>%
    mutate(Date = as.POSIXct(Date, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) %>%
    select(-5) %>%
    rename(zone_1 = V1, zone_2 = V2, zone_3 = V3, zone_4 = V4)
  
  # Create the plot 
  plot <- ggplot2::ggplot(data=variable_zones, aes(x=Date, y=zone_1, colour="zone 1")) +
    geom_line() +
    geom_line(data=variable_zones, aes(x=Date, y=zone_2, colour="zone 2")) +
    geom_line(data=variable_zones, aes(x=Date, y=zone_3, colour="zone 3")) +
    geom_line(data=variable_zones, aes(x=Date, y=zone_4, colour="zone 4")) +
    ylab(y_labels[[var]]) +
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
  
  plot_list[[var]] <- plot
}



sed_plots <- patchwork::wrap_plots(plot_list, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", plot.tag = element_text(
        size = 22,                  
        face = "bold",              
        family = "Helvetica",       
        color = "black"             
      )) &
  plot_annotation(tag_levels = 'a', tag_prefix = "", tag_suffix = ")")
sed_plots


ggsave("Plots/FigureS1_sediment_diag.png",
       plot = sed_plots,
       width = 400, 
       height = 500,
       units = "mm")

#######################################################################################################################################
#POC settling validation

obs_fluxes <- read.csv('field_data/FluxSummary.csv') %>%
  mutate(Date = as.Date(Date, format="%Y-%m-%d")) %>%
  filter(Reservoir=="FCR")

#g/m2/d to mmol/m2/d
obs_fluxes$TOCflux_mmolm2d <- ((obs_fluxes$TOCFlux_gm2d)/12)*1000

obs_fluxes_filtered<- obs_fluxes %>%
  mutate(Date = as.POSIXct(paste(Date, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(year(Date) %in% c(2018, 2019))

  
#create data frame
POC_set_mod <- ncvar_get(output, "CH4_poc_set_Z")
POC_set_zones <- as.data.frame(t(POC_set_mod)) %>%
    mutate(Date = hourly_vector) %>%
    mutate(Date = as.POSIXct(Date, "%Y-%m-%d %H:%M:%S")) %>%
    select(-5) %>%
    rename(zone_1 = V1, zone_2 = V2, zone_3 = V3, zone_4 = V4) %>%
    filter(year(Date) %in% c(2018, 2019))

poc_set_p <- ggplot() +
  # Modelled lines
  geom_line(data = POC_set_zones, aes(x = Date, y = -zone_1, colour = "zone 1")) +
  geom_line(data = POC_set_zones, aes(x = Date, y = -zone_3, colour = "zone 3")) +

  # Observed points
  geom_point(data = filter(obs_fluxes_filtered, Depth_m == "4"),
             aes(x = Date, y = TOCflux_mmolm2d, fill = "zone 3"),
            colour="black", pch=21, size = 4) +
  geom_point(data = filter(obs_fluxes_filtered, Depth_m == "8"),
             aes(x = Date, y = TOCflux_mmolm2d, fill = "zone 1"),
            colour="black", pch=21, size = 4) +

  # Axis label
  ylab(expression(POC~settling~(mmol~m^{-2}~d^{-1}))) +

  # Manual scales
  scale_colour_manual(name = "Modelled", values = c("zone 1" = "#1b9e77", "zone 3" = "#d95f02")) +
  scale_fill_manual(name = "Observed", values = c("zone 1" = "#1b9e77", "zone 3" = "#d95f02")) +
  
  # Theme
  theme_light() +
  ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm")
  )
  poc_set_p
ggsave("Plots/FigureS2_POC_set.png",
       plot = poc_set_p,
       width = 300, 
       height = 200, 
       units = "mm")



#Observed diffusion flux data 2017

diff_zones <- read.csv("field_data/EDI_DATA_EBU_DIFF_DEPTH.csv") %>%
  mutate(Date = as.POSIXct(diff_zones$DateTime,format="%Y-%m-%d %H:%M")) %>%
  mutate(Diff_rate_mmol = Diff_rate / 16.04) %>%
  mutate(zone = case_when(
      Depth_m >= 6.3 & Depth_m < 10  ~ "Zone 1",
      Depth_m >= 4.3 & Depth_m < 6.3 ~ "Zone 2",
      Depth_m >= 2.3 & Depth_m < 4.3 ~ "Zone 3",
      Depth_m <= 2.3             ~ "Zone 4"
  )) %>%
  group_by(zone, Date) %>%
  summarise(avg_per_zone = mean(Diff_rate_mmol), max_per_zone = max(Diff_rate_mmol), min_per_zone = min(Diff_rate_mmol)) %>%
  na.omit() %>%
  filter(Date > "2016-12-31")


diff_zone <- ggplot(data=diff_zones, aes(x=Date, ymin=min_per_zone, ymax=max_per_zone, fill=zone))+
geom_ribbon(alpha = 0.4) + 
geom_line(data=diff_zones, aes(x=Date, y=avg_per_zone, colour=zone), linewidth = 1)+
ylab(expression(CH[4]~diffusive~flux~(mmol~m^{-2}~d^{-1}))) + 
scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 months")+
theme_light()+
ggplot2::theme(
    legend.spacing = unit(-6, "pt"),
    plot.title = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size= 12),
    axis.title.x = ggplot2::element_text(size= 12),
    legend.text = ggplot2::element_text(size= 12), 
    legend.title=element_blank(),
    axis.text = element_text(size=12),
    legend.position = "top",
    panel.grid = element_blank(),
    strip.text = element_text(face="bold", size = 12),
    plot.margin = margin(t = 10, r = 35, b = 10, l = 10, unit = "pt"),
    legend.spacing.y = unit(0,"mm")
  ) 
diff_zone

ggsave("/Users/Kamilla/Compile_old_GLM1602/Bubbles/meeting_Cayelan/diff_transect.png",
       plot = diff_transect,
       width = 400, 
       height = 300,
       units = "mm")

#Observed diffusion flux data 2018 onwards UGGA

diff_dataset <- read.csv("field_data/UGGA_2018_2023.csv") %>%
  mutate(CH4_diff_mmolm2d = CH4Flux_umolCm2s * 86.4) %>%
  filter(as.Date(Date, format="%Y-%m-%d") <= "2019-01-01") %>%
  group_by(Site, Date) %>%
  summarise(avg_per_site = mean(CH4_diff_mmolm2d), max_per_site = max(CH4_diff_mmolm2d), min_per_site = min(CH4_diff_mmolm2d))
diff_dataset$Date <- as.Date(diff_dataset$Date, format="%Y-%m-%d")
diff_dataset$Year <- format(diff_dataset$Date, "%Y")

RColorBrewer::brewer.pal(5, "Set1")
# [1] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
diff_transect <- ggplot(data=diff_dataset, aes(x=Date, ymin=min_per_site, ymax=max_per_site, fill=as.factor(Site)))+
geom_ribbon(alpha = 0.4) + 
geom_line(data=diff_dataset, aes(x=Date, y=avg_per_site, colour=as.factor(Site)), linewidth = 1)+
scale_fill_manual(values = c("10" = "#E41A1C", "20" = "#377EB8", "30" = "#4DAF4A", "45" = "#984EA3", "50" = "#FF7F00")) +
scale_colour_manual(values = c("10" = "#E41A1C", "20" = "#377EB8", "30" = "#4DAF4A", "45" = "#984EA3", "50" = "#FF7F00"))+
ylab(expression(CH[4]~diffusive~flux~(mmol~m^{-2}~d^{-1}))) + 
scale_x_date(date_labels = "%b", date_breaks = "1 months")+
labs(
    fill = "Site",         
    colour = "Site"        
  ) +
facet_wrap(~ Year, scales = "free_x", ncol = 1) + 
theme_light()+
ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 18, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm")
  )
diff_transect

ggsave("Plots/FigureS10_Diff_flux.png",
       plot = diff_transect,
       width = 300, 
       height = 200,
       units = "mm")

#Observed ebullition data
ebb_data <- read.csv("field_data/EDI_DATA_EBU_DIFF_DEPTH.csv") %>%
mutate(Date = as.POSIXct(ebb_data$DateTime,format="%Y-%m-%d %H:%M:%S")) %>%
mutate(Ebb_rate_mmol = Ebu_rate / 16.04) %>%
mutate(zone = case_when(
    Depth_m >= 6.3 & Depth_m < 10  ~ "Zone 1",
    Depth_m >= 4.3 & Depth_m < 6.3 ~ "Zone 2",
    Depth_m >= 2.3 & Depth_m < 4.3 ~ "Zone 3",
    Depth_m <= 2.3             ~ "Zone 4"
  )) %>%
  group_by(zone, Date) %>%
  summarise(avg_per_zone = mean(Ebb_rate_mmol), max_per_zone = max(Ebb_rate_mmol), min_per_zone = min(Ebb_rate_mmol)) %>%
  na.omit() 


ebb_data$Year <- format(ebb_data$Date, "%Y")
ebb_data <- ebb_data %>%
  filter(Year != "2016") 

ebb_zones <- ggplot(data=ebb_data, aes(x=Date, ymin=min_per_zone, ymax=max_per_zone, fill=zone))+
geom_ribbon(alpha = 0.4) + 
geom_line(data=ebb_data, aes(x=Date, y=avg_per_zone, colour=zone), linewidth = 1)+
ylab(expression(CH[4]~ebullition~(mmol~m^{-2}~d^{-1})))+
facet_wrap(~ Year, scales = "free_x", ncol = 1) + 
scale_x_datetime(date_labels = "%b", date_breaks = "1 months")+
theme_light()+
ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 18, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm")
  )
ebb_zones

ggsave("Plots/FigureS11_ebb_flux_zones.png",
       plot = ebb_zones,
       width = 400, 
       height = 300,
       units = "mm")

####################################################NUTRIENTS#######################################################
#Chlorophyll a
#Observed chla
obs_chla <-read.csv('field_data/CleanedObsChla.csv') %>%
  rename(PHY_tchla = PHY_TCHLA)
obs_chla$DateTime <- as.Date(obs_chla$DateTime, format="%Y-%m-%d")
obs_chla$DateTime <- as.POSIXct(paste(obs_chla$DateTime, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")
depths<- unique(obs_chla$Depth)

chla <- get_var(nc_file, "PHY_tchla", reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("PHY_tchla_"), names_to="Depth", names_prefix="PHY_tchla_", values_to = "PHY_tchla") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))) 

chla_merge <- merge(obs_chla, chla, by=c("DateTime","Depth")) %>%
  dplyr::rename(obs_chla = PHY_tchla.x, mod_chla = PHY_tchla.y) 

plot <- vector('list')

for(i in 1:length(depths)){
  
  
  plot[[i]] <- chla_merge %>%
    filter(Depth == depths[i]) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = obs_chla, fill="Observed")) +
    ggplot2::geom_point(pch=21, size=2, colour="black")+
    geom_line(data=filter(chla, Depth==depths[i]), aes(x=DateTime, y=PHY_tchla, colour="Modelled"))+
    ggplot2::ggtitle(paste("Depth:", depths[i], "m"))+    
    ylim(0, 25)+
    xlab("Date")+
    ylab(expression(Chl-a~(mu*g/L))) +  
    ggplot2::scale_colour_manual(name="", values=c("Modelled"="#1b9e77")) + #, guide=guide_legend(override.aes=list(linetype=c(NA, 1), shape=c(10, NA))))+
    ggplot2::scale_fill_manual(name = "", values=c("Observed"="#1b9e77"))+
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm")
  )
  
  
}
plot

combinedPlot <- patchwork::wrap_plots(plot, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot
ggsave("Plots/FigureS4_Chla.png",
       plot = combinedPlot,
       width = 400, 
       height = 400,
       units = "mm")

#DO
#Observed DO
obs_oxy <-read.csv('field_data/CleanedObsOxy.csv') 
obs_oxy$DateTime <- as.Date(obs_oxy$DateTime, format="%Y-%m-%d")
obs_oxy$DateTime <- as.POSIXct(paste(obs_oxy$DateTime, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")
depths<- unique(obs_oxy$Depth)
depths <- depths[-11]

oxy_mod <- get_var(nc_file, "OXY_oxy", reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))) 

oxy_merge <- merge(obs_oxy, oxy_mod, by=c("DateTime","Depth")) %>%
  dplyr::rename(obs_oxy = OXY_oxy.x, mod_oxy = OXY_oxy.y) 

plot <- vector('list')

for(i in 1:length(depths)){
  
  
  plot[[i]] <- oxy_merge %>%
    filter(Depth == depths[i]) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = obs_oxy, fill="Observed")) +
    ggplot2::geom_point(pch=21, size=2, colour="black")+
    geom_line(data=filter(oxy_mod, Depth==depths[i]), aes(x=DateTime, y=OXY_oxy, colour="Modelled"))+
    ggplot2::ggtitle(paste("Depth:", depths[i], "m"))+    
    ylim(0, 450)+
    xlab("Date")+
    ylab(expression(DO~conc.~(mmol~m^{-3})))+
    ggplot2::scale_colour_manual(name="", values=c("Modelled"="violetred4")) + #, guide=guide_legend(override.aes=list(linetype=c(NA, 1), shape=c(10, NA))))+
    ggplot2::scale_fill_manual(name = "", values=c("Observed"="violetred4"))+
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm")
  )
  
  
}
plot

combinedPlot <- patchwork::wrap_plots(plot, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot
ggsave("Plots/FigureS3_DO.png",
       plot = combinedPlot,
       width = 400, 
       height = 400,
       units = "mm")

#Observations
depths_obs <- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)
obs_nutrients <-read.csv('field_data/field_chem_2DOCpools.csv') 
obs_nutrients$DateTime <- as.Date(obs_nutrients$DateTime, format="%Y-%m-%d")
obs_nutrients$DateTime <- as.POSIXct(paste(obs_nutrients$DateTime, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")

#NIT_nit
new_nitnit <- get_var(nc_file, "NIT_nit", reference="surface", z_out=depths_obs) %>%
  pivot_longer(cols=starts_with("NIT_nit_"), names_to="Depth", names_prefix="NIT_nit_", values_to = "NIT_nit") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))) 

merge <- merge(obs_nutrients, new_nitnit, by=c("DateTime", "Depth")) %>%
  dplyr::rename(obs_nitnit = NIT_nit.x, mod_nitnit = NIT_nit.y) 

#NIT_amm
new_NIT_amm <- get_var(nc_file, "NIT_amm", reference="surface", z_out=depths_obs) %>%
  pivot_longer(cols=starts_with("NIT_amm_"), names_to="Depth", names_prefix="NIT_amm_", values_to = "NIT_amm") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))) 

merge <- merge(merge, new_NIT_amm, by=c("DateTime", "Depth")) %>%
  dplyr::rename(obs_nitamm = NIT_amm.x, mod_nitamm = NIT_amm.y) 

#PHS_frp
new_PHS_frp <- get_var(nc_file, "PHS_frp", reference="surface", z_out=depths_obs) %>%
  pivot_longer(cols=starts_with("PHS_frp_"), names_to="Depth", names_prefix="PHS_frp_", values_to = "PHS_frp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))) 

merge <- merge(merge, new_PHS_frp, by=c("DateTime", "Depth")) %>%
  dplyr::rename(obs_phsfrp = PHS_frp.x, mod_phsfrp = PHS_frp.y) 

#OGM_doc
new_OGM_doc <- get_var(nc_file, "OGM_doc", reference="surface", z_out=depths_obs) %>%
  pivot_longer(cols=starts_with("OGM_doc_"), names_to="Depth", names_prefix="OGM_doc_", values_to = "OGM_doc") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))) 

merge <- merge(merge, new_OGM_doc, by=c("DateTime", "Depth")) %>%
  dplyr::rename(obs_ogmdoc = OGM_doc.x, mod_ogmdoc = OGM_doc.y) 

#OGM_docr
new_OGM_docr <- get_var(nc_file, "OGM_docr", reference="surface", z_out=depths_obs) %>%
  pivot_longer(cols=starts_with("OGM_docr_"), names_to="Depth", names_prefix="OGM_docr_", values_to = "OGM_docr") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))) 

merge <- merge(merge, new_OGM_docr, by=c("DateTime", "Depth")) %>%
  dplyr::rename(obs_ogmdocr = OGM_docr.x, mod_ogmdocr = OGM_docr.y) 


#NIT_nit plot
plot <- vector('list')

for(i in 1:length(depths_obs)){
  
  
  plot[[i]] <- merge %>%
    filter(Depth == depths_obs[i]) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = obs_nitnit, fill="Observed")) +
    ggplot2::geom_point(pch=21, size=2, colour="black")+
    geom_line(data=filter(merge, Depth==depths_obs[i]), aes(x=DateTime, y=mod_nitnit, colour="Modelled"))+
    ggplot2::ggtitle(paste("Depth:", depths_obs[i], "m"))+   
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 years")+ 
    ylim(0, 2.5)+
    xlab("Date")+
    ylab(expression(Nitrate~conc.~(mmol~m^{-3})))+
    ggplot2::scale_colour_manual(name="", values=c("Modelled"="tomato3")) + #, guide=guide_legend(override.aes=list(linetype=c(NA, 1), shape=c(10, NA))))+
    ggplot2::scale_fill_manual(name = "", values=c("Observed"="tomato3"))+
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm"),
      plot.margin = margin(10, 20, 10, 10)
  )  
}
plot

combinedPlot1 <- patchwork::wrap_plots(plot, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot1
ggsave("Plots/FigureS5_Nitrate.png",
       plot = combinedPlot1,
       width = 400, 
       height = 400,
       units = "mm")

#NIT_amm plot

plot <- vector('list')

for(i in 1:length(depths_obs)){
  
  
  plot[[i]] <- merge %>%
    filter(Depth == depths_obs[i]) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = obs_nitamm, fill="Observed")) +
    ggplot2::geom_point(pch=21, size=2, colour="black")+
    geom_line(data=filter(merge, Depth==depths_obs[i]), aes(x=DateTime, y=mod_nitamm, colour="Modelled"))+
    ggplot2::ggtitle(paste("Depth:", depths_obs[i], "m"))+   
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 years")+ 
    ylim(0, 170)+
    xlab("Date")+
    ylab(expression(Ammonium~conc.~(mmol~m^{-3})))+
    ggplot2::scale_colour_manual(name="", values=c("Modelled"="mediumpurple2")) + #, guide=guide_legend(override.aes=list(linetype=c(NA, 1), shape=c(10, NA))))+
    ggplot2::scale_fill_manual(name = "", values=c("Observed"="mediumpurple2"))+
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm"),
      plot.margin = margin(10, 20, 10, 10)
  )  
  
  
}
plot

combinedPlot2 <- patchwork::wrap_plots(plot, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot2
ggsave("Plots/FigureS6_Ammonium.png",
       plot = combinedPlot2,
       width = 400, 
       height = 400,
       units = "mm")


#PHS_frp plot
plot <- vector('list')

for(i in 1:length(depths_obs)){
  
  
  plot[[i]] <- merge %>%
    filter(Depth == depths_obs[i]) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = obs_phsfrp, fill="Observed")) +
    ggplot2::geom_point(pch=21, size=2, colour="black")+
    geom_line(data=filter(merge, Depth==depths_obs[i]), aes(x=DateTime, y=mod_phsfrp, colour="Modelled"))+
    ggplot2::ggtitle(paste("Depth:", depths_obs[i], "m"))+    
    ylim(0, 0.3)+
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 years")+
    xlab("Date")+
    ylab(expression(Phosphate~conc.~(mmol~m^{-3})))+
    ggplot2::scale_colour_manual(name="", values=c("Modelled"="steelblue3")) + #, guide=guide_legend(override.aes=list(linetype=c(NA, 1), shape=c(10, NA))))+
    ggplot2::scale_fill_manual(name = "", values=c("Observed"="steelblue3"))+
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm"),
      plot.margin = margin(10, 20, 10, 10)
  )  
  
  
}
plot

combinedPlot3 <- patchwork::wrap_plots(plot, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot3
ggsave("Plots/FigureS7_Phosphate.png",
       plot = combinedPlot3,
       width = 400, 
       height = 400,
       units = "mm")


#OGM_doc plot
plot <- vector('list')

for(i in 1:length(depths_obs)){
  
  
  plot[[i]] <- merge %>%
    filter(Depth == depths_obs[i]) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = obs_ogmdoc, fill="Observed")) +
    ggplot2::geom_point(pch=21, size=2, colour="black")+
    geom_line(data=filter(merge, Depth==depths_obs[i]), aes(x=DateTime, y=mod_ogmdoc, colour="Modelled"))+
    ggplot2::ggtitle(paste("Depth:", depths_obs[i], "m"))+   
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 years")+ 
    ylim(0, 60)+
    xlab("Date")+
    ylab(expression(Diss.~OC~conc.~(mmol~m^{-3})))+
    ggplot2::scale_colour_manual(name="", values=c("Modelled"="salmon4")) + #, guide=guide_legend(override.aes=list(linetype=c(NA, 1), shape=c(10, NA))))+
    ggplot2::scale_fill_manual(name = "", values=c("Observed"="salmon4"))+
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm"),
      plot.margin = margin(10, 20, 10, 10)
  )  
  
  
}
plot

combinedPlot4 <- patchwork::wrap_plots(plot, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot4
ggsave("Plots/FigureS8_DOC.png",
       plot = combinedPlot4,
       width = 400, 
       height = 400,
       units = "mm")

#OGM_docr plot
plot <- vector('list')

for(i in 1:length(depths_obs)){
  
  
  plot[[i]] <- merge %>%
    filter(Depth == depths_obs[i]) %>%
    ggplot2::ggplot(ggplot2::aes(x = DateTime, y = obs_ogmdocr, fill="Observed")) +
    ggplot2::geom_point(pch=21, size=2, colour="black")+
    geom_line(data=filter(merge, Depth==depths_obs[i]), aes(x=DateTime, y=mod_ogmdocr, colour="Modelled"))+
    ggplot2::ggtitle(paste("Depth:", depths_obs[i], "m"))+   
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 years")+ 
    ylim(0, 500)+
    xlab("Date")+
    ylab(expression(Diss.~OC~(ref)~conc.~(mmol~m^{-3})))+
    ggplot2::scale_colour_manual(name="", values=c("Modelled"="darkorange3")) + #, guide=guide_legend(override.aes=list(linetype=c(NA, 1), shape=c(10, NA))))+
    ggplot2::scale_fill_manual(name = "", values=c("Observed"="darkorange3"))+
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18),
      axis.title.x = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      axis.title.y = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"),
      legend.text = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      axis.text = element_text(size=18, family="Helvetica", colour="black"),
      panel.grid = element_blank(),
      legend.title = ggplot2::element_text(size= 18, family = "Helvetica", colour="black"), 
      strip.text = element_text(size = 12, colour="black"),
      strip.background = element_rect(fill="gray88"),
      axis.ticks.x = element_line(colour="black", linewidth = 1), 
      axis.ticks.y = element_line(colour="black", linewidth = 1), 
      axis.ticks.length = unit(2, "mm"),
      plot.margin = margin(10, 20, 10, 10)
  )  
  
  
}
plot

combinedPlot5 <- patchwork::wrap_plots(plot, ncol = 2) +
  patchwork::plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combinedPlot5
ggsave("Plots/FigureS9_DOCr.png",
       plot = combinedPlot5,
       width = 400, 
       height = 400,
       units = "mm")


#Oxic methane production
#Reran the model with setting ch4_oxic_prod to 0.05 in the aed2_4zones.nml file to swicth on oxic methane production
#then read in the new output.nc file below
#Setting working directory
setwd('.../FCR-GLM-methane/FCR_model_setup/')
sim_folder <- getwd()
output_OP <- nc_open("output/output.nc")
nc_file_OP <- file.path(sim_folder, 'output/output.nc')

#Colours
library(wesanderson)
mycolours <- wes_palette(n=5, name="Moonrise3")

#Simulation period
start_date <- as.POSIXct("2017-02-27 01:00:00", tz = "Etc/GMT--8") 
end_date <- as.POSIXct("2019-12-31 00:00:00", tz = "Etc/GMT--8") 

hourly_vector <- seq(from = start_date, to = end_date, by = "hour")

var="CH4_ch4"
obs_methane<-read.csv('field_data/observed_ch4.csv') %>%
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) 

depths<- unique(obs_methane$Depth)

new_ch4 <- get_var(nc_file_OP, "CH4_ch4", reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("CH4_ch4_"), names_to="Depth", names_prefix="CH4_ch4_", values_to = "CH4_ch4") %>%
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"))

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

ggsave("Plots/FigureS12_CH4_oxicProd.png",
       plot = combinedPlot,
       width = 400, 
       height = 300,
       units = "mm")

#Diffusion
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

combined_op <- diff_transect + diff_transect2 + diff_transect3 + plot_layout(ncol = 2)
combined_op
ggsave("Plots/FigureS13_diff_flux_oxicProd.png",
       plot = combined_op,
       width = 400, 
       height = 400,
       units = "mm")






