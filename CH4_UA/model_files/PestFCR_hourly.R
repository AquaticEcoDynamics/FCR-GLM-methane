#Loading packages

library(dplyr)
library(remotes)
#library(GLMr)
library(glmtools)
library(tidyr)
library(rLakeAnalyzer)
library(base)
library(utils)
library(ncdf4)
library(reshape2)
print("i")
library(here)

#Set working directory
local_path<- here::here()
print("b")
setwd(local_path)
sim_folder <- local_path
nc_file <- file.path(sim_folder, 'output/output.nc')
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)

#obstemp<-read.csv('field_data/new_observed_temp.csv')
#  obstemp <- obstemp %>%
#  mutate(DateTime_H = as.POSIXct(DateTime_H, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) %>%
#  rename(Depth = depth) %>%
#  select(DateTime_H, Depth, temp) %>%
#  rename(DateTime = DateTime_H)
  

#modtemp <- get_temp(nc_file, reference="surface", z_out=depths) %>%
#  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") 

#watertemp<-merge(modtemp, obstemp, by=c("DateTime","Depth")) %>%
#  dplyr::rename(modtemp = temp.x, obstemp = temp.y)


#match <- vector('list')

#for(i in 1:length(unique(watertemp$Depth))){


#  match[[i]] <- watertemp %>%
#    dplyr::filter(Depth == depths[i])
#  write.csv(round(match[[i]]$modtemp, 6), paste0(  #file path for saving csv files, should be set to the working directory
#                                       "match", depths[i],
#                                       ".csv"), row.names=FALSE)
#}



obs_oxy<-read.csv('field_data/new_observed_oxy.csv') 
obs_oxy <- obs_oxy %>%
  mutate(DateTime_H = as.POSIXct(DateTime_H, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) %>%
  rename(Depth = depth) %>%
  select(DateTime_H, Depth, OXY_mmol) %>%
  rename(DateTime = DateTime_H)

mod_oxy <- get_var(nc_file, "OXY_oxy", reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy")

oxy_compare <-merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) %>%
  dplyr::rename(mod_oxy = OXY_oxy, obs_oxy = OXY_mmol)

#Loop for writing csv files for PEST
match_oxy <- vector('list')

for(i in 1:length(unique(oxy_compare$Depth))){


  match_oxy[[i]] <- oxy_compare %>%
    filter(Depth == depths[i])
  write.csv(round(match_oxy[[i]]$mod_oxy, 6), paste0( #file path for saving csv files, should be set to the working directory
                                                    "matchoxy", depths[i],
                                                    ".csv"), row.names=FALSE)

}


#Methane
depths_ch4 <- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)
obsch4 <- read.csv("field_data/observed_ch4.csv") %>%
mutate(DateTime_H = as.POSIXct(DateTime_H, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8")) %>%
select(DateTime_H, Depth, ch4_obs) %>%
rename(DateTime = DateTime_H)


modch4 <- get_var(nc_file, var="CH4_ch4", reference="surface", z_out=depths_ch4) %>%
 pivot_longer(cols=starts_with("CH4_ch4_"), names_to="Depth", names_prefix="CH4_ch4_", values_to = "CH4_ch4") 

 #The model data is set to Etc/GMT--8 by default even though in the nml the timzezone is set to EST hence I set the observed data to Etc/GMT--8 so that they match (even though it's not the correct timezone)

ch4_merge<-merge(modch4, obsch4, by=c("DateTime","Depth"))

match_ch4 <- vector('list')

for(i in 1:length(unique(ch4_merge$Depth))){


  match_ch4[[i]] <- ch4_merge %>%
    dplyr::filter(Depth == depths_ch4[i])
  write.csv(round(match_ch4[[i]]$CH4_ch4, 6), paste0(  #file path for saving csv files, should be set to the working directory
                                       "matchch4_", depths_ch4[i],
                                       ".csv"), row.names=FALSE)
}

#Diffusion flux
obs_ch4_diff <- read.csv("field_data/obs_diffusion_combined.csv") %>%
mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"))

ch4_diff_atm <- glmtools::get_var(nc_file, "CH4_ch4_atm") %>%
mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d", tz="Etc/GMT--8"))

diff_merge <-merge(obs_ch4_diff, ch4_diff_atm, by="DateTime")
 
write.csv(diff_merge$CH4_ch4_atm, "matchdiff.csv", row.names=FALSE)


#Ebullition flux
obs_ch4_ebb <- read.csv("field_data/observed_ebullition.csv") %>%
mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz="Etc/GMT--8"))

ch4_ebb_atm <- glmtools::get_var(nc_file, "BUB_bub_fluxes_Wch4") %>%
mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d", tz="Etc/GMT--8"))

ebb_merge <-merge(obs_ch4_ebb, ch4_ebb_atm, by="DateTime") %>%
na.omit()
 
write.csv(ebb_merge$BUB_bub_fluxes_Wch4, "matchebb.csv", row.names=FALSE)

ch4_diff_atm_merged <- ch4_diff_atm %>%
  mutate(ebb_flux = ch4_ebb_atm$BUB_bub_fluxes_Wch4)

write.csv(ch4_diff_atm_merged, "ebb_diff_fluxes.csv")

#PEST control file

#oxy profile weight
#1/sd(watertemp$obstemp)

#oxy control
#temp_control <- watertemp %>%
#  group_by(Depth) %>%
#  mutate(numbering = row_number()) %>%
#  arrange(Depth) %>%
#  mutate(weight = 0.1775454)
#  temp_control$ID <- paste0("wq", temp_control$Depth, "_t_", temp_control$numbering)
#  temp_control$obs_group <- paste0("temp_", temp_control$Depth)
#  temp_control<- temp_control %>% select(ID, obstemp, weight, obs_group)
#write.csv(temp_control, "temp_control.csv", row.names=FALSE)


#oxy profile weight
#1/sd(oxy_compare$obs_oxy)

#oxy control
#oxy_control <- oxy_compare %>%
#  group_by(Depth) %>%
#  mutate(numbering = row_number()) %>%
#  arrange(Depth) %>%
#  mutate(weight = 0.009506)
#  oxy_control$ID <- paste0("wq", oxy_control$Depth, "_ox_", oxy_control$numbering)
#  oxy_control$obs_group <- paste0("oxy_", oxy_control$Depth)
#  oxy_control<- oxy_control %>% select(ID, obs_oxy, weight, obs_group)
#write.csv(oxy_control, "oxy_control.csv", row.names=FALSE)


#ch4 profile weight
#1/sd(ch4_merge$ch4_obs)

#ch4 control
#ch4_control <- ch4_merge %>%
#  group_by(Depth) %>%
#  mutate(numbering = row_number()) %>%
#  arrange(Depth) %>%
#  mutate(weight = 0.05263655)
#  ch4_control$ID <- paste0("wq", ch4_control$Depth, "_ch4_", ch4_control$numbering)
#  ch4_control$obs_group <- paste0("ch4_", ch4_control$Depth)
#  ch4_control<- ch4_control %>% select(ID, CH4_ch4, weight, obs_group)
#write.csv(ch4_control, "ch4_control.csv", row.names=FALSE)


#diff weight
#1/sd(obs_ch4_diff$avg_ch4_diff)

#diff control

#diff_control <- diff_merge %>%
# mutate(numbering = row_number()) %>%
# mutate(weight = 0.3382529)
#diff_control$ID <- paste0("wq_diff_", diff_control$numbering)
#diff_control$obs_group <- paste0("diff_ch4")
#diff_control <- diff_control %>%
#  select(ID, avg_ch4_diff, weight, obs_group)
#write.csv(diff_control, "diff_control.csv", row.names=FALSE)

#ebb weight
#1/sd(ebb_merge$avg_ch4_ebb)

#ebb control
#ebb_control <- ebb_merge %>%
# mutate(numbering = row_number()) %>%
# mutate(weight = 0.6007484)
#ebb_control$ID <- paste0("wq_ebb_", ebb_control$numbering)
#ebb_control$obs_group <- paste0("ebb_ch4")
#ebb_control <- ebb_control %>%
#  select(ID, avg_ch4_ebb, weight, obs_group)

#write.csv(ebb_control, "ebb_control.csv", row.names=FALSE)



