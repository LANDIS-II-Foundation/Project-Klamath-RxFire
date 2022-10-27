library(terra)
library(tidyverse)

## ------------------------------------
## Sites with increased prescribed fire will have lower yet more stable 
## aboveground biomass in all climate scenarios due to lowered mortality 
## of trees during high-severity wildfire events. 
## ------------------------------------

wd <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/"
number_yrs = 51
clim <- list.files(wd)

## ------------------------------------

clim_region <- rast("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/ecoregions/spatial_data/ecoregions.tif") %>%
  as.data.frame() %>%
  rename("clim_region" = ecoregions)
mask <- rast("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/landscape/landscape_mask.tif")
oneyr_dnbr <- rast("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/Historical/rep-1/10x-Rx/files-for-use/fire-dnbr-2.img")
dem <- rast("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/landscape/merged_dem.tif")
slope <- terrain(dem, v="slope", neighbors=8, unit="degrees") %>%
  project(mask) %>%
  crop(mask) %>%
  as.data.frame %>%
  mutate(slope = case_when(slope <= 20 ~ "< 20°", slope > 20 ~ "> 20°"))
variables <- terrain(dem, v="aspect", neighbors=8, unit="degrees") %>%
  project(mask) %>%
  crop(mask) %>%
  as.data.frame %>%
  mutate(aspect = case_when(aspect >  45 & aspect <= 135 ~ "north",
                            aspect > 135 & aspect <= 225 ~ "west",
                            aspect > 225 & aspect <= 315 ~ "south",
                            aspect > 315 | aspect <=  45 ~ "east")) %>%
  cbind(slope) %>%
  cbind(clim_region) %>%
  mutate(cell = seq(1:8828190))

rm(clim_region, mask, oneyr_dnbr, dem, slope)
## ------------------------------------

# h=1; i=1; j=1; k=2
Rx_cells4analysis <- NULL
wf_cells4analysis <- NULL

for (h in 1:length(clim)) { # for each climate scenario
  clim_scen <- clim[[h]]
  reps <- list.files(paste0(wd, "/", clim_scen))
  
  for (i in 1: length(reps)) { # for each rep
    rep <- reps[[i]]
    mgmt_scenario <- list.files(paste0(wd,clim_scen, "/", rep))
    
    for (j in 1:length(mgmt_scenario)) { # for each management scenario
      management <- mgmt_scenario[[j]]
      all_cells_scen <- NULL
      
      for(k in seq(2,number_yrs)) {
        oneyr_ign <- rast(paste0(wd,clim_scen,"/",rep,"/", management,"/files-for-use/ignition-type-", k, ".img")) %>%
          as.data.frame() %>%
          mutate(clim = clim_scen, rep = rep, mgmt_scenario = management, year = k, cell = seq(1:8828190)) %>%
          filter(Layer_1 > 0) %>%
          rename(ign_type = "Layer_1") %>%
          mutate(ign_type = case_when(ign_type == 1 ~ "unburned",
                                      ign_type == 2 | ign_type == 3 ~ "wildfire",
                                      ign_type == 4 ~ "Rx"))
        
        oneyr_sev <- rast(paste0(wd,clim_scen, "/", rep, "/", management,"/files-for-use/fire-dnbr-", k, ".img")) %>%
          as.data.frame() %>% 
          mutate(clim = clim_scen, rep = rep, mgmt_scenario = management, year = k, cell = seq(1:8828190),
                 severity = case_when(Layer_1 == 0 ~ "Unburned",
                                      Layer_1 >= 1 & Layer_1 <= 200 ~ "Low severity", 
                                      Layer_1 > 200 & Layer_1 <= 439 ~ "Moderate severity", 
                                      Layer_1 > 439 & Layer_1 <= 2000 ~ "High severity")) %>%
          select(!Layer_1)
        
        oneyr_somC <- rast(paste0(wd, clim_scen,"/",rep,"/", management,"/files-for-use/SOMTC-", k, ".img")) %>%
          as.data.frame() %>%
          mutate(cell = seq(1:8828190)) %>%
          rename("som_C" = Layer_1)
        
        oneyr <- rast(paste0(wd,clim_scen,"/",rep,"/", management,"/files-for-use/TotalC-", k, ".img")) %>%
          as.data.frame() %>% 
          rename("TotalC" = Layer_1) %>%
          mutate(cell = seq(1:8828190)) %>%
          right_join(oneyr_ign) %>%
          left_join(oneyr_sev) %>%
          left_join(oneyr_somC) %>%
          mutate(aboveground_C = TotalC - som_C)
        
        rm(oneyr_ign, oneyr_sev, oneyr_somC)
        all_cells_scen <- rbind(all_cells_scen, oneyr)
      }
      for_sub <- all_cells_scen %>%
        left_join(variables) %>%
        filter(aspect == "north" | aspect == "south") %>%
        group_by(cell, clim, rep, mgmt_scenario, aspect, slope, clim_region) %>%
        count(ign_type) %>%
        pivot_wider(names_from = ign_type, values_from = n, names_prefix = "n_") %>%
        mutate(n_Rx = ifelse("n_Rx" %in% names(.), n_Rx, 0)) %>%
        select(cell, clim, rep, mgmt_scenario, n_wildfire, n_Rx) %>%
        mutate(n_wildfire = ifelse(is.na(n_wildfire), 0, n_wildfire), n_Rx = ifelse(is.na(n_Rx), 0, n_Rx)) %>%
        right_join(all_cells_scen) %>%
        ungroup()
      rm(all_cells_scen)
      
      wf_cells4analysis <- for_sub %>% 
        ungroup() %>%
        mutate(ign_type = paste0(ign_type, "_", severity)) %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 0 & n_wildfire == 1) %>% 
        filter(if_any(c(y_2:y_7), ~ . == "wildfire_High severity")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "one_wildfire") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen, aspect, slope, clim_region) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%
        rbind(wf_cells4analysis)
      
      Rx_cells4analysis <- for_sub %>% 
        ungroup() %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 1 & n_wildfire == 0) %>% 
        filter(if_any(c(y_2:y_11), ~ . == "Rx")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "1x-Rx") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen, aspect, slope, clim_region,) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        
        rbind(Rx_cells4analysis)
      
      Rx_cells4analysis <- for_sub %>% 
        ungroup() %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 2 & n_wildfire == 0) %>% 
        filter(if_any(c(y_2:y_25), ~ . == "Rx")) %>%
        filter(if_any(c(y_26:y_51), ~ . == "Rx")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "2x-Rx") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen, aspect, slope, clim_region) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        
        rbind(Rx_cells4analysis)     
      
      Rx_cells4analysis <- for_sub %>% 
        ungroup() %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 3 & n_wildfire == 0) %>% 
        filter(if_any(c(y_2:y_17), ~ . == "Rx")) %>%
        filter(if_any(c(y_18:y_33), ~ .  == "Rx")) %>% 
        filter(if_any(c(y_34:y_50), ~ .  == "Rx")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "3x-Rx") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen, aspect, slope, clim_region) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        
        rbind(Rx_cells4analysis)
      
      Rx_cells4analysis <- for_sub %>% 
        ungroup() %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 4 & n_wildfire == 0) %>% 
        filter(if_any(c(y_2:y_13), ~ . == "Rx")) %>%
        filter(if_any(c(y_14:y_25), ~ .  == "Rx")) %>% 
        filter(if_any(c(y_26:y_37), ~ .  == "Rx")) %>%
        filter(if_any(c(y_38:y_51), ~ .  == "Rx")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "4x-Rx") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen, aspect, slope, clim_region) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        
        rbind(Rx_cells4analysis)      
      
      Rx_cells4analysis <- for_sub %>% 
        ungroup() %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 5 & n_wildfire == 0) %>% 
        filter(if_any(c(y_2:y_11), ~ . == "Rx")) %>%
        filter(if_any(c(y_12:y_21), ~ .  == "Rx")) %>% 
        filter(if_any(c(y_22:y_31), ~ .  == "Rx")) %>%
        filter(if_any(c(y_32:y_41), ~ . == "Rx")) %>%
        filter(if_any(c(y_42:y_51), ~ .  == "Rx")) %>% 
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "5x-Rx") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen, aspect, slope, clim_region) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        
        rbind(Rx_cells4analysis)
      
      rm(for_sub)
      gc()
    }
  }
}

cells4analysis <- Rx_cells4analysis %>% rbind(wf_cells4analysis)

write.csv(cells4analysis, "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/large_data_for_figs/agb_by_n_fires_&_terrain.csv")

# create dot plot
