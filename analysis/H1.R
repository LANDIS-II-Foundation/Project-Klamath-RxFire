library(terra)
library(tidyverse)

## ------------------------------------
## Sites with increased prescribed fire will have lower yet more stable 
## aboveground biomass in all climate scenarios due to lowered mortality 
## of trees during high-severity wildfire events. 
## ------------------------------------

# /Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/Historical/rep-1/10x-Rx/social-climate-fire/ignition-type-1.img
wd <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/"
number_yrs = 51
clim <- list.files(wd)

## ------------------------------------
## Change in cell biomass based on the number of prescribed fires during simulation
##------------------------------------

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
        group_by(cell, clim, rep, mgmt_scenario) %>%
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
        group_by(clim, year, fire_scen) %>%
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
        group_by(clim, year, fire_scen) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        rbind(Rx_cells4analysis)
      
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
        group_by(clim, year, fire_scen) %>%
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
        group_by(clim, year, fire_scen) %>%
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
        group_by(clim, year, fire_scen) %>%
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
        group_by(clim, year, fire_scen) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        
        rbind(Rx_cells4analysis)
      
      Rx_cells4analysis <- for_sub %>% 
        ungroup() %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 6 & n_wildfire == 0) %>% 
        filter(if_any(c(y_2:y_9), ~ . == "Rx")) %>%
        filter(if_any(c(y_10:y_17), ~ .  == "Rx")) %>% 
        filter(if_any(c(y_18:y_25), ~ .  == "Rx")) %>%
        filter(if_any(c(y_26:y_34), ~ . == "Rx")) %>%
        filter(if_any(c(y_35:y_42), ~ .  == "Rx")) %>% 
        filter(if_any(c(y_43:y_51), ~ . == "Rx")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "6x-Rx") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen) %>%
        summarise(mean_TotalC = mean(TotalC, na.omit = T), sd_TotalC = sd(TotalC), n_cells = n(),
                  mean_abgC = mean(aboveground_C, na.omit = T), sd_aboveground = sd(aboveground_C),
                  mean_somC = mean(som_C, na.omit = T), sd_somC = sd(som_C)) %>%        
        rbind(Rx_cells4analysis)

      rm(for_sub)
      gc()
    }
  }
}
cells4analysis <- rbind(Rx_cells4analysis, wf_cells4analysis)
write.csv(cells4analysis, "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/large_data_for_figs/agb_by_n_fires_subset.csv")
cells4analysis <- read.csv("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/large_data_for_figs/agb_by_n_fires_subset.csv")

unique(Rx_cells4analysis$fire_scen)

# Aboveground C
cells4analysis %>%
  group_by(clim, year, fire_scen) %>%
  mutate(year = year - 1) %>%
  summarise(mean_abgC = mean(mean_abgC), sd_abgC = mean(sd_aboveground),
            mean_somC = mean(mean_somC), sd_somC = mean(sd_somC), 
            mean_TotalC = mean(mean_TotalC), sd_TotalC = sd_TotalC) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = mean_TotalC - sd_TotalC, ymax = mean_TotalC + sd_TotalC, fill = "lightgray"), alpha = 0.2) +
  geom_ribbon(aes(ymin = mean_somC - sd_somC, ymax = mean_somC + sd_somC, fill = "#6b6138"), alpha = 0.4) +
  geom_ribbon(aes(ymin = mean_abgC - sd_abgC, ymax = mean_abgC + sd_abgC, fill = "#659357"), alpha = 0.4) +
  geom_line(aes(y = mean_TotalC, color = "mean_TotalC"), size = 0.5, linetype = "dashed") +
  geom_line(aes(y = mean_abgC), color = "darkgreen") +
  geom_line(aes(y = mean_somC), color = "chocolate4") +
  guides(fill=guide_legend(title="")) +
  facet_grid(rows = vars(clim), cols = vars(fire_scen)) +
  scale_fill_manual( "", labels = c("Aboveground C", "Soil organic C"), values=c("#659357"="#659357", "#6b6138"="#6b6138")) +
  theme_minimal() + 
  xlab("Simulation year") +
  ylab("Carbon (g/m^2)") +
  theme(legend.position = "bottom") 

ggsave("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/agb+bg_C_by_Rx_scenario.pdf", height = 5, width = 8)

# Belowground C
Rx_cells4analysis %>%
  rbind(wf_cells4analysis) %>%
  mutate(year = year - 1) %>%
  group_by(clim, year, fire_scen) %>%
  summarise(mean_somC = mean(mean_somC), sd_somC = mean(sd_somC)) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = mean_somC - sd_somC, ymax = mean_somC + sd_somC), 
              fill = "#6b6138", alpha = 0.6) +
  geom_line(aes(y = mean_somC)) +
  facet_grid(rows = vars(clim), cols = vars(fire_scen)) +
  theme_light() + 
  ggtitle("Belowground SOM carbon storage of cells with increasing prescribed fire frequency") +
  xlab("Simulation year") +
  ylab("Total SOM carbon (g/m^2)")
ggsave("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/somC_by_Rx_scenario.pdf", height = 5, width = 8)

## Above + below ground C
Rx_cells4analysis %>%
  rbind(wf_cells4analysis) %>%
  mutate(year = year - 1) %>%
  group_by(clim, year, fire_scen) %>%
  summarise(mean_TotalC = mean(mean_TotalC), sd_TotalC = mean(sd_TotalC)) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = mean_TotalC - sd_TotalC, ymax = mean_TotalC + sd_TotalC), 
              fill = "#1C5E5F", alpha = 0.6) +
  geom_line(aes(y = mean_TotalC)) +
  facet_grid(rows = vars(clim), cols = vars(fire_scen)) +
  theme_light() + 
  ggtitle("Total carbon storage of cells with increasing prescribed fire frequency") +
  xlab("Simulation year") +
  ylab("Total carbon (g/m^2)")
ggsave("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/Total_C_by_Rx_scenario.pdf", height = 5, width = 8)

##-----------------------------
## BIOMASS CONSUMED ANALYSIS
##-----------------------------
all_scen <- list.files(wd)
all_sims_agb_lost <- NULL
all_sims_agb <- NULL
for (h in 1:length(all_scen)) {
  clim <- all_scen[[h]]
  clim_scenario <- list.files(paste0(wd, clim))
  for (i in 1: length(clim_scenario)) {
    rep <- clim_scenario[[i]]
    mgmt_scenario <- list.files(paste0(wd, clim, "/", rep))
    for (j in 1:length(mgmt_scenario)) {     
      management <- mgmt_scenario[[j]]
      sim_agb_lost <- read.csv(paste0(wd, clim, "/", rep, "/", management,"/scrapple-summary-log.csv")) %>%
        filter(SimulationYear > 1) %>%
        dplyr::select(SimulationYear, TotalBiomassMortalityAccidental, TotalBiomassMortalityLightning, TotalBiomassMortalityRx) %>%
        pivot_longer(cols = starts_with("TotalBiomassMortality"), names_to = "fire_type", names_prefix = "TotalBiomassMortality", values_to = "carbon_lost") %>%
        group_by(fire_type) %>% 
        summarise(total_carbon_lost = (sum(carbon_lost)*0.00000001)/2) %>%
        na.omit %>%
        mutate(clim_scenario = clim, Rx_scenario = management, rep = rep)
      
      all_sims_agb_lost <- rbind(all_sims_agb_lost, sim_agb_lost)
    }
  }
}

cols = c("#c1d4bc", "#659357", "#334a2b")

all_sims_agb_lost %>%
  group_by(rep, Rx_scenario, clim_scenario) %>%
  mutate(sum_c_lost = sum(total_carbon_lost)) %>%
  group_by(Rx_scenario, clim_scenario) %>%
  mutate(sd_c_lost = sd(sum_c_lost), mean_c_lost = mean(sum_c_lost)) %>%
  select(fire_type, total_carbon_lost, Rx_scenario, clim_scenario, sd_c_lost, mean_c_lost) %>%
  group_by(fire_type, Rx_scenario, clim_scenario) %>%
  mutate(total_carbon_lost = mean(total_carbon_lost)) %>%
  distinct() %>%
  mutate(Rx_scenario = factor(Rx_scenario, levels = c("No-Rx", "1x-Rx", "3x-Rx", "10x-Rx")),
         fire_type = factor(fire_type, level = c("Rx", "Lightning", "Accidental"))) %>%
  ggplot(aes(x=Rx_scenario, y=total_carbon_lost, fill=fire_type)) + 
  geom_errorbar(aes(ymax = mean_c_lost + sd_c_lost, ymin = mean_c_lost-sd_c_lost), colour="darkgrey", width=.65) +
  geom_col(position = position_stack(), alpha = 70, color = "white") +
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
  guides(fill=guide_legend(title="Ignition type")) +
  theme_minimal() +
  labs(y="Tg aboveground carbon lost", x = "Treatment scenario") +
  facet_grid(cols = vars(clim_scenario))

ggsave(paste0(out_dir,"carbon_loss_by_scenario.pdf"), width = 7, height = 4)

all_sims_agb_lost %>% filter(clim_scenario == "GCM8.5b_WET" & Rx_scenario == "1x-Rx") %>% 
  group_by(fire_type, clim_scenario, Rx_scenario) %>%
  summarise(mean_agb_lost = mean(total_carbon_lost))

## Total Aboveground C on landscape
all_scen <- list.files(wd)
all_sims_C <- NULL
for (h in 1:length(all_scen)) {
  clim <- all_scen[[h]]
  clim_scenario <- list.files(paste0(wd, clim))
  for (i in 1: length(clim_scenario)) {
    rep <- clim_scenario[[i]]
    mgmt_scenario <- list.files(paste0(wd, clim, "/", rep))
    for (j in 1:length(mgmt_scenario)) {     
      management <- mgmt_scenario[[j]]
      time_50_agb <- rast(paste0(wd, clim, "/", rep, "/", management,"/files-for-use/bio-TotalBiomass-50.tif")) %>%
        as.data.frame() %>%
        rename("agb" = "bio-TotalBiomass-50") %>%
        filter(agb > 0) %>%
        summarise(sum_C = sum(agb/2)) %>%
        mutate(clim_scenario = clim, Rx_scenario = management, rep = rep)
      
      all_sims_C<- rbind(all_sims_C, time_50_agb)
    }
  }
}

all_sims_C %>%
  group_by(clim_scenario, Rx_scenario) %>%
  summarize(mean_C = mean(sum_agb*0.00000001/2, na.omit =T)) %>%
  ggplot() +
  geom_col(aes(y = mean_C, x = Rx_scenario)) +
  facet_grid(cols = vars(clim_scenario))


