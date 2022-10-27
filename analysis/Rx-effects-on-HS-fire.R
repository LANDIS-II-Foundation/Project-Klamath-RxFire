library(terra)
library(tidyverse)
library(ggpubr)

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

# h=3; i=1; j=1; k=1
cells4analysis <- NULL
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
        
        oneyr <- rast(paste0(wd,clim_scen, "/", rep, "/", management,"/files-for-use/fire-dnbr-", k, ".img")) %>%
          as.data.frame() %>% 
          mutate(clim = clim_scen, rep = rep, mgmt_scenario = management, year = k, cell = seq(1:8828190),
                 severity = case_when(Layer_1 == 0 ~ "Unburned",
                                      Layer_1 >= 1 & Layer_1 <= 200 ~ "Low severity", 
                                      Layer_1 > 200 & Layer_1 <= 439 ~ "Moderate severity", 
                                      Layer_1 > 439 & Layer_1 <= 2000 ~ "High severity")) %>%
          select(!Layer_1) %>%
          right_join(oneyr_ign) %>%
          filter(ign_type != "unburned")
        
        rm(oneyr_ign)
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
      
      cells4analysis <- for_sub %>% 
        ungroup() %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 1 & n_wildfire == 1) %>%
        filter(if_any(c(y_2:y_7), ~ . == "Rx")) %>%
        filter(if_any(c(y_8:y_13), ~ . == "wildfire")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "five years") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen) %>%
        rbind(cells4analysis)
        
      cells4analysis <- for_sub %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 1 & n_wildfire == 1) %>%
        filter(if_any(c(y_2:y_7), ~ . == "Rx")) %>%
        filter(if_any(c(y_13:y_18), ~ . == "wildfire")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "ten years") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen) %>%
        rbind(cells4analysis)
      
      cells4analysis <- for_sub %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 1 & n_wildfire == 1) %>%
        filter(if_any(c(y_2:y_7), ~ . == "Rx")) %>%
        filter(if_any(c(y_19:y_24), ~ . == "wildfire")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "fifteen years") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen) %>%
        rbind(cells4analysis)
      
      cells4analysis <- for_sub %>%
        select(cell, clim, year, ign_type, n_Rx, n_wildfire) %>%
        pivot_wider(names_from = year, names_prefix = "y_", values_from = ign_type) %>%
        filter(n_Rx == 1 & n_wildfire == 1) %>%
        filter(if_any(c(y_2:y_7), ~ . == "Rx")) %>%
        filter(if_any(c(y_25:y_30), ~ . == "wildfire")) %>%
        select(cell, clim) %>%
        distinct %>%
        mutate(fire_scen = "twenty years") %>%
        left_join(for_sub) %>%
        group_by(clim, year, fire_scen) %>%
        rbind(cells4analysis)
      
      rm(for_sub)
      gc()
    }
  }
}

cells4analysis <- read.csv("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/large_data_for_figs/severity_cells4analysis.csv")

cells4analysis %>%
  group_by(clim, fire_scen, ign_type, severity) %>%
  filter(ign_type != "Rx") %>%
  summarize(n_events = n()) %>%
  ggplot() +
  geom_col(aes(y = n_events, x = severity)) +
  facet_grid(cols = vars(clim), rows = vars(fire_scen))

for_plot <- cells4analysis %>%
  mutate(fire_scen = ifelse(fire_scen == "no Rx", "no Rx", "with Rx")) %>%
  group_by(clim, fire_scen, severity) %>%
  summarise(n_events = sum(n_events)) %>%
  group_by(clim, fire_scen) %>%
  mutate(proportion = ceiling(n_events/sum(n_events) * 100)) %>%
  filter(severity == "Low severity")

ggdotchart(for_plot, x = "clim", y = "proportion",
           color = "clim", size = 10,
           add = "segment",
           add.params = list(color = "lightgray", size = 1.5),
           position = position_dodge(1),
           palette = "jco",
           ggtheme = theme_pubclean()
)

ggsave("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/prop_low_severity_wildfire.pdf", width = 4, height =5)  



