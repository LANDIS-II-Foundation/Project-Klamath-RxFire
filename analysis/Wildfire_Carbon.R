library(terra)
library(tidyverse)
terraOptions(tempdir = "/Volumes/Alison.Deak_440-223-4897/R_temp_dir")

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
      
      for(k in seq(2,number_yrs)) { # for each year
        oneyr_ign <- rast(paste0(wd,clim_scen,"/",rep,"/", management,"/files-for-use/ignition-type-", k, ".img"))
        oneyr_sev <- rast(paste0(wd,clim_scen, "/", rep, "/", management,"/files-for-use/fire-dnbr-", k, ".img"))
        oneyr_totalC <- rast(paste0(wd,clim_scen,"/",rep,"/", management,"/files-for-use/TotalC-", k, ".img"))
        oneyr <- c(oneyr_ign, oneyr_sev, oneyr_totalC) %>% as.data.frame()
        names(oneyr) <- c("ign_type", "severity", "total_c")  
        oneyr_df <- oneyr %>% 
          mutate(clim = clim_scen, rep = rep, mgmt_scenario = management, year = k, cell = seq(1:8828190)) %>%
          filter(total_c != 0) %>%
          mutate(ign_type = case_when(ign_type == 1 ~ "Unburned", 
                                      ign_type == 2 | ign_type == 3 ~ "wildfire", 
                                      ign_type == 4 ~ "Rx"),
                 severity = case_when(severity == 0 ~ "Unburned",
                                      severity >=  1 & severity <= 200  ~ "Low severity", 
                                      severity > 200 & severity <= 439  ~ "Moderate severity", 
                                      severity > 439 & severity <= 2000 ~ "High severity"))
        all_cells_scen <- rbind(all_cells_scen, oneyr_df)
        rm(oneyr_ign, oneyr_sev, oneyr_totalC, oneyr, oneyr_df)
        gc()
      }
      one_wf <- all_cells_scen %>%
        group_by(cell, clim, rep, mgmt_scenario) %>%
        count(ign_type) %>%
        pivot_wider(names_from = ign_type, values_from = n, names_prefix = "n_") %>%
        filter(n_wildfire == 1) %>%
        mutate(n_Rx = ifelse("n_Rx" %in% names(.), n_Rx, 0)) %>%
        right_join(all_cells_scen) %>%
        filter(ign_type == "wildfire" & severity == "High severity" & year == 10) %>%
        right_join(all_cells_scen)  
      cells4analysis <- rbind(cells4analysis, one_wf)  
      rm(all_cells_scen, one_wf)
      
    }
  }
}
