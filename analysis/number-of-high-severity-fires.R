library(tidyverse)

wd <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/"

cols <- c("#660d20", "#e59a52", "#edce79")

options(scipen = 33333)
ha_per_cell <- 0.09
number_yrs <- 51
n_cells <- 8828190

high_severity_fires <- NULL
all_scen <- list.files(wd)
# h=1; i=1; j=1
for (h in 1:length(all_scen)) {
  clim <- all_scen[[h]]
  clim_scenario <- list.files(paste0(wd, clim))
  for (i in 1: length(clim_scenario)) {
    rep <- clim_scenario[[i]]
    mgmt_scenario <- list.files(paste0(wd, clim, "/", rep))
    for (j in 1:length(mgmt_scenario)) {
      management <- mgmt_scenario[[j]]
      severities <- read.csv(paste0(wd, clim, "/",rep,"/", management,"/scrapple-events-log.csv")) %>%
        filter(SimulationYear > 1 & IgnitionType == " Accidental" | IgnitionType == " Lightning") %>%
        select(SimulationYear, MeanDNBR) %>%
        mutate(severity = case_when(MeanDNBR >= 0 & MeanDNBR <= 200 ~ "Low_severity", 
                                    MeanDNBR > 200 & MeanDNBR <= 439 ~ "Moderate_severity", 
                                    MeanDNBR > 439 & MeanDNBR <= 2000 ~ "High_severity"),
               clim_scenario = clim, rep = i, mgmt_scenario = management) %>%
        filter(severity == "High_severity")
      high_severity_fires <- rbind(high_severity_fires, severities)
    }
  }
}

high_severity_fires %>%
  group_by(clim_scenario, mgmt_scenario, rep) %>%
  summarise(n_fires = n()) %>%
  group_by(clim_scenario, mgmt_scenario) %>%
  summarise(mean_n = mean(n_fires)) %>%
  ggplot() +
  geom_col(aes(y = mean_n, x = mgmt_scenario)) +
  facet_grid(cols = vars(clim_scenario))

