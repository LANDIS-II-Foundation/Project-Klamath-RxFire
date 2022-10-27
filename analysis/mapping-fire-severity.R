library(tidyverse)
library(terra)

##-----------------------------
## Looking at where wildfires occured on the landscape
##-----------------------------

wd <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/"

cols <- c("#660d20", "#e59a52", "#edce79")

options(scipen = 33333)
ha_per_cell <- 0.09
number_yrs <- 51
n_cells <- 8828190

##-----------------------------
## STACKED BAR CHART OF SEVERITY BY PROPORTION OF TOTAL CELLS BURNED
##-----------------------------
# h=1; i=1; j=1; k=2
all_events <- NULL
all_scen <- list.files(wd)
for (h in 1:length(all_scen)) {
  clim <- all_scen[[h]]
  clim_scenario <- list.files(paste0(wd, clim))
  for (i in 1: length(clim_scenario)) {
    rep <- clim_scenario[[i]]
    mgmt_scenario <- list.files(paste0(wd, clim, "/", rep))
    for (j in 1:length(mgmt_scenario)) {
      management <- mgmt_scenario[[j]]
      for(k in seq(2,number_yrs)) {
        oneyr_events <- rast(paste0(wd, clim, "/",rep,"/", management,"/files-for-use/ignition-type-", k,".img")) 
        oneyr_events[oneyr_events == 0 | oneyr_events == 1 | oneyr_events == 4] <- NA
        oneyr_dnbr <- rast(paste0(wd, clim, "/", rep, "/", management, "/files-for-use/fire-dnbr-", k, ".img"))
        oneyr_mask <- mask(oneyr_dnbr, oneyr_events)
        all_events <- c(all_events, oneyr_mask)
      }
    }
   assign(paste0(clim, "_", management), all_events) 
   all_events <- NULL
  }
}
