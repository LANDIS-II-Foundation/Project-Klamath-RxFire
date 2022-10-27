library(raster)
library(sf)
library(tidyverse)
options(scipen = 100)

rasterOptions(tmpdir = "/Volumes/Alison.Deak_440-223-4897/R_temp_dir")
wd <- "/Volumes/Alison.Deak_440-223-4897/"
# wd <- "E://biomass_harvest/"
# out_dir <- "C://Users/adeak/Desktop/LANDIS/Siskiyous/"

mask <- raster(paste0(wd, "SCRPPLE/landscape_mask.tif"))
mask[mask==0] <- NA

stands <- raster(paste0(wd, "biomass_harvest/E4_stands_BAU_v5.tif"))
stands_reproj <- projectRaster(stands, mask, method = 'ngb', res=30)
stands <- mask(stands_reproj, mask)
stands[is.na(stands)] <- 0
writeRaster(stands, paste0(wd, "biomass_harvest/stands.tif"))

ownership <- raster(paste0(wd, "biomass_harvest/E4_ownership_BAU_v4.tif"))
cells_big <- (ncell(ownership) - cellStats(ownership, 'countNA')) 
ha_big <- cells_big* 7.29
ownership_reproj <- projectRaster(ownership, mask, method = 'ngb', res=30)
ownership <- mask(ownership_reproj, mask)
ownership[is.na(ownership)] <- 0
writeRaster(ownership, paste0(wd, "biomass_harvest/ownership.tif"))

ownership_lil_rast <- raster(paste0(wd,"biomass_harvest/ownership.tif"))
ownership_lil_rast[ownership_lil_rast == 0] <- NA
stands <- raster(paste0(wd,"biomass_harvest/stands.tif"))

## ---------------------------------------------
imp <- read.csv(paste0(wd, "biomass_harvest/harvest_implementations.csv"))
ncells_lil <- 1901986 
ha_lil <- cells_lil * 0.09

ownership_lil <- ownership_lil_rast %>%
  as.data.frame() %>%
  group_by(ownership) %>%
  summarise(n_lil = n()) %>%
  mutate(ha_lil = n_lil * 0.09)

ownership_big <- ownership %>%
  as.data.frame() %>%
  group_by(E4_ownership_BAU_v4) %>%
  summarise(n_big = n()) %>%
  mutate(ha_big = n_big * 7.29)

ownership_all <- cbind.data.frame(ownership_lil, ownership_big) %>%
  left_join(imp, by = c(ownership = "mgmt_area")) %>%
  mutate(proportion = ha_lil/ha_big) %>%
  mutate(harvest_area_revised = harvest_area * proportion * 100) 

write.csv(ownership_all, paste0(wd, "biomass_harvest/revised_harvest_areas.csv"))


## ----------------------------------------------

writeRaster(ownership, paste0(out_dir, "ownership.tif"), datatype = "INT4S", overwrite=T)
writeRaster(stands, paste0(out_dir, "stands.tif"), datatype = "INT4S", overwrite=T)

is.na(ownership)
