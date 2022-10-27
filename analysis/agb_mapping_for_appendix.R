## BIOMASS MAPS

library(tidyverse)
library(terra)
library(MetBrewer)
library(patchwork)
options(scipen=3333)

out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/agb_maps"

landis <- rast("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/GCM8.5a_DRY/rep-1/No-Rx/output/biomass/bio-TotalBiomass-0.tif")

mask_noproj <- rast("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/landscape/landscape_mask.tif")
mask <- project(mask_noproj, "EPSG:4269")

raster_USFS <- rast("/Users/alisondeak/Downloads/carbon_ag_mg_ha.img")
cat_USFS <- catalyze(raster_USFS)

polygon<-as.polygons(mask, dissolve=TRUE)

FIA <- project(cat_USFS,mask)
FIA <- mask(FIA, polygon)
FIA <- crop(FIA, polygon)
FIA <- FIA * 100 # Convert to g carbon/m^2
FIA[FIA>50000] <- NA
FIA[FIA==0] <- NA


terra::values(mask_noproj) <- terra::values(landis)
landis <- project(mask_noproj, "EPSG:4269")
landis[landis==0] <- NA

FIA_df <- as.data.frame(FIA, xy = T)
landis_df <- as.data.frame(landis, xy = T) %>% mutate(landscape_mask = landscape_mask/2)

ggplot() +
  geom_raster(data = FIA_df, aes(x = x, y = y, fill = Histogram)) +
  theme_light() +
  scale_fill_gradientn(colors = met.brewer("VanGogh3")) +
  ggtitle("Interpolated from FIA data") +
  ylab("Longitude") + xlab("Latitude") +
  theme(legend.position = "bottom") +
ggplot() +
  geom_raster(data = landis_df, aes(x = x, y = y, fill = landscape_mask)) +
  theme_light() +
  scale_fill_gradientn(colors = met.brewer("VanGogh3")) +
  ggtitle("Study area at time zero") +
  ylab("Longitude") + xlab("Latitude") +
  theme(legend.position = "bottom")
ggsave(paste0(out_dir, "/agb_maps.pdf"), width = 8, height = 8)

ggplot(data = FIA_df, aes(x=Histogram)) +
  geom_histogram(fill = "#659357", color = "white", binwidth = 2500) +
  theme_light() + 
  xlim(c(0, 75000)) +
  xlab("Aboveground carbon (g/m^2)") +  
ggplot(data = landis_df, aes(x = landscape_mask)) +
  geom_histogram(fill = "#659357", color = "white", binwidth = 2500) +
  theme_light() + 
  xlim(c(0, 75000)) +
  xlab("Aboveground carbon (g/m^2)")
ggsave(paste0(out_dir, "/agb_histograms.pdf"), width = 8, height = 3)

summary(FIA_df$Histogram)
sum(FIA_df$Histogram)

summary(landis_df$landscape_mask)
sum(FIA_df$Histogram)
  
