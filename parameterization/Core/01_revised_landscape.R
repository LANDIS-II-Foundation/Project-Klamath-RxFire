library(sf)
library(rgdal)
library(raster)
library(spatialEco)
library(ggplot2)

wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous"
setwd(wd)
desired_crs <- crs("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")

EPA_Eco <- readOGR("EPA_Ecoregions/us_eco_l4/us_eco_l4_no_st.shp")
eco_reproj <- spTransform(EPA_Eco, CRSobj = desired_crs)
Klamath_ERs <- eco_reproj[eco_reproj$NA_L3NAME == "Klamath Mountains",]
plot(Klamath_ERs, axes=TRUE)
Klamath_ERs$US_L4NAME

Serp_Siskiyous <- Klamath_ERs[Klamath_ERs$US_L4NAME == "Serpentine Siskiyous",]
plot(Serp_Siskiyous, axes=TRUE)

### potential landscape 1 
# Coast_Siskiyous <- Klamath_ERs[Klamath_ERs$US_L4NAME == "Coastal Siskiyous",]
# plot(Coast_Siskiyous)
# coast_sisk_outline <- remove.holes(Coast_Siskiyous)
# plot(coast_sisk_outline)
# 
# raster_temp <- raster()
# extent(raster_temp) = extent(coast_sisk_outline)
# res(raster_temp) = 30
# coast_sisk_raster <- rasterize(coast_sisk_outline, raster_temp)
# crs(coast_sisk_raster) = desired_crs
# plot(coast_sisk_raster)
# freq(coast_sisk_raster) # number of active cells = 2535682 ; inactive cells = 3831518
# landscape_1 <- coast_sisk_raster

### potential landscape 2

sh <- max(Serp_Siskiyous$Shape_Area)
max_serp_sisk <- Serp_Siskiyous[Serp_Siskiyous$Shape_Area == sh,]
plot(max_serp_sisk)

raster_temp <- raster()
extent(raster_temp) = extent(max_serp_sisk)
res(raster_temp) = 30
landscape_2 <- rasterize(max_serp_sisk, raster_temp)
crs(landscape_2) = desired_crs
plot(landscape_2)
freq(landscape_2)   # number active cells = 1,934,454 ; inactive cells = 9,058,594
landscape_2 <- trim(landscape_2, values=NA)
plot(landscape_2)

writeRaster(landscape_2, "Final Products/landscape_v2/landscape_mask.tif", format='GTiff', overwrite=TRUE)
writeOGR(max_serp_sisk, dsn="Final Products/landscape_v2/landscape_shape", layer = "landscape_shape", driver="ESRI Shapefile", overwrite_layer = TRUE)



