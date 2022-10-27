##------------------------------------------------
# Script to create NECN soil rasters from UC Davis SoilWeb data (Walkinshaw, 2020) 
# and Oak Ridge National Laboratory (West, 2014).

# data citations: 
# Walkinshaw, Mike, A.T. O'Geen, D.E. Beaudette. "Soil Properties." California Soil Resource Lab, 
# 1 Oct. 2020, casoilresource.lawr.ucdavis.edu/soil-properties/. 

# West, T.O. 2014. Soil Carbon Estimates in 20-cm Layers to 1-meter Depth, Conterminous US, 1970-1993. 
# ORNL DAAC, Oak Ridge, Tennessee, USA. https://doi.org/10.3334/ORNLDAAC/1238

# Soil Survey Staff, Natural Resources Conservation Service, United States 
# Department of Agriculture. U.S. General Soil Map (STATSGO2). Available 
# online at https://sdmdataaccess.sc.egov.usda.gov. Accessed [06/15/2021]. 

##------------------------------------------------

library(raster)
library(tidyverse)
library(rgdal)
library(sf)

rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous" 
setwd(wd)

##---------------------------------------------
# soil clay percentage, sand percentage, depth, and drainage (Walkinshaw, 2020)

# input soil data tifs
clay           <- raster("soils/UCDavis_soil_tifs/clay.tif")
drainage_cl    <- raster("soils/UCDavis_soil_tifs/drainage_class_int.tif")
sand           <- raster("soils/UCDavis_soil_tifs/sand.tif")
soil_depth     <- raster("soils/UCDavis_soil_tifs/soil_depth.tif")

# input raster to match dimensions, crop to extent of, and mask files with
landscape_mask <- raster("FinalProducts/landscape_mask.tif")
landscape_mask[landscape_mask==0] <- NA
plot(landscape_mask)

# reproject
clay_reproj      <- projectRaster(clay, landscape_mask, method = 'ngb', res=30)
drainage_reproj  <- projectRaster(drainage_cl, landscape_mask, method = 'ngb', res = 30)
sand_reproj      <- projectRaster(sand, landscape_mask, method = 'ngb', res=30)
depth_reproj     <- projectRaster(soil_depth, landscape_mask, method = 'ngb', res=30)

# crop 
clay_crop      <- crop(clay_reproj, landscape_mask)
drainage_crop  <- crop(drainage_reproj, landscape_mask)
sand_crop      <- crop(sand_reproj, landscape_mask)
depth_crop     <- crop(depth_reproj, landscape_mask)

# mask
clay_mask      <- mask(clay_crop, landscape_mask)
drainage_mask  <- mask(drainage_crop, landscape_mask)
sand_mask      <- mask(sand_crop, landscape_mask)
depth_mask     <- mask(depth_crop, landscape_mask)

# remove zeroes
clay_mask[is.na(clay_mask)] <- 0
drainage_mask[is.na(drainage_mask)] <- 0
sand_mask[is.na(sand_mask)] <- 0
depth_mask[is.na(depth_mask)] <- 0

plot(drainage_mask)

rm(clay, drainage_cl, sand, soil_depth, clay_reproj, drainage_reproj, sand_reproj, depth_reproj,
  clay_crop, drainage_crop, sand_crop, depth_crop)

### modify masks to fit requisite NECN parameters, save jpegs of plots, and raster tifs 

##-----
# percent clay - needs to be expressed as ratio (0.0 - 1.0)
final_clay <- clay_mask * 0.01
writeRaster(final_clay, "FinalProducts/soils/percent_clay.tif", datatype='FLT8S', overwrite=TRUE)

##-----
# percent sand - needs to be expressed as ratio (0.0 - 1.0)
final_sand <- sand_mask * 0.01

jpeg("soils/sand_clay_percentage.jpg")
par(mfrow=c(1,2), mar = c(5, 5, 5, 5))
plot(final_sand, main = "Percent sand, \nexpressed as a fraction")
plot(final_clay, main = "Percent clay, \nexpressed as a fraction")
dev.off()

writeRaster(final_sand, "FinalProducts/soils/percent_sand.tif", datatype='FLT8S', overwrite=TRUE)

##-----
# soil depth 
writeRaster(depth_mask, "FinalProducts/soils/soil_depth.tif", datatype='FLT8S', overwrite=TRUE)

##-----
# soil drainage
reclass_matrix <- matrix(c(7,3,5,1,2,4,6,8,0.25,0.50,0.75,1.0,1.0,1.0,1.0,1.0), ncol=2)
final_drainage <- reclassify(drainage_mask, reclass_matrix)

jpeg("soils/soil_drainage_depth.jpg")
par(mfrow=c(1,2), mar = c(5, 5, 5, 5))
plot(final_drainage, main = "Soil drainage")
plot(depth_mask, main = "Soil depth, in centimeters")
dev.off()

writeRaster(final_drainage, "FinalProducts/soils/soil_drainage.tif", datatype='FLT8S', overwrite=TRUE)

##------------------------------------------------------------------------
##------------------------------------------------------------------------

# Soil carbon and nitrogen pools (West, 2014)
SOC_shp <- st_read("soils/ORNL_SOC_estimates/data/statsgo1poly/statsgo1poly.shp")
extent <- st_as_sfc(st_bbox(landscape_mask))
SOC_reproj <- st_transform(SOC_shp, crs="+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
st_crs(extent) <- st_crs(SOC_reproj) 
SOC_clip <- st_intersection(SOC_reproj, extent)

plot(SOC_clip)

# find sum of total C
SOC_df <- as.data.frame(SOC_clip)
SOC_df$total_C <- 0 # create new column to put total carbon value in
SOC_df$total_C <- rowSums(SOC_df[,5:9])

# join total_C to shape 
SOC_df <- SOC_df[,c(3, 11)]
SOC_total_C <- left_join(SOC_clip, SOC_df)

# transform into Raster and mask with landscape
SOC_raster <- rasterize(SOC_total_C, landscape_mask, field="total_C")
plot(SOC_raster)

#landscape_binomial <- as.data.frame(landscape_mask) %>% mutate(final_IC_map = ifelse(final_IC_map !=0, 1, 0)) %>% as.matrix()
#values(landscape_mask) <- landscape_binomial
#landscape_mask[landscape_mask==0] <- NA
total_C <- mask(SOC_raster, landscape_mask)
plot(total_C)

# ----------- 
# Calculate carbon and nitrogen pools, save plots, and write geotiff files

##--- SOM1surfC = 0.01 of total carbon
SOM1surfC <- total_C * 0.01
SOM1surfC[is.na(SOM1surfC)] <- 0
plot(SOM1surfC)

writeRaster(SOM1surfC, "FinalProducts/soils/SOM1surfC.tif", datatype='FLT4S', overwrite=T)

##--- SOM1soilC = 0.02 of total C
SOM1soilC <- total_C * 0.02
SOM1soilC[is.na(SOM1soilC)] <- 0

jpeg("soils/SOM1soilC_SOM1surfC.jpg")
par(mfrow=c(1,2), mar = c(3, 3, 5, 5))
plot(SOM1soilC, main = "SOM1soilC, g C/m2")
plot(SOM1surfC, main = "SOM1surfC, g C/m2")
dev.off()

writeRaster(SOM1soilC, "FinalProducts/soils/SOM1soilC.tif", datatype='FLT4S', overwrite=T)

##--- SOM2C = 0.59 of total C
SOM2C <- total_C * 0.59
SOM2C[is.na(SOM2C)] <- 0

writeRaster(SOM2C, "FinalProducts/soils/SOM2C.tif", datatype='FLT4S', overwrite=T)

##--- SOM3C = 0.38 of total C
SOM3C <- total_C * 0.38
SOM3C[is.na(SOM3C)] <- 0

jpeg("soils/SOM2C_SOM3C.jpg")
par(mfrow=c(1,2), mar = c(3, 3, 5, 5))
plot(SOM2C, main = "SOM2C, g C/m2")
plot(SOM3C, main = "SOM3C, g C/m2")
dev.off()

writeRaster(SOM3C, "FinalProducts/soils/SOM3C.tif", datatype='FLT4S', overwrite=T)

##--- SOM1surfN = 0.1 of SOM1surfC
SOM1surfN <- SOM1surfC * 0.1
SOM1surfN[is.na(SOM1surfN)] <- 0
writeRaster(SOM1surfN, "FinalProducts/soils/SOM1surfN.tif", datatype='FLT4S', overwrite=T)

## SOM1soilN = 0.1 of SOM1soilC
SOM1soilN <- SOM1soilC * 0.1
SOM1soilN[is.na(SOM1soilN)] <- 0

jpeg("soils/SOM1soilN_SOM1surfN.jpg")
par(mfrow=c(1,2), mar = c(3, 3, 5, 5))
plot(SOM1soilN, main = "SOM1soilN, g N/m2")
plot(SOM1surfN, main = "SOM1surfN, g N/m2")
dev.off()

writeRaster(SOM1soilN, "FinalProducts/soils/SOM1soilN.tif", datatype='FLT4S', overwrite=T)

##--- SOM2N = 0.04 of SOM2C
SOM2N <-  SOM2C * 0.04
SOM2N[is.na(SOM2N)] <- 0
writeRaster(SOM2N, "FinalProducts/soils/SOM2N.tif", datatype='FLT4S', overwrite=T)

##--- SOM3N = 0.118 of SOM3C 
SOM3N <- SOM3C * 0.118
SOM3N[is.na(SOM3N)] <- 0

jpeg("soils/SOM2N_SOM3N.jpg")
par(mfrow=c(1,2), mar = c(3, 3, 5, 5))
plot(SOM2N, main = "SOM2N, g N/m2")
plot(SOM3N, main = "SOM3N, g N/m2")
dev.off()

writeRaster(SOM3N, "FinalProducts/soils/SOM3N.tif", datatype='FLT4S', overwrite=T)

rm(SOM1soilC, SOM1soilN, SOM1surfC, SOM1surfN, SOM2C, SOM2N, SOM3C, SOM3N, total_C,
   SOC_df, SOC_reproj, SOC_shp, SOC_total_C, extent, landscape_binomial, SOC_clip, SOC_raster)

##------------------------------------------------------------------------
##------------------------------------------------------------------------
# Field capacity and wilting point - calculated from STATSGO data
statsgo_shp <- readOGR("soils/statsgo/statsgo_shape.shp")
statsgo_shp <- spTransform(statsgo_shp, CRS="+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")

statsgo_ca_chorizon <- read.csv("soils/statsgo/statsgo_CA_chorizon.csv")
statsgo_or_chorizon <- read.csv("soils/statsgo/statsgo_OR_chorizon.csv")
chorizon <- rbind(statsgo_ca_chorizon, statsgo_or_chorizon)

statsgo_ca_component <- read.csv("soils/statsgo/statsgo_CA_component.csv")
statsgo_or_component <- read.csv("soils/statsgo/statsgo_OR_component.csv")
component <- rbind(statsgo_ca_component, statsgo_or_component)

# having some issues with MUKEY being character
statsgo_shp$MUKEY <- as.integer(statsgo_shp[[4]]) 

statsgo <- statsgo_shp %>% 
  as.data.frame() %>% 
  select(MUKEY) %>% 
  left_join(component, by = c("MUKEY" = "Field108")) %>%
  select(MUKEY, Field109) %>%
  left_join(chorizon, by= c("Field109" = "Field170")) %>%
  select(MUKEY, Field109, Field92, Field95) %>%
  group_by(MUKEY) %>% 
  summarise(wthirdbar = mean(Field92, na.rm=T), wfifteenbar = mean(Field95, na.rm=T)) %>% 
  distinct() %>%
  mutate(wthirdbar = wthirdbar * 0.01, wfifteenbar = wfifteenbar * 0.01) %>%
  # assigning field capacity and wilting point with value of 0 mean of 
  # each respective soil property within landscape buffer
  mutate(wthirdbar = ifelse(is.na(wthirdbar), mean(wthirdbar, na.rm=T), wthirdbar)) %>%
  mutate(wfifteenbar = ifelse(is.na(wfifteenbar), mean(wfifteenbar, na.rm=T), wfifteenbar))

final_shp <- merge(statsgo_shp, statsgo, by="MUKEY")
wilt_point <- rasterize(final_shp, landscape_mask, field="wfifteenbar")
field_cap <- rasterize(x=final_shp, y=landscape_mask, field="wthirdbar")

wilt_point_mask <- mask(wilt_point, landscape_mask) 
field_cap_mask <- mask(field_cap, landscape_mask)

wilt_point_mask[is.na(wilt_point_mask)] <- 0
field_cap_mask[is.na(field_cap_mask)] <- 0

# plot
jpeg("soils/fieldCap_wiltPoint.jpg")
par(mfrow=c(1,2), mar = c(3, 3, 5, 5))
plot(field_cap_mask, main = "Field capacity,\nexpressed as a fraction")
plot(wilt_point_mask, main = "Wilting point,\nexpressed as a fraction")
dev.off()

# write to file
writeRaster(wilt_point_mask, "FinalProducts/soils/wilting_point.tif", datatype='FLT8S', overwrite=T)
writeRaster(field_cap_mask, "FinalProducts/soils/field_capacity.tif", datatype='FLT8S', overwrite=T)

## UNSURE ABOUT THIS - PRODUCES PRODUCT GREATER THAN 1
# # need to multiply both by soil depth to get wilting point and field capacity
# soil_depth <- raster("FinalProducts/soils/soil_depth.tif")
# final_wilt_point <- wilt_point_mask * soil_depth
# final_field_cap <- field_cap_mask * soil_depth

## storm flow and base flow - Melissa said to just set both to 0.3 initially
landscape_df <- as.data.frame(landscape_mask)
landscape_df[is.na(landscape_df)] <- 0
flow_df <- landscape_df %>% mutate(final_IC_map = ifelse(final_IC_map != 0, 0.3, 0))

flow_df[is.na(flow_df)] <- 0
flow_matrix <- as.matrix(flow_df)

base_flow <- landscape_mask
storm_flow <- landscape_mask

values(base_flow) <- flow_matrix
values(storm_flow) <- flow_matrix
plot(base_flow)
plot(storm_flow)

writeRaster(base_flow, "FinalProducts/soils/base_flow.tif",datatype='FLT8S', overwrite=T)
writeRaster(storm_flow, "FinalProducts/soils/storm_flow.tif",datatype='FLT8S', overwrite=T)


