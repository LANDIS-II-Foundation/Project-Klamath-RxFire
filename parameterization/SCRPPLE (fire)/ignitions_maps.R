## This script creates kernel density maps of human, lightning, and Rx ignitions to be used with SCRPPLE
# Rx ignitions start at line 76
library(plyr)
library(sf)
library(raster)
library(spatstat.geom)
library(maptools)
# library(terra)

options(scipen = 1000000)

# Set up directories
w.dir <- "/Volumes/Alison.Deak_440-223-4897/"
out.dir <- "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/"

# #### K. Short data. 
# ignition_dat <- st_read(paste0(w.dir, "LargeDataForLandis/SCRPPLE/Short_ignitions_CA_OR/Short_ignitions_CA_OR.shp", sep=""))
# ignition_dat <- st_transform(ignition_dat, "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
# plot(ignition_dat$geometry,axes=T)
# head(ignition_dat)
# 
# # subset by ignition type
# l_fire_dat <- ignition_dat[ignition_dat$NWCG_CAUSE == "Natural",]
# h_fire_dat <- ignition_dat[ignition_dat$NWCG_CAUSE == "Human",]

### Study area, convert to owin object
StudyArea <- st_read(paste0(out.dir, "landscape/landscape_shape/landscape_shape.shp"))
StudyArea <- st_transform(StudyArea, st_crs(ignition_dat))
SA <- as.owin(sf::st_as_sf(StudyArea))
plot(SA)

# # find ignitions within study area, convert to ppp object
# l_ign <- l_fire_dat[StudyArea,]
# plot(StudyArea$geometry,col="red", axes=T)
# plot(l_ign, add=T)
# l_ign <- as.ppp(st_coordinates(l_ign),W=SA)
# 
# h_ign <- h_fire_dat[StudyArea,]
# plot(StudyArea$geometry,col="red", axes=T)
# plot(h_ign, add=T)
# h_ign <- as.ppp(st_coordinates(h_ign),W=SA)
# 
# Window(l_ign) <- SA
# Window(h_ign) <- SA
#
# # find density of lightning ignitions and scale from 0 to 1
# density(l_ign)
# l_density <- density(l_ign, weights=1000000*4) 
# intensity(l_ign)
# range(l_density)
# l_d_r <- raster(l_density)
# plot(l_d_r)
# plot(l_ign, add=T)
#
# # resample to match resolution and extent of landscape
# mask <- raster(paste0(out.dir, "FinalProducts/landscape_mask.tif", sep=""))
# l_dens_rast <- resample(l_d_r, mask, method="ngb", res=30)
# l_dens_rast[is.na(l_dens_rast)]<-0
# plot(l_dens_rast)
# writeRaster(l_dens_rast, paste0(out.dir, "SCRPPLE/lightning_ignitions.tif"))
#
# find density of human ignitions and scale from 0 to 1
# h_density <- density(h_ign, weights=1000000*1.16)
# range(h_density)
# h_d_r <- raster(h_density)
# plot(h_ign, add=T)
# plot(h_d_r)
# 
# h_dens_rast <- resample(h_d_r, mask, method="ngb", res=30)
# h_dens_rast[is.na(h_dens_rast)]<-0
# plot(h_dens_rast)
# writeRaster(h_dens_rast, paste0(out.dir, "SCRPPLE/accidental_ignitions.tif"))

##--------------------------------------------------------
### Rx fire - data from NFPORS - 
# used roads to identify where Rx fires would most likely take place due to ease of access and containment
# created kernel density of roads
roads <- st_read(paste0(out.dir, "SCRPPLE/Inputs/suppression_inputs/roads_reproj.shp"))
roads_clip <- st_intersection(roads, StudyArea)

# Object "SA" created in lines 28-31
# Find coordinates of roads, convert to ppp object and define window using spat.stat package
road_points <- st_coordinates(st_line_sample(st_cast(roads_clip, "LINESTRING"), density = 30/1000)) # point per 30 m cell on landscape
road_ppp <- as.ppp(road_points, W=SA)
Window(road_ppp) <- SA 

StudyArea <- st_read(paste0(out.dir, "landscape/landscape_shape/landscape_shape.shp"))
StudyArea <- st_transform(StudyArea, st_crs(roads_clip))

# Create kernal density map
# scale from 0 to 1 using weights argument 
roads_density <- stats::density(road_ppp, weights=10000*2.1) 
intensity(road_ppp)
range(roads_density) # check correctly scaled

Rx_rast <- raster(roads_density)

# resample to match resolution and extent of landscape
mask <- raster(paste0(out.dir, "landscape/landscape_mask.tif", sep=""))
wilderness <- st_read("/Volumes/Alison.Deak_440-223-4897/MS Research/SCRPPLE/suppression/suppression_inputs/Wilderness_Areas/Wilderness_Areas_071921.shp")
wilderness_reproj <- st_transform(wilderness, crs = st_crs(mask))
Rx_ignitions <- mask(mask, wilderness_reproj, inverse = T)

Rx_ignitions[is.na(Rx_ignitions)] <- 0
plot(Rx_ignitions)

writeRaster(Rx_ignitions, paste0(out.dir, "SCRPPLE/Outputs/Rx_ignitions.tif"), overwrite=T)

## Ended up not being able to simulate enough ignitions with the ignitions map created
## using the below code; Switched to making all but wilderness areas open to Rx ignitions
# Rx_rast <- resample(Rx_rast, mask, method="ngb", res=30)
# Rx_rast[is.na(Rx_rast)]<-0
# 
# plot(Rx_rast)
# points(road_points)



