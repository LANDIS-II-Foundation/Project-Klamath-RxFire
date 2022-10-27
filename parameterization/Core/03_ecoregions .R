
## ---------------------------------------------------------------
## Script to make climate region and LEMMA raster files extent, CRS, and resolution match
## Begins with inputting TIF files of both 
## ---------------------------------------------------------------

## outputs from first part of script
library(raster)
library(sf)
library(rgdal)

wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous"
setwd(wd)
removeTmpFiles(0.5) # good practice - raster files fill up temp folder FAST

# Used external hard drive to avoid filling up temporary files
rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

# input and plot climate regions
ClimRegs <- raster("climate_regions/Kmeans_temp_precip.tif", band=2)
plot(ClimRegs)

# reproject raster to desired CRS and resolution
desired_CRS <- '+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'
ClimRegsReproj <- projectRaster(ClimRegs, crs=desired_CRS, method = 'ngb', res=30)
plot(ClimRegsReproj)

# input landscape shape file and clip 
landscapeShFile <- "FinalProducts/landscape_shape/landscape_shape.shp"
landscape <- st_read(landscapeShFile)
desired_crs <- crs("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
landscape.reproj <- st_transform(landscape, desired_crs)
landscape.reproj <- as_Spatial(landscape.reproj)
plot(landscape.reproj)

writeOGR(landscape.reproj, layer="US_L4CODE", driver="ESRI Shapefile",
        "FinalProducts/Final_landscape_shp/Final_landscape.shp")

rm(landscapeShFile, landscape)

# clip to landscape shape file
maskedClimRegs <- mask(x = ClimRegsReproj, mask = landscape.reproj)
plot(maskedClimRegs)

# fill in NaNs with zeroes to create 'box' around landscape
maskedClimRegs[is.na(maskedClimRegs)] <- 0 # remove NaNs

jpeg("climate_regions/final_climate_regions.jpg")
plot(maskedClimRegs,main="Climate Regions based on K-means clustering")
dev.off()

writeRaster(maskedClimRegs, filename="FinalProducts/FinalEcoRegions.tif", overwrite=TRUE)
rm(ClimRegs, ClimRegsCrop, landscape.reproj, desired_crs)

### -------------------------------------------------------------
# This part of the script is just to create an extent from the created rasters to extract future data

library(sp)
EcoRegions <- raster("FinalProducts/FinalEcoRegions.tif")
Extent <- extent(EcoRegions)
EcoRegion_vector <- as(Extent, 'SpatialPolygons')
crs(EcoRegion_vector) <- desired_CRS
plot(EcoRegion_vector, axes=TRUE)
plot(EcoRegions)
shapefile(x=EcoRegion_vector, file="LandscapeExtent/FinalProducts/LandscapeExtent.shp")

### ------------------------------------------------------------
# This part of the script is to create a shapefile to extract climate data with
ecoregions <- raster("FinalProducts/FinalEcoRegions.tif")
eco_shp <- rasterToPolygons(ecoregions, dissolve=TRUE)
plot(eco_shp)

shapefile(x=eco_shp, file="FinalProducts/ecoregion_shape/ecoregion_shape.shp", overwrite=TRUE)

