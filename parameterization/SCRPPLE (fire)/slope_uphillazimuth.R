library(raster)
library(tidyverse)
rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

out.dir <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/"
dem.dir <- "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/LargeDataForLandis/USGS DEMs/"

# Data moved to external hard drive ^^

dem1 <- raster(paste0(dem.dir,"USGS_13_n42w124_20210301.tif"))
dem2 <- raster(paste0(dem.dir,"USGS_13_n42w125.tif"))
dem3 <- raster(paste0(dem.dir,"USGS_13_n43w124_20210623.tif"))
dem4 <- raster(paste0(dem.dir,"USGS_13_n43w125.tif"))

dem <- merge(dem1,dem2,dem3,dem4)
plot(dem)

mask <- raster(paste0(out.dir, "FinalProducts/landscape_mask.tif"))

dem_reproj <- projectRaster(dem, mask, method = 'ngb', res=30)
dem_mask <- mask(dem_reproj, mask)
plot(dem_mask)

#compute slope
slope <- terrain(dem_mask, opt="slope", unit="degrees", neighbors=4)
slope[is.na(slope)] <- 0
writeRaster(slope, paste0(out.dir, "SCRPPLE/slope.tif"), datatype="INT4S", overwrite=T)

#compute aspect and convert to upslope aspect
aspect <- terrain(dem_mask, opt="aspect", unit="degrees",neighbors=4)
aspect <- aspect + 180
uphill_aspect <- function(aspect) {ifelse(aspect > 360, aspect-360, aspect)}
aspect <- calc(aspect, uphill_aspect)
plot(aspect)

writeRaster(aspect, paste0(out.dir, "SCRPPLE/uphill_slope_azimuth.tif"), datatype="INT4S", overwrite=T)

