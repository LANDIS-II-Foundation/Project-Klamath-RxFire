library(raster)
library(sf)
rasterOptions(tmpdir = "/Volumes/Alison.Deak_440-223-4897/R_temp_dir")

dir <- "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/"
w_dir <- "/Users/alisondeak/Desktop/Landis-Siskiyous/SCRPPLE/Inputs/suppression_inputs/"
output_dir <- "/Users/alisondeak/Desktop/Landis-Siskiyous/SCRPPLE/Inputs/suppression_inputs/working_spatial_data/"

mask <- raster(paste0(dir, "ecoregions/spatial_data/ecoregions.tif"))
mask[mask==0,] <- NA

landscape <- st_read(paste0(dir, "landscape/LandscapeExtent/LandscapeExtent.shp"))
landscape <- st_transform(landscape, crs=st_crs(mask))

# template <- raster(crs=crs(mask), res=res(mask), ext=extent(mask))

# Wilderness -----------------------------------------
# No suppression if lightning ignition (0)
# data downloaded from Wilderness.net
wilderness <- st_read(paste0(w_dir, "Wilderness_Areas/Wilderness_Areas_071921.shp"))
wilderness_reproj <- st_transform(wilderness, crs=st_crs(mask))
wilderness_clip <- st_intersection(wilderness_reproj, landscape)
wilderness_rast <- rasterize(wilderness_clip, mask, mask=T)
wilderness_rast[wilderness_rast==0] <- NA
wilderness_rast[wilderness_rast!=0] <- 0  #zero because we don't want suppression here
writeRaster(wilderness_rast, paste0(output_dir, "wilderness.tif"))
rm(wilderness, wilderness_reproj, wilderness_clip)

## WUI -----------------------------------------------
# max suppression (3) 
# data downloaded from http://silvis.forest.wisc.edu/data/wui-change/
ca_wui <- st_read(paste0(w_dir, "ca_wui_cp12/ca_wui_cp12.shp"))
or_wui <- st_read(paste0(w_dir, "or_wui_cp12/or_wui_cp12.shp"))
wui <- rbind(ca_wui, or_wui)
wui_reproj <- st_transform(wui, crs=st_crs(mask))
wui_reproj <- st_make_valid(wui_reproj)
wui_clip <- st_intersection(wui_reproj, landscape)
wui_clip <- wui_clip[,11]

wui_rast <- rasterize(wui_clip, mask, field="WUIFLAG10")
wui_mask <- mask(wui_rast, mask)
wui_mask[wui_mask==0] <- NA
wui_mask[wui_mask!=0] <- 3
writeRaster(wui_mask, paste0(output_dir, "wui.tif"))
rm(ca_wui, or_wui, wui, wui_reproj, wui_clip)

## Roads ----------------------------------------------
# moderate suppression (2) - Most often used as control lines by wildland firefighters
# data downloaded from US Census Bureau

# road_a <- st_read(paste0(w_dir, "tl_2021_06015_roads/tl_2021_06015_roads.shp"))
# road_b <- st_read(paste0(w_dir, "tl_2021_06023_roads/tl_2021_06023_roads.shp"))
# road_c <- st_read(paste0(w_dir, "tl_2021_41015_roads/tl_2021_41015_roads.shp"))
# road_d <- st_read(paste0(w_dir, "tl_2021_41033_roads/tl_2021_41033_roads.shp"))
# 
# roads <- rbind(road_a, road_b, road_c, road_d)
# roads_reproj <- st_transform(roads, crs=st_crs(mask))
# st_write(roads_reproj, paste0(output_dir, "roads_reproj.shp"), overwrite=T)
# roads_clip <- st_intersection(roads_reproj, landscape)
# roads_clip <- roads_clip[,5]

# I ended up creating the roads raster in QGIS because R was really struggling to reproject
roads <- raster(paste0(w_dir, "roads_rasterized.tif"))
roads_clip <- raster::crop(roads, mask, snap="near")
roads_reproj <- projectRaster(roads_clip, mask)
roads_mask <- mask(roads_reproj, mask)
roads_mask[roads_mask==0] <- NA
roads_mask[roads_mask != 0] <- 2
plot(roads_mask)
writeRaster(roads_mask, paste0(output_dir, "roads.tif"), overwrite=T)

## Ridgelines -----------------------------------------
# light suppression  (1) - often used by firefighters as control lines
# raster created in ArcMap 
ridgelines <- raster(paste0(output_dir, "ridgelines.tif"))
ridgelines_reproj <- projectRaster(ridgelines, mask)
ridgelines_crop <- crop(ridgelines_reproj, mask)
ridgelines_mask <- mask(ridgelines_crop, mask)
ridgelines_mask[ridgelines_mask!=0] = 1
ridgelines_mask[ridgelines_mask==0] <- NA
plot(ridgelines_mask)

## write rasters --------------------------------------
# human accidental 
unnatural <- mosaic(wilderness_rast, wui_mask, roads_mask, ridgelines_mask, fun="max")
writeRaster(unnatural, "/Users/alisondeak/Desktop/Landis-Siskiyous/SCRPPLE/Outputs/Rx&human_suppression.tif")

# lightning
natural <- mosaic(unnatural, wilderness_rast, fun="min")
writeRaster(natural, "/Users/alisondeak/Desktop/Landis-Siskiyous/SCRPPLE/Outputs/lightning_suppression.tif")

# Rx fire
mask <- raster(paste0(dir, "landscape/landscape_mask.tif"))
mask[mask] <- 0
plot(mask)

writeRaster(mask, paste0(dir, "SCRPPLE/Outputs/Rx_suppression.tif"))

