library(raster)
library(tidyverse)
library(sf)

ign <- st_read("/Volumes/Alison.Deak_440-223-4897/LargeDataForLandis/SCRPPLE/Short_ignitions_CA_OR/Short_ignitions_CA_OR.shp")
landscape <- st_read("/Users/alisondeak/Desktop/Landis-Siskiyous/landscape/landscape_shape/landscape_shape.shp")

landscape_reproj <- st_transform(landscape, crs = crs(ign))
ign_clip <- st_intersection(ign, landscape_reproj) %>% as.data.frame() %>%
  select(FIRE_YEAR, NWCG_CAUSE, FIRE_SIZE) %>%
  mutate(FIRE_SIZE = FIRE_SIZE * 0.404686)

fires_annually <- ign_clip %>%
  group_by(FIRE_YEAR) %>%
  summarise(no_fires= n(), 
            max_size= max(FIRE_SIZE), 
            average_size= mean(FIRE_SIZE),
            median_size= median(FIRE_SIZE))

boxplot(fires_annually$no_fires)
boxplot(fires_annually$max_size)
boxplot(fires_annually$average_size)
boxplot(fires_annually$median_size)



setwd("/Volumes/Alison.Deak_440-223-4897/LargeDataForLandis")
rasterOptions(tmpdir = "/Volumes/Alison.Deak_440-223-4897/R_temp_dir")

mtbs_files <- list.files("mtbs_severity_data", full.names = TRUE)
mtbs_names <- list(str = str_split(mtbs_files, '[_]', simplify = T)[,5])
mtbs_years <- list(year = str_split(mtbs_names$str, '[.]', simplify = T)[,1])

mask <- raster("/Volumes/Alison.Deak_440-223-4897/SCRPPLE/landscape_mask.tif")
mask <- projectRaster(mask, crs = CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


i=11
mtbs_stack <- stack()
for (i in 1:length(mtbs_files)) {
  r <- raster(mtbs_files[[i]])
  reproj <- projectRaster(r, mask, method = "ngb")
  crop <- crop(reproj, mask)
  mtbs_stack <- stack(mtbs_stack, crop)
}

plot(mtbs_stack)

