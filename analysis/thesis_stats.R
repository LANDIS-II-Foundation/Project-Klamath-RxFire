library(raster)
library(tidyverse)
library(sf)

ign <- st_read("/Volumes/Alison.Deak_440-223-4897/LargeDataForLandis/SCRPPLE/Short_ignitions_CA_OR/Short_ignitions_CA_OR.shp")
landscape <- st_read("/Users/alisondeak/Desktop/Landis-Siskiyous/landscape/landscape_shape/landscape_shape.shp")

landscape_reproj <- st_transform(landscape, crs = crs(ign))
ign_clip <- st_intersection(ign, landscape_reproj)
plot(ign_clip)

summary(ign_clip$FIRE_SIZE)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0      0.1      0.1   1117.4      0.5 499945.0 

ign_number_per_year <- ign_clip %>%
  group_by(FIRE_YEAR) %>%
  summarise(n=n(), sum = sum(FIRE_SIZE)) 
ign_number_per_year$sum

ign_stats <- ign_number_per_year %>%
  summarise(mean_area_burned=mean(sum), median_area_burned = median(sum)) %>%
  print()

summary(ign_number_per_year$n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   10.00   17.00   17.56   21.50   44.00 




