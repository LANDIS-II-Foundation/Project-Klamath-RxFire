# Prepping raster data to input to Mortality script 

library(raster)
library(tidyverse)
library(ncdf4)
library(sf)
library(geosphere)

options(scipen=999) # This removes scientific notation in printing
# rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

w.dir <- "C://Users/adeak/Desktop/LANDIS/SCRPPLE/"
# scrpple.dir <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/"

mask <- raster(paste0(w.dir, "ecoregions.tif"))
mask[mask==0] <- NA
plot(mask)

## Mortality  -----------------------------------------------------------------------------------
perimeter_map <- st_read(paste0(w.dir,"data/consecutive_fires.shp"))
# subset fires found to be within perimeter and with spread data
fires <- c("Buckskin-NA", "Chetco Bar", "Taylor Creek", "Klondike")
perims_sub <- subset(perimeter_map, perimeter_map$firenam %in% fires)
# perims_df <- as.data.frame(perims_sub)[,1:11]
# write.csv(perims_df, paste0(w.dir, "perims.csv")) 
### putting day of spread number into csv and fire names without spaces in Excel then reloading
# perims_df <- as.data.frame(perims_sub)[,1:11]
# write.csv(perims_df, paste0(w.dir, "perims.csv")) 
perims_df <- read.csv(paste0(w.dir, "data/perims.csv")) # reloading 
perims_sub <- left_join(perims_sub, perims_df, by="ID_dtrc")
perims_sub <- subset(perims_sub, !is.na(perims_sub$loop_name))

#change date loaded format to be able to use a raster name
perims_sub$new_date <- strptime(perims_sub$dt_rcrd, "%Y-%m-%d")
perims_sub$new_date <- format(perims_sub$new_date, "%Y%m%d")

# 2015 ----------
buckskin <- raster(paste0(w.dir,"data/RAVG/Buckskin/or4210312387320150611_20140908_20150911_rdnbr_cbi4.tif"))
buckskin <- projectRaster(buckskin, mask)
buckskin <- crop(buckskin, mask)
buckskin[buckskin==0] <- NA
# 2017 ----------
chetco <- raster(paste0(w.dir,"data/RAVG/ChetcoBar/or4229712395420170712_20151013_20171018_rdnbr_cbi4.tif"))
chetco <- projectRaster(chetco, mask)
chetco <- crop(chetco, mask)
chetco[chetco==0] <- NA
# 2018-------
taylor <- raster(paste0(w.dir,"data//RAVG/TaylorCreek/or4252812357120180715_20160913_20180919_rdnbr_cbi4.tif"))
taylor <- projectRaster(taylor, mask)
taylor <- crop(taylor, mask)
taylor[taylor==0] <- NA
klondike <- raster(paste0(w.dir,"data/RAVG/Klondike/or4237012386020180716_20171018_20181106_rdnbr_cbi4.tif"))
klondike <- projectRaster(klondike, mask)
klondike <- crop(klondike, mask)
klondike[klondike==0] <- NA

rdnbr_stack <- stack(buckskin, chetco, taylor, klondike)
unique(perims_sub$loop_name)
names(rdnbr_stack) <- c("BuckSkin", "Chetco.Bar", "Taylor.Creek", "Klondike")

rm(perimeter_map, fires, perims_df, buckskin, chetco, taylor, klondike)

# create individual rasters of each days spread 
daily_rdnbr <- stack()
daily_mortality <- stack()
spread_mosaic <- raster(ext=extent(mask), crs=crs(mask), resolution=res(mask))
spread_names <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("ID", "fire_name", "spread_date", "spread_dir"))))
spread_azimuth <- raster(ext=extent(mask), crs=crs(mask), resolution=res(mask))
for (f in unique(perims_sub$loop_name)) {
  fire <- subset(perims_sub, perims_sub$loop_name == f)
  rdnbr <- rdnbr_stack[[f]]
  for (n in unique(fire$new_nam.x)) {
    consec_spread <- subset(fire, fire$new_nam.x == n)
    for (d in 1:nrow(consec_spread)) {
    one_day <- consec_spread[d,]
    one_day_rdnbr <- mask(rdnbr, one_day)
    name <- one_day$loop_name
    date <- one_day$new_date
    daily_rdnbr <- stack(daily_rdnbr, one_day_rdnbr)
    index <- nlayers(daily_rdnbr)
    names(daily_rdnbr)[[index]] <- paste0(name, "_", date)
    if (one_day$day_of_spread > 1) {
      as_date <- as.Date(date, "%Y%m%d")
      prior_day <- as_date - 1
      prior_day <- format(prior_day, "%Y%m%d")
      prior_day_name <- paste0(name,"_",prior_day)
      prior_day_shape <- consec_spread[consec_spread$matchnm==paste0(one_day$firenam,"-", as.Date(prior_day, "%Y-%m-%d"))]
      prior_raster <- daily_rdnbr[[prior_day_name]]
      spread <- mask(one_day_rdnbr, prior_raster, inverse=T)
      final_spread <- crop(spread, mask) 
      daily_mortality <- stack(daily_mortality, final_spread)
      index <- nlayers(daily_mortality)
      names(daily_mortality)[[index]] <- paste0(name, "_", date)
      spread_mosaic <- mosaic(spread_mosaic, final_spread, fun="max")
      # Getting wind direction. This is inferred from direction of spread, where azimuth of spread = wind direction
      one_day_centroid <- st_centroid(one_day)
      prior_day_centroid <- st_centroid(prior_day_shape) # Finding centroid for estimation wind direction
      one_day_centroid <- st_transform(one_day_centroid, CRS("+proj=longlat +datum=WGS84")) # Need to convert back to lat/long to get azimuth direction
      prior_day_centroid <- st_transform(prior_day_centroid, CRS("+proj=longlat +datum=WGS84")) # Need to convert back to lat/long to get azimuth direction
      one_day_centroid_mat <- t(as.matrix(unlist(one_day_centroid[,15])))
      prior_day_centroid_mat <- t(as.matrix(unlist(prior_day_centroid[1,])))
      fire_azimuth <- bearingRhumb(prior_day_centroid_mat, one_day_centroid_mat) # find the azimuth of fire spread by Rhumb bearing. This is used as a proxy assumed wind direction
      daily_fire_azi <- final_spread
      values(daily_fire_azi) <- fire_azimuth
      daily_fire_azi <- mask(daily_fire_azi, final_spread)
      spread_azimuth <- raster::mosaic(spread_azimuth, daily_fire_azi, fun = "max")
      # create csv with fire IDs, name, and date of spread
      spread_names[index, 1] <- names(daily_mortality)[[index]]
      spread_names[index, 2] <- name
      spread_names[index, 3] <- date
      spread_names[index, 4] <- fire_azimuth
      }
    }
  }
}
plot(spread_mosaic)
plot(spread_azimuth)

spread_names$true_date <- as.Date(spread_names[,3], "%Y%m%d")
spread_names$year <- format(spread_names$true_date, "%Y")

spread_mosaic_masked <- mask(spread_mosaic, mask)
spread_mosaic_masked[is.na(spread_mosaic_masked)] <- 0

spread_azimuth_masked <- mask(spread_azimuth, mask)
spread_azimuth_masked[is.na(spread_azimuth_masked)] <- 0

writeRaster(spread_mosaic_masked, paste0(w.dir, "mortality/mortality_inputs/rdnbr_allyears.tif"), overwrite=T)
writeRaster(spread_azimuth_masked, paste0(w.dir, "mortality/mortality_inputs/fire_spread_azimuth.tif"), overwrite=T)
write.csv(spread_names, paste0(w.dir, "mortality/mortality_inputs/each_day_spread_rdnbr.csv"))

rm(n, fire, rdnbr, one_day, one_day_rdnbr, name, date, index, as_date, prior_day, 
   prior_day_name, prior_raster, spread, final_spread, f, d, one_day_centroid, 
   prior_day_centroid, prior_day_centroid_mat, one_day_centroid_mat, fire_azimuth, 
   daily_fire_azi, spread_mosaic, spread_azimuth)

## effective wind speed --------------------------------------------------------
ncin <- nc_open(paste0(w.dir, "data/terraclimate/windspeed_2015.nc"))
lon_2015 <- ncvar_get(ncin, "lon", verbose = F)
lat_2015 <- ncvar_get(ncin, "lat", verbose = F)
vs_2015 <- ncvar_get(ncin, "wind_speed")
nc_close(ncin)

ncin <- nc_open(paste0(w.dir, "data/terraclimate/windspeed_2017.nc"))
lon_2017 <- ncvar_get(ncin, "lon", verbose = F)
lat_2017 <- ncvar_get(ncin, "lat", verbose = F)
vs_2017 <- ncvar_get(ncin, "wind_speed")
nc_close(ncin)

ncin <- nc_open(paste0(w.dir, "data/terraclimate/windspeed_2018.nc"))
lon_2018 <- ncvar_get(ncin, "lon", verbose = F)
lat_2018 <- ncvar_get(ncin, "lat", verbose = F)
vs_2018 <- ncvar_get(ncin, "wind_speed")
nc_close(ncin)

# need to pull the windspeed data for each day of spread from the ncdf file then 
# create raster, reproject, and mask to daily spread.

# Start by transforming date of spread to julian day to index
spread_names$j_day <- format(spread_names$true_date,"%j")
wind_map <- raster(ext=extent(mask), crs=crs(mask), resolution=res(mask))

# pulling the windspeed for each day of spread from the ncdf file then creating raster for each day,
# reprojecting, masking by daily spread pixels, and mosaicing together to create single windspeed raster
for(d in 1:nrow(spread_names)) {
  one_day <- spread_names[d,]
  j_day <- as.numeric(one_day[ ,"j_day"])
  year <- one_day[ ,"year"]
  index <- one_day[ ,"ID"]
  daily_rdnbr <- daily_mortality[[index]]
  if(year==2015) {
    wind_raster <- raster(t(vs_2015[,,j_day]), xmn=min(lon_2015), xmx=max(lon_2015), ymn=min(lat_2015), ymx=max(lat_2015), 
                          crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    reproj <- projectRaster(wind_raster, mask, method="bilinear")
    wind_mask <- mask(reproj, daily_rdnbr)
    crop <- crop(wind_mask, mask)    
  }
  if(year==2017) {
    wind_raster <- raster(t(vs_2017[,,j_day]), xmn=min(lon_2017), xmx=max(lon_2017), ymn=min(lat_2017), ymx=max(lat_2017), 
                crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    reproj <- projectRaster(wind_raster, mask, method="bilinear")
    wind_mask <- mask(reproj, daily_rdnbr)
    crop <- crop(wind_mask, mask)
  }
  if(year==2018) {
    wind_raster <- raster(t(vs_2018[,,j_day]), xmn=min(lon_2018), xmx=max(lon_2018), ymn=min(lat_2018), ymx=max(lat_2018), 
                          crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    reproj <- projectRaster(wind_raster, mask, method="bilinear")
    wind_mask <- mask(reproj, daily_rdnbr)
    crop <- crop(wind_mask, mask)    
  }
  wind_map <- mosaic(wind_map, crop, fun="max")
}

plot(wind_map)
wind_map <- mask(wind_map, mask)
wind_map[is.na(wind_map)] <- 0
writeRaster(wind_map, paste0(scrpple.dir, "SCRPPLE/mortality_inputs/wind_speed.tif"), overwrite=T)

rm(lon_2015, lat_2015, vs_2015, lon_2017, lat_2017, vs_2017, lon_2018, lat_2018, vs_2018,
   one_day, j_day, year, index, daily_rdnbr, wind_raster, reproj, wind_mask, crop, d)


##--------------------------------------------------------------------------
## Caculating effective windspeed, i.e., the windspeed in direction of spread 
# equation from https://www.fs.fed.us/rm/pubs_journals/2002/rmrs_2002_nelson_r001.pdf

# We use wind speed, uphill azimuth and slope maps to calculate the direction of windspeed
wind_map <- raster(paste0(w.dir, "mortality/mortality_inputs/wind_speed.tif"))
uphill_azi_map<- raster(paste(w.dir,"uphill_slope_azimuth.tif",sep=""))
slope_map <-  raster(paste(w.dir,"slope.tif",sep=""))

spread_mosaic_masked[spread_mosaic_masked==0] <- NA
spread_azimuth_masked[spread_azimuth_masked==0] <- NA

U_b <- 5 # This changes based on fire severity. Combustion buoyancy.
relative_wd <- spread_azimuth_masked - uphill_azi_map
effective_wind_speed <- 
  U_b * ((wind_map / U_b) ^ 2 + 2 * (wind_map / U_b) * 
           sin(slope_map) * cos(relative_wd) + (sin(slope_map) ^ 2) ^ 0.5)

effective_wind_speed <- mask(effective_wind_speed, spread_mosaic)
effective_wind_speed[is.na(effective_wind_speed)] <- NA
plot(effective_wind_speed)
effective_wind_speed[is.na(effective_wind_speed)] <- 0

writeRaster(effective_wind_speed, paste0(w.dir, "mortality/mortality_inputs/effective_wind_speed.tif"), overwrite=T)
rm(ncin, perims_sub, rdnbr_stack, U_b)

# Climatic Water Deficit  --------------------------------------------------------------------
# we need to sum CWD for each year prior to our fire data above 
# then mosaic data together to create single raster 

## load netCDF file & data - looping through lines 116-132 by hand for each year needed
ncin <- nc_open(paste0(w.dir, "terraclimate/cwd_monthly_2016.nc"))
lon <- ncvar_get(ncin, "lon", verbose = F)
lat <- ncvar_get(ncin, "lat", verbose = F)
cwd <- ncvar_get(ncin, "def")
dim(cwd)
nc_close(ncin)

for (n in 1:12) {
  r <- raster(t(cwd[,,n]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  reproj <- projectRaster(r, mask, method="bilinear")
  crop <- crop(reproj, mask)
  assign(paste0("cwd_", n), mask(crop, mask))
}
plot(cwd_6)
cwd_stack <- stack(cwd_1, cwd_2, cwd_3, cwd_4, cwd_5, cwd_6, cwd_7, cwd_8, cwd_9, cwd_10, cwd_11, cwd_12)
cwd_2016 <- sum(cwd_stack)

writeRaster(cwd_2014, paste0(w.dir,"terraclimate/cwd_2014.tif"),overwrite=T)
writeRaster(cwd_2016, paste0(w.dir,"terraclimate/cwd_2016.tif"),overwrite=T)
writeRaster(cwd_2017, paste0(w.dir,"terraclimate/cwd_2017.tif"),overwrite=T)

rm(cwd_1, cwd_2, cwd_3, cwd_4, cwd_5, cwd_6, cwd_7, cwd_8, cwd_9, cwd_10, cwd_11, cwd_12,
   cwd_2014, cwd_2016, cwd_2017)

# mask each years CWD and AET with following years fire severity found above
# mosaic resulting rasters together
cwd_2014 <- raster(paste0(w.dir,"terraclimate/cwd_2014.tif"))
cwd_2016 <- raster(paste0(w.dir,"terraclimate/cwd_2016.tif"))
cwd_2017 <- raster(paste0(w.dir,"terraclimate/cwd_2017.tif"))
years <- c("2015", "2017", "2018")

cwd_mosaic <- raster(ext=extent(mask), crs=crs(mask), resolution=res(mask))
for (s in 1:nlayers(cwd_stack)) {
  one_cwd <- cwd_stack[[s]]
  for(y in years) {
    one_year_spread <- subset(spread_names, spread_names$year == y)
    spread_sub <- subset(daily_mortality, one_year_spread$ID)
    one_year_mosaic <- stackApply(spread_sub, indices= 1, fun="max")
    cwd_mask <- mask(one_cwd, one_year_mosaic)
    cwd_mosaic <- mosaic(cwd_mosaic, cwd_mask, fun="max")
  }
}
plot(cwd_mosaic)
cwd_mosaic[is.na(cwd_mosaic)] <- 0
writeRaster(cwd_mosaic, paste0(scrpple.dir, "SCRPPLE/mortality_inputs/cwd_allyears.tif"), overwrite=T)

## Actual Evapotranspiration ----------------------------------------------------------
## load netCDF file & data - looping through lines 57-76 by hand for each year 2013-2016
ncin <- nc_open(paste0(w.dir, "terraclimate/aet_monthly_2017.nc"))
lon <- ncvar_get(ncin, "lon", verbose = F)
lat <- ncvar_get(ncin, "lat", verbose = F)
aet <- ncvar_get(ncin, "aet")
dim(aet)
nc_close(ncin)

## calculate and plot the mean of monthly data over 2014
for (n in 1:12) {
  r <- raster(t(aet[,,n]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  reproj <- projectRaster(r, mask, method="bilinear")
  crop <- crop(reproj, mask)
  assign(paste0("aet_", n), mask(crop, mask))
}

aet_stack <- stack(aet_1, aet_2, aet_3, aet_4, aet_5, aet_6, aet_7, aet_8, aet_9, aet_10, aet_11, aet_12)
aet_2017 <- sum(aet_stack)
plot(aet_2017)

writeRaster(aet_2014, paste0(w.dir,"terraclimate/aet_2014.tif"),overwrite=T)
writeRaster(aet_2016, paste0(w.dir,"terraclimate/aet_2016.tif"),overwrite=T)
writeRaster(aet_2017, paste0(w.dir,"terraclimate/aet_2017.tif"),overwrite=T)

rm(aet_1, aet_2, aet_3, aet_4, aet_5, aet_6, aet_7, aet_8, aet_9, aet_10, aet_11, aet_12,
   aet_2014, aet_2016, aet_2017)

# mask each years AET with following years fire severity found above
# mosaic resulting rasters together
aet_2014 <- raster(paste0(w.dir,"terraclimate/aet_2014.tif"))
aet_2016 <- raster(paste0(w.dir,"terraclimate/aet_2016.tif"))
aet_2017 <- raster(paste0(w.dir,"terraclimate/aet_2017.tif"))
aet_mosaic <- raster(ext=extent(mask), crs=crs(mask), resolution=res(mask))
for (s in 1:nlayers(aet_stack)) {
  one_cwd <- aet_stack[[s]]
  for(y in years) {
    one_year_spread <- subset(spread_names, spread_names$year == y)
    spread_sub <- subset(daily_mortality, one_year_spread$ID)
    one_year_mosaic <- stackApply(spread_sub, indices= 1, fun="max")
    aet_mask <- mask(one_cwd, one_year_mosaic)
    aet_mosaic <- mosaic(aet_mosaic, aet_mask, fun="mean")
  }
}
plot(aet_mosaic)
aet_mosaic[is.na(aet_mosaic)] <- 0
writeRaster(aet_mosaic, paste0(scrpple.dir, "SCRPPLE/mortality_inputs/aet_allyears.tif"), overwrite=T)


## fine fuels ------------------------------------------------------------------
# Attach LANDFIRE fuel loading info based on fuel type number
# I selected columns I thought would define fine fuels, but these can change if we need. Units are tons/acre
fccs.rast <- raster(paste0(w.dir, "fccs_clip/fccs_clip.tif")) # Had to clip FCCS data in ArcGIS
fccs.rast <- crop(fccs.rast, spread_mosaic)
fccs.rast <- mask(fccs.rast, spread_mosaic) 

fccs.data <- read.csv(paste0(w.dir, "landfire_FCCS.csv"))
fccs <- as.data.frame(fccs.rast) %>% left_join(fccs.data, by=c("fccs_clip" = "Value"))
fccs.loading <- fccs[,c("fccs_clip", "LLM_tpa", "Ground_tpa")] # only kept fine fuels 
fccs.loading.total <- rowSums(fccs.loading[,2:length(fccs.loading)])
fccs.loading.total <- fccs.loading.total * 224.17 #convert from tons/acre to g/m-2
fccs.finefuels <- data.frame(fccs.loading[,1], fccs.loading.total)
colnames(fccs.finefuels) <- c("fueltype", "finefuels_loading")
summary(fccs.finefuels)

fuel_map <- spread_mosaic_masked
values(fuel_map) <- fccs.finefuels$finefuels_loading

writeRaster(fuel_map, paste0(scrpple.dir, "SCRPPLE/mortality_inputs/fine_fuels.tif"), overwrite=T)


## Ladder fuels------------------------------------------------------
# Need to determine what species are ladder fuels


