## Script to calculate temperature curves based on species ranges

# Basically, you plug in your species distribution shapefile, and your WorldClim rasters, clip those rasters to
# to your shapefile then take a random sample of 10,000 points within each raster. In Lines 174-193, you'll input your AmeriFlux data
# and do a regression between air temperature and soil temperature to calculate the equation for transforming air to soil temp.
# You'll then create a frequency table from those calculated soil temperatures that will then be fit to a nonlinear regression model
# to calculate the coefficients for the temperature curve.

library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(nls2)

# spp_dir <- "C://Users/EWP/Desktop/temp_curve/CEVE"
spp_dir <- "C://Users/EWP/Desktop/temp_curve/ARME"
# spp_dir <- "C://Users/EWP/Desktop/temp_curve/VAPA"
# spp_dir <- "C://Users/EWP/Desktop/temp_curve/PSME"
setwd(spp_dir)

# species range shapefile
# spp_bound <- readOGR("CEVE_distribution.shp)
spp_bound <- readOGR("ARME_distribution.shp")
# spp_bound <- readOGR("VAPA_distribution.shp")
# spp_bound <- readOGR("pseumenz.shp")

## --------------------------------------------------------------
## first input needed data from World Clim

Tmax_dir<-("C://Users/EWP/Desktop/temp_curve/wc2.1_30s_tmax")
setwd(Tmax_dir)
Jun_tMax <- raster("wc2.1_30s_tmax_06.tif")
Jul_tMax <- raster("wc2.1_30s_tmax_07.tif")
Aug_tMax <- raster("wc2.1_30s_tmax_08.tif")
Sep_tMax <- raster("wc2.1_30s_tmax_09.tif")

Tavg_dir<-("C://Users/EWP/Desktop/temp_curve/wc2.1_30s_tavg")
setwd(Tavg_dir)
Apr_tavg <- raster("wc2.1_30s_tavg_04.tif")
May_tavg <- raster("wc2.1_30s_tavg_05.tif")
Jun_tavg <- raster("wc2.1_30s_tavg_06.tif")
Jul_tavg <- raster("wc2.1_30s_tavg_07.tif")
Aug_tavg <- raster("wc2.1_30s_tavg_08.tif")
Sep_tavg <- raster("wc2.1_30s_tavg_09.tif")


# reproject shapefile to match rasters
setwd(spp_dir)
spp_bound_WGS84 = spTransform(spp_bound, crs(Jul_tMax))
plot(spp_bound_WGS84)

##----------------------------------------------------------------
## Mask and crop shapefile

# function to mask and crop climate data with shapefile
mask_and_crop <- function(temp) {
  mask <- mask(temp, spp_bound_WGS84)
  crop <- crop(mask, spp_bound_WGS84)
  return(crop)
}
jun_tmax_crop <- mask_and_crop(Jun_tMax)
jul_tmax_crop <- mask_and_crop(Jul_tMax)
aug_tmax_crop <- mask_and_crop(Aug_tMax)
sep_tmax_crop <- mask_and_crop(Sep_tMax)
apr_tavg_crop <- mask_and_crop(Apr_tavg)
may_tavg_crop <- mask_and_crop(May_tavg)
jun_tavg_crop <- mask_and_crop(Jun_tavg)
jul_tavg_crop <- mask_and_crop(Jul_tavg)
aug_tavg_crop <- mask_and_crop(Aug_tavg)
sep_tavg_crop <- mask_and_crop(Sep_tavg)

rm(Jul_tMax, Aug_tMax, Sep_tMax, Apr_tavg, May_tavg, Jun_tavg, Jul_tavg, Aug_tavg, Sep_tavg, Tavg_dir, Tmax_dir, spp_bound)


##--------------------------------------------------------------------------
## randomly sample rasters for point values
tmax_pts_jun <- as.data.frame(sampleRandom(jun_tmax_crop, 10000))
tmax_pts_jul <- as.data.frame(sampleRandom(jul_tmax_crop, 10000))
tmax_pts_aug <- as.data.frame(sampleRandom(aug_tmax_crop, 10000))
tmax_pts_sep <- as.data.frame(sampleRandom(sep_tmax_crop, 10000))
tavg_pts_apr <- as.data.frame(sampleRandom(apr_tavg_crop, 10000))
tavg_pts_may <- as.data.frame(sampleRandom(may_tavg_crop, 10000))
tavg_pts_jun <- as.data.frame(sampleRandom(jun_tavg_crop, 10000))
tavg_pts_jul <- as.data.frame(sampleRandom(jul_tavg_crop, 10000))
tavg_pts_aug <- as.data.frame(sampleRandom(aug_tavg_crop, 10000))
tavg_pts_sep <- as.data.frame(sampleRandom(sep_tavg_crop, 10000))

colnames(tavg_pts_apr) <- 'temp'
colnames(tavg_pts_may) <- 'temp'
colnames(tavg_pts_jun) <- 'temp'
colnames(tavg_pts_jun) <- 'temp'
colnames(tavg_pts_jul) <- 'temp'
colnames(tavg_pts_aug) <- 'temp'
colnames(tavg_pts_sep) <- 'temp'
colnames(tmax_pts_jun) <- 'temp'
colnames(tmax_pts_jul) <- 'temp'
colnames(tmax_pts_aug) <- 'temp'
colnames(tmax_pts_sep) <- 'temp'

## ----------------------------------------------------------------------

## AmeriFlux data used to create regression from site US-MRf: Mary's River Fir
## this equation ended up not being used because it too greatly restricted soil temperature
# setwd(spp_dir)
# AF_MRf <- read.csv("AMF_US-MRf_BASE_HH_4-1.csv")
# AF_MRf[AF_MRf$TA == -9999,] <- NA
# AF_MRf[!is.na(AF_MRf$TS_2) & AF_MRf$TS_2 == -9999,] <- NA
# AF_MRf <- na.omit(AF_MRf)
# AF_MRf <- AF_MRf[,c("TA", "TS_2")]
# colnames(AF_MRf) <- c("air_temp", "soil_temp_8cm")
# 
# # do regression of air and soil temp to create soil conversion equation
# AF_MRf_lm <- lm(AF_MRf$soil_temp_8cm ~ AF_MRf$air_temp)
# print(AF_MRf_lm) # a = 0.4857, b = 4.7615
# 
# plot(AF_MRf$soil_temp_8cm ~ AF_MRf$air_temp)
# abline(AF_MRf_lm, col="red", lwd=2)

# create function based on regression equation
# air_to_soil_temp <- function (airT) {
#   soilT <- (0.4857 * airT) + 4.7615
#   return(soilT)
# }

## AmeriFlux data used to create regression from site US-xSJ: NEON San Joaquin Experimental Range
# AF_xSJ <- read.csv("AMF_US-xSJ_BASE_HH_2-5.csv")
# AF_xSJ[AF_xSJ$TS_1_3_1 == -9999,] <- NA
# AF_xSJ[!is.na(AF_xSJ$TA_1_1_1) & AF_xSJ$TA_1_1_1 == -9999,] <- NA
# AF_xSJ <- na.omit(AF_xSJ)
# AF_xSJ <- AF_xSJ[,c("TA_1_1_1", "TS_1_3_1")]
# colnames(AF_xSJ) <- c("air_temp", "soil_temp_10cm")

# # do regression of air and soil temp to create soil conversion equation
# AF_xSJ_lm <- lm(AF_xSJ$soil_temp_10cm ~ AF_xSJ$air_temp)
# print(AF_xSJ_lm) # a = 0.6433 b = 6.0322
# 
# plot(AF_xSJ$soil_temp_10cm ~ AF_xSJ$air_temp)
# abline(AF_xSJ_lm, col="red", lwd=2)
# 
# # # create function based on regression equation
air_to_soil_temp <- function (airT) {
  soilT <- (0.6433 * airT) + 6.0322
  return(soilT)
}

## ----------------------------------------------------------------------
## "observed productivity"
## create line plot to model poisson curve based on average growing season temps above
temp_pts_merge <- rbind(tavg_pts_apr, tavg_pts_may, tavg_pts_jun, tavg_pts_jul, tavg_pts_aug, tavg_pts_sep, tmax_pts_jun, tmax_pts_jul, tmax_pts_aug, tmax_pts_sep)

# transform air temp to soil temp
soil_temp_pts <- air_to_soil_temp(airT=temp_pts_merge)
max(soil_temp_pts)
hist(soil_temp_pts$temp)

#calculate ppdf1 and 2 (mean and max temp respectively)
ppdf_1 <- ceiling(mean(soil_temp_pts$temp))
ppdf_2 <- ceiling(max(soil_temp_pts$temp))

temp_freq <- as.data.frame(table(soil_temp_pts))
temp_freq$soil_temp_pts <- as.numeric(as.character(temp_freq$soil_temp_pts))
temp_freq$soil_temp_pts <- ceiling(temp_freq$soil_temp_pts)

temp_freq_table <- temp_freq %>% group_by(soil_temp_pts) %>% summarise_each(list(sum))

# substituting species range (frequency of obs at each temp) for productivity
observed_productivity <- temp_freq_table$Freq/max(temp_freq_table$Freq)
observed_soil_temp <- temp_freq_table$soil_temp_pts

plot(observed_soil_temp, observed_productivity, col="blue", type="p")

##-----------------------------------------------------------------------
## fit curve to observed productivity

# starting coefficients
ppdf_3 <- 1
ppdf_4 <- 1

# calculate fraction here
fraction <- (ppdf_2 - observed_soil_temp)/(ppdf_2 - ppdf_1)

# this is the curve based on the coefficients listed above
Landis_Relative_production <-  ifelse(fraction > 0, (exp(ppdf_3/ppdf_4 * (1 - (fraction ^ ppdf_4))) * (fraction ^ ppdf_3)), 0)
plot(observed_soil_temp, Landis_Relative_production, type="l", lwd=3, ylab="Max relative GPP", xlab="temperature")

# Algorithm for calculating fitted relative production based on temp and the 4 coefficients
calculate_fitted_RP_ppdf34 <- function(temp, coef_3, coef_4)
{
  fraction <- (ppdf_2 - temp) / (ppdf_2 - ppdf_1)
  pred <- ifelse(fraction > 0, (exp(coef_3/coef_4 * (1-(fraction ^ coef_4))) * (fraction ^ coef_3)), 0)
}

# non-linear curve fitting with the equation above
temp_dataframe <- data.frame(cbind(observed_soil_temp, observed_productivity))
plot(temp_dataframe)
names(temp_dataframe) <- c("soil_temp_8cm", "productivity")

constraints <- data.frame(coef_3=c(0,5), coef_4=c(0,5))
lower_constraints <- data.frame(coef_3=0, coef_4=0)
upper_constraints <- data.frame(coef_4=5, coef_4=5)

Fitted_RelativeProduction_coef34 <- nls2(productivity ~ calculate_fitted_RP_ppdf34(soil_temp_8cm, coef_3, coef_4),
                                  data = temp_dataframe, start = constraints, trace = T,
                                  nls.control(maxiter = 10000, printEval = F, warnOnly = T), 
                                  lower=lower_constraints, upper=upper_constraints, algorithm = "port")


summary(Fitted_RelativeProduction_coef34)
coef(Fitted_RelativeProduction_coef34)
ppdf_1
ppdf_2

plot(observed_soil_temp, observed_productivity, col="blue", type="p")
lines(observed_soil_temp, predict(Fitted_RelativeProduction_coef34))


##-----------------------------------------------------------------------------
## plot it pretty
obs_v_predicted <- ggplot(temp_dataframe, aes(x=soil_temp_8cm, y=productivity)) +
  geom_point(shape = 15, size=2, aes(color='observed')) +
  geom_line(aes(x=observed_soil_temp, y=predict(Fitted_RelativeProduction_coef34), color="predicted")) +
  labs(x='soil temperature, 10cm', y="expected productivity",
       title= "Temperature curve for Arbutus menziesii (ARME)\nusing April-September Tavg and July-Sep Tmax\n\nppdf 1 = 18 (mean soil temp), ppdf 2 = 31 (max soil temp),\nppdf 3 = 5, ppdf 4 = 2.6") +
  scale_color_manual(name='', values = c('observed' = 'blue', 'predicted' = 'black'))


obs_v_predicted

# setwd(spp_dir)
ggsave("tempCurve_ARME_Final.jpeg")
