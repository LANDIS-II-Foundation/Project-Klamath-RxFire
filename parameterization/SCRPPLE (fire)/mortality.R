# This the code provides a simple example of how to assimilate 
# field and remote sensing data to create inputs for the Scrpple mortality model. 

# ZJ Robbins 2021 ##############

install.packages(c("ncmeta", "tidync", "maps", "stars", "ggplot2", "devtools", 
                   "stars", "RNetCDF", "raster", "dplyr", "tidyr"))
library(pbapply)
library(ncmeta)
library(tidync)
library(tidyverse)
library(RNetCDF)
library(ncdf4)
options(scipen=999) # This removes scientific notation in printing
rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

w.dir <- "/Volumes/LaCie/SCRPPLE/"
scrpple.dir <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/"

# Raster data
# Need raster data for the area of these sites, including the previous years climatic 
# water deficit and actual evapotranspiration, the effective wind speed on the day of, 
# estimations of fuels, and the post fire RDNBR. These are constructed into a 
# dataframe and joined with the plot level data by site name. 
# Note: Here every site represents a pixel in the field
# input rasters created with mortality_datacleaning.R script

rdnbr <- raster(paste0(scrpple.dir, "SCRPPLE/mortality_inputs/rdnbr_allyears.tif"))
cwd <- raster(paste0(scrpple.dir, "SCRPPLE/mortality_inputs/cwd_allyears.tif"))
aet <- raster(paste0(scrpple.dir, "SCRPPLE/mortality_inputs/aet_allyears.tif"))
wind <- raster(paste0(scrpple.dir, "SCRPPLE/mortality_inputs/effective_wind_speed.tif"))
fuels <- raster(paste0(scrpple.dir, "SCRPPLE/mortality_inputs/fine_fuels.tif"))
clay <- raster(paste0(scrpple.dir, "FinalProducts/soils/percent_clay.tif"))

Rasters <- stack(rdnbr, cwd, aet, wind, fuels, clay)
Rasters[Rasters==0] <- NA
plot(Rasters)

R_df <- as.data.frame(Rasters)
colnames(R_df) <- c("rdnbr", "cwd", "aet", "wind", "fuels", "clay")
R_df[R_df==0] <- NA
R_df$rdnbr <- ifelse(R_df$rdnbr > 4, NA, R_df$rdnbr)
R_df <- R_df[complete.cases(R_df),]

##-------------
### Fitting site level mortlaity
# Here we fit the site level mortality predictors, for this study we are using 
# RDNBR as the measure of site level mortality. We test that against the effective 
# windspeed, fuels, and climatic water deficit

glm1 <- with(R_df, glm(rdnbr ~ wind + fuels + cwd + clay, family = Gamma(link="inverse")))
summary(glm1)

# We can test other models to see which performs best.

glm2 <- with(R_df, glm(rdnbr ~ wind + fuels, family=gaussian(link="inverse")))
summary(glm2)

glm3 <- with(R_df, glm(rdnbr ~ cwd + aet + wind + fuels + clay, family=gaussian(link="inverse")))
summary(glm3)
# Lets take a look of that relationship.

### Plotting the model 
ysim_No <- predict(glm3, data.frame(Eff_Windspeed = R_df$wind[order(R_df$wind)], 
                                    fuels = (rep(0,length(R_df$wind)))), type="response",se=T)
ysim_Median <- predict(glm3, data.frame(Eff_Windspeed = R_df$wind[order(R_df$wind)],
                                        fuels=(rep(median(R_df$fuels),length(R_df$wind)))), type="response",se=T)
ysim_Max <- predict(glm3, data.frame(Eff_Windspeed = R_df$wind[order(R_df$wind)],
                                     Fuels=(rep(quantile(R_df$fuels,.95),length(R_df$wind)))), type="response",se=T)


plot(R_df$wind, R_df$rdnbr, pch = 16, xlab = "Effective Windspeed", ylab = "RDNBR")
lines(R_df$wind[order(R_df$wind)], ysim_No$fit + ysim_No$se.fit, col="blue", lwd=1.0, lty=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_Median$fit + ysim_Median$se.fit, col="black", lwd=1.0, lty=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_Median$fit, col="orange", lwd=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_Median$fit - ysim_Median$se.fit, col="black", lwd=1.0, lty=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_Max$fit + ysim_Max$se.fit, col="red", lwd=1.0, lty=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_Max$fit, col="red", lwd=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_Max$fit - ysim_Max$se.fit, col="red", lwd=1.0, lty=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_No$fit, col="blue", lwd=3.0)
lines(R_df$wind[order(R_df$wind)], ysim_No$fit - ysim_No$se.fit, col="blue", lwd=1.0, lty=3.0)
legend(40,600.0, legend=c("High Fuels","Median Fuels","Minimum Fuels"), lty=c(1,1,1), col=c("red","black","blue")) 

### This analysis provides us with the following parameters.
# SiteMortalityB0  1.0584554174 << Intercept
# SiteMortalityB1 -0.0275613390 << The parameter fit for site level clay % in Soil.
# SiteMortalityB2 -0.0040636390 << The parameter fit for site level previous years annual ET.
# SiteMortalityB3 -0.0008418894 << The parameter fit for site level Effective Windspeed.
# SiteMortalityB4 -0.0011393422 << The parameter fit for site level Climatic Water Deficit (PET-AET).
# SiteMortalityB5  0.0000058876 << The parameter fit for fine fuels.
# SiteMortalityB6  0.0          << The parameter fit for ladder fuels.

