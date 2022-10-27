# This the code provides a simple example of how to use geomac shape files
# to estimate the fire spread parameters needed for scrpple. 
# 
# ZJ Robbins 2021 \#\#\#\#\#\#\#\#\#\#\#\#\#\#

library(sp)
library(tidyverse)
library(raster)
library(rgdal)
library(nplr)
library(nlme)
library(sf)
library(geosphere)

# rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

# Directories
w.dir <- "C://Users/adeak/Desktop/LANDIS/SCRPPLE/"
fire.dir <- "C://Users/adeak/Desktop/LANDIS/SCRPPLE/data/"
setwd(w.dir)

# landscape
mask <- st_read(paste0(w.dir, "landscape_shape/landscape_shape.shp"))
rast_mask <- raster(paste0(w.dir,"ecoregions.tif",sep=""))

### --------------------------------------------------
# THIS IS WHERE I TAKE CATEGORICAL VARIABLES FROM INPUT RASTERS AND SET UP LOOKUPS 
# FOR REAL VALUES OF WIND SPEED, FUEL BIOMASS ETC 

# Process FWI values produced by LANDIS climate library to prep for attaching to larger spread matrix
# This is the Fire weather index calculation 
fwi.dat <- read.csv(paste0(fire.dir,"Climate-future-input-log.csv", sep=""),stringsAsFactors = FALSE)

# Need to attach ecoregion info
fwi.dat.slim <- fwi.dat[,c(1,2,3,23)]
fwi.date.info <- with(fwi.dat.slim, paste(Year, Timestep))
fwi.dates <- strptime(fwi.date.info, "%Y %j") #Converting from julian day to y-m-d
fwi.date.dat <- cbind(as.POSIXct(fwi.dates), fwi.dat.slim[,3:4]) #attaching 
colnames(fwi.date.dat) <- c("Date", "Ecoregion", "FWI")

# --------------------------------
# Attach LANDFIRE fuel loading info based on fuel type number
# I selected columns I thought would define fine fuels, but these can change if we need. Units are tons/acre
fccs.rast <- raster(paste0(fire.dir, "fccs_clip/fccs_clip.tif")) # Had to clip FCCS data in ArcGIS
rast_mask[rast_mask==0] <- NA 
fccs.rast <- crop(fccs.rast, rast_mask)
fccs.rast <- mask(fccs.rast, rast_mask) 

fccs.data <- read.csv(paste0(fire.dir, "landfire_FCCS.csv"))
fccs <- as.data.frame(fccs.rast) %>% left_join(fccs.data, by=c("fccs_clip" = "Value"))
fccs.loading <- fccs[,c("fccs_clip", "LLM_tpa", "Ground_tpa")] # only kept fine fuels 
fccs.loading.total <- rowSums(fccs.loading[,2:length(fccs.loading)])
fccs.loading.total <- fccs.loading.total * 224.17 #convert from tons/acre to g/m-2
fccs.finefuels <- data.frame(fccs.loading[,1], fccs.loading.total)
colnames(fccs.finefuels) <- c("fueltype", "finefuels_loading")
summary(fccs.finefuels)

fuel_map <- rast_mask
values(fuel_map) <- fccs.finefuels$finefuels_loading

# --------------------------------------------------
# Here we are looking to fit a binomial model relating the probability to FWI, wind, and fuels.
# The ecoregion map is used to locate the windspeed and the FWI data for each time step.

### This is the climate regions we are using
wind_map <- raster(paste0(w.dir,"ecoregions.tif",sep=""))
plot(wind_map)

# We also have an uphill azimuth map and a slope map (used to calculate the direction of windspeed)
uphill_azi_map<- raster(paste(w.dir,"uphill_slope_azimuth.tif",sep=""))

# This a slope map. 
slope_map <-  raster(paste(w.dir,"slope.tif",sep=""))

# This all becomes one raster stack to look at in the analysis.
climate_stack <- stack(wind_map, fuel_map, uphill_azi_map, slope_map)
names(climate_stack) <- c("wind_FWI", "fuels", "uphill_slope_azi", "slope")
plot(climate_stack)

# --------------------------------------------------
# Here is the processing of the geomac/NIFC data
# These are shape files for each days fire boundary, gathered together by
# fire. They are used to measure cells for which spread occurs and which it does not.
# Consecutive fires were found using the GEOMACC_cleaning script

perimeter_map <- st_read(paste0(fire.dir,"consecutive_fires.shp"))
fire_names <- as.character(perimeter_map$new_nam)

## These fires were filtered by having multiple days of spread, and shapes that made sense as a progression. 
SoundInc <- unique(perimeter_map$new_nam)

# subset years with climate data and with known fuel values from Landfire
years <- 2014:2016
perimeter_map <- subset(perimeter_map, perimeter_map$year %in% years)

wsv_dat_df <- read.csv(paste0(fire.dir, "wind_speed.csv"))

# Select for fires that lasted more than 1 day
fire_names_manydays <- fire_names[!duplicated(fire_names)]
fire_names_manydays <- fire_names_manydays[!is.na(fire_names_manydays)] ##Removing potential NAs
fire_names_manydays <- fire_names_manydays[fire_names_manydays %in% SoundInc]
fire_names_manydays <- fire_names_manydays[fire_names_manydays %in% perimeter_map$new_nam]

perimeter_map$TrueDate <- as.Date(perimeter_map$dt_rcrd)

##-----------------------------------
# For each fire for each day. 
# i=2
# j=2
climate_day_mat <- NULL
for (i in 1:length(fire_names_manydays)){
  # print(fire_names_manydays[i])
  fire_select <- subset(perimeter_map, perimeter_map$new_nam ==fire_names_manydays[i]) # selecting an individual fire
  fire_days <- as.character(sort(unique(fire_select$TrueDate)))
  ### Check that there are two days worth of data to look at. 
  if(length(fire_days) < 2) next
  storedcells<-NULL
  dftest <- as.data.frame(fire_select)
  ### This subloop looks through each set of polygons (t, t+1) and finds the last day in which expansion occurs
  ### this will indicate when to tell th algorithm to consider the remaining cells failed spread. 
  for(j in 1:(length(fire_days)-1)){
    ## Day t shape
    fire_day_select <- subset(fire_select, fire_select$TrueDate == fire_days[j])# selecting the first day of the fire
    fire_day_select <- fire_day_select[order(fire_day_select$acres),]
    fire_day_select <- fire_day_select[1,] #selecting the first fire perim from that date, in case there are multiples
    ## Day t+1 shape
    fire_day_select_1 <- subset(fire_select, fire_select$TrueDate== fire_days[j+1])# getting day 2 fire perimeter
    fire_day_select_1<-fire_day_select_1[order(fire_day_select_1$acres),]
    fire_day_select_1 <- fire_day_select_1[1,] #selecting the first fire perim from that date, in case there are multiples
    area_expansion <- as.numeric((st_area(fire_day_select_1)/4046.86)-(st_area(fire_day_select)/4046.86))
    if(area_expansion < 30.00){
      next()
    }
    lastday <- j
  }
  for(j in 1:(length(fire_days)-1)){
    Failedvalues<-NA
    Successcells<-NA
    EndFailedvalues<-NA
    #print(paste0("day  ",j))
    ## day t shape
    fire_day_select <- subset(fire_select, fire_select$TrueDate == fire_days[j])# selecting the first day of the fire
    fire_day_select <- fire_day_select[order(fire_day_select$acres),]
    fire_day_select <- fire_day_select[1,] # selecting the first fire perim from that date, in case there are multiples
    fire_day_centroid <- st_centroid(fire_day_select)
    ## day t+1 shape
    fire_day_select_1 <- subset(fire_select, fire_select$TrueDate== fire_days[j+1]) # getting day 2 fire perimeter
    fire_day_select_1 <- fire_day_select_1[order(fire_day_select_1$acres),]
    fire_day_select_1 <- fire_day_select_1[1,] #selecting the first fire perim from that date, in case there are multiples
    
    # Getting wind direction. This is inferred from direction of spread, where azimuth of spread = wind direction
    fire_day_1_centroid <- st_centroid(fire_day_select_1) # Finding centroid for estimation wind direction
    fire_day_centroid <- st_transform(fire_day_centroid, CRS("+proj=longlat +datum=WGS84")) # Need to convert back to lat/long to get azimuth direction
    fire_day_1_centroid <- st_transform(fire_day_1_centroid, CRS("+proj=longlat +datum=WGS84")) # Need to convert back to lat/long to get azimuth direction
    fire_day_centroid_mat <- t(as.matrix(unlist(fire_day_centroid[,12])))
    fire_day_1_centroid_mat <- t(as.matrix(unlist(fire_day_1_centroid[,12])))
    fire_azimuth <- bearingRhumb(fire_day_centroid_mat, fire_day_1_centroid_mat) # find the azimuth of fire spread by Rhumb bearing. This is used as a proxy assumed wind direction
    
    ## calculate area of expansion, Area m2 to acres.
    area_expansion <- as.numeric((st_area(fire_day_select_1)/4046.86)-(st_area(fire_day_select)/4046.86))
    ### In this case the spread would be less than one 30m cell in our simulation.
    if(area_expansion < 30.00){
      next()
    }
    ### Here we associate the wind and fire weather index, and reclassify the 
    ### raster to associate with spread/not spread cells
    wind_today <- wsv_dat_df %>% subset(TIMESTEP==gsub("/","-",paste0(fire_days[j],'T00:00:00Z')))
    
    WindMap <- climate_stack$wind_FWI
    WindTrans <- as.matrix(data.frame(was=c(1,2,3), is=as.numeric(t(wind_today[2:4]))))
    Wind_Reclas <- reclassify(WindMap,WindTrans)
    fire_today <- fwi.date.dat %>% subset(Date == gsub("/","-",paste0(fire_days[j],'T00:00:00Z'))) %>% arrange(Ecoregion)
    FireMap <- climate_stack$wind_FWI
    FireTrans <- as.matrix(data.frame(was=c(1,2,3),is=fire_today$FWI))
    FireWeather <- reclassify(FireMap,FireTrans)  
    End_stack <- stack(climate_stack,FireWeather)
    End_stack <- stack(End_stack,Wind_Reclas)
    #Creating vector of fire dates. It doesn't work when I bind them below for some reason 
    date_char <- as.POSIXct(as.character(fire_day_select$TrueDate)) 
    # Extracting climate and fuels variables from raster stack.
    # This will compile a data frame of every cell that fire perimeters touch
    day1 <- raster::extract(End_stack, fire_day_select, cellnumbers = TRUE, df=TRUE, method="simple")
    day2 <- raster::extract(End_stack, fire_day_select_1, cellnumbers = TRUE, df=TRUE, method="simple")
    # First find day not in stored, then find adjacents, remove adjacents in stored. 
    day1cells <- day1$cell[!day1$cell %in% storedcells]
    storedcells <- c(storedcells,day1$cell)
    if( length(day1cells) >0){ 
      adjacentcells1 <- adjacent(End_stack,day1cells, directions=4, pairs=TRUE,id=TRUE)
      adjacentcells1 <- unique(adjacentcells1[,3])
      adjacentcells1 <- adjacentcells1[!adjacentcells1 %in% storedcells]
    }else(adjacentcells1=NA)
    newspread <- day2$cell[!day2$cell %in% storedcells]
    ### Calculate failed spread, areas where spread could occur, but are have not previously been tried. 
    Failedvalues <- data.frame()
    if(length(day1cells) >0){ 
      FailedCells <-  adjacentcells1[!adjacentcells1 %in% newspread]
      Failedvalues <- as.data.frame(raster::extract(End_stack,adjacentcells1))
    }
    if (nrow(Failedvalues)>0){ Failedvalues$spread_success<-0}
    Successcells <- as.data.frame(raster::extract(End_stack,newspread))
    if (nrow(Successcells)==0){next}
    # Successful spread gets one 
    Successcells$spread_success <- 1
    # Here if it is the last day of spread, calculate one more days possible spread,
    # all of which is counted as failed. 
    EndFailedvalues<-data.frame()
    if(j==(lastday)){
      ### Creating the same wind and fire weather index as above. 
      wind_today <- wsv_dat_df %>% subset(TIMESTEP==gsub("/","-",paste0(fire_days[j],'T00:00:00Z')))
      WindMap <- climate_stack$wind_FWI
      WindTrans <-  as.matrix(data.frame(was=c(1,2,3),is=as.numeric(t(wind_today[2:4]))))
      Wind_Reclas <- reclassify(WindMap,WindTrans)
      fire_today <- fwi.date.dat %>% subset(Date==gsub("/","-",paste0(fire_days[j],'T00:00:00Z'))) %>% arrange(Ecoregion)
      FireMap <- climate_stack$wind_FWI
      FireTrans <- as.matrix(data.frame(was=c(1,2,3),is=fire_today$FWI))
      FireWeather <- reclassify(FireMap,FireTrans)  
      End_stack <- stack(climate_stack,FireWeather)
      End_stack <- stack(End_stack,Wind_Reclas)
      EndCells <- adjacent(End_stack,day2$cell, directions=4, pairs=TRUE, id=TRUE)
      EndCells <- unique(EndCells[,3])
      EndCells <- EndCells[!EndCells %in% storedcells]
      EndFailedvalues <- as.data.frame(raster::extract(End_stack, EndCells))
      EndFailedvalues$spread_success <- 0
    }
    ### Adding cells to one dataframe.
    if(nrow(Failedvalues)>0 & nrow(Successcells)>0){
      Dfout <- rbind(Failedvalues,Successcells)}else{Dfout<-Successcells}
    if(nrow(EndFailedvalues)>0) {Dfout<-rbind(Dfout,EndFailedvalues) }
    #print(table(Dfout$spread_success))
    Dfout$Area_expansion <- area_expansion
    climate_day_df <- cbind(fire_names_manydays[j],
                            as.character(date_char),Dfout, fire_azimuth) #Putting everything into a big dataframe with all climate variables
    climate_day_mat <- rbind.data.frame(climate_day_mat, climate_day_df) #binding individual days to every other day
  }
  
}

# Here is a table of the number of successful spread events versus non-successful spread events
table(climate_day_mat$spread_success)

# 0      1 
# 92377  115070

### Cleaning up the dataframe. 
# climate_day_mat <- read.csv(paste0(w.dir, "/SCRPPLE/spread_params.csv")))
climate_day_total <- climate_day_mat

### Looking at files that have all the neccesary data 
climate_day_total <- climate_day_total[climate_day_total$wind_FWI.1!=0,]
climate_day_complete <- climate_day_total[complete.cases(climate_day_total[1:9]),]

##Attaching a unique ID to each row in case we need it later
climate_day_complete <- cbind(1:nrow(climate_day_complete), climate_day_complete)

# Renaming columns
# climate_day_complete<-climate_day_complete[,-1]
colnames(climate_day_complete) <- c("ID","FireName","date","wind_region","fuel_number","uphill_azi","slope",
                                    "FWI","WSPD","spread","expansion", "fire_azimuth")
climate_day_complete$expansion_hectares <- climate_day_complete$expansion*0.404686 ## To hectares
climate_day_complete$fuel_number <- climate_day_complete$fuel_number/3000
climate_day_complete$fuel_number[climate_day_complete$fuel_number > 1.0]<-1.0

# Effective wind speed
U_b <- 5 # This changes based on fire severity. Combustion bouancy.
### Caculating windspeed in direction of spread 
relative_wd <- as.numeric(climate_day_complete$fire_azimuth) - as.numeric(climate_day_complete$uphill_azi)
### Calculating effective wind speed. 
climate_day_complete$effective_wsv <- U_b * ((climate_day_complete$WSPD/U_b) ^ 2 + 2*(climate_day_complete$WSPD/U_b) *  
                                               sin(climate_day_complete$slope) * cos(relative_wd) + (sin(climate_day_complete$slope)^2)^0.5)
head(climate_day_complete)

## Save for future analysis
write.csv(climate_day_complete, paste0(w.dir, "outputs/spread_params.csv"))

### Looking at variable response. 
hexbinplot(climate_day_complete$spread~climate_day_complete$FWI,xlab="FWI",ylab="(P) Spread",xbins=50,aspect=1,type="r")
hexbinplot(climate_day_complete$spread~climate_day_complete$effective_wsv,xlab="Wind Speed",ylab="(P) Spread",xbins=50,aspect=1,type="r")
hexbinplot(climate_day_complete$spread~climate_day_complete$fuel_number,xlab="Fuel number",ylab="(P) Spread",xbins=50,aspect=1,type="r")
hexbinplot(climate_day_complete$expansion~climate_day_complete$FWI,xlab="FWI",ylab="Spread (HA)",xbins=50,aspect=1,type="r")
hexbinplot(climate_day_complete$expansion~climate_day_complete$effective_wsv,xlab="Wind Speed",ylab="Spread(HA)",xbins=50,aspect=1,type="r")

### Here is an example model fit. Using this data and the GLM package.
spread_vars_short <- climate_day_complete
table(spread_vars_short$spread)

# 0     1 
# 17595 20476

Full_logit <- glm(spread ~ fix(FWI)+fix(effective_wsv)+fix(fuel_number), 
                  data = spread_vars_short, family = "binomial")
summary(Full_logit)
#                          Estimate   Std. Error  z value            Pr(>|z|)    
#   (Intercept)           -0.646877     0.068849  -9.396 < 0.0000000000000002 ***
#   fix(FWI)               0.034532     0.001594  21.668 < 0.0000000000000002 ***
#   fix(effective_wsv)     0.018966     0.002565   7.395    0.000000000000141 ***
#   fix(fuel_number)   -1602.860365   233.562904  -6.863    0.000000000006759 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
AIC(Full_logit)
# 51717.17

# fine fuels seem to have a supressive effect, lets look at a model without it.
Par_logit <- glm(spread ~fix(FWI)+fix(effective_wsv), 
                 data = spread_vars_short, family = "binomial")
summary(Par_logit)

#                     Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)        -0.999358   0.045939 -21.754 < 0.0000000000000002 ***
# fix(FWI)            0.033238   0.001579  21.048 < 0.0000000000000002 ***
# fix(effective_wsv)  0.019802   0.002559   7.737   0.0000000000000102 ***


AIC(Par_logit)
## 51763 Not as good as above model!

### Here is what that second model looks like.
FWI<-seq(0,max(spread_vars_short$FWI),.5)
xB<-exp(( -1.259443 )+ 0.036959*(FWI)+2.271e-04*max(spread_vars_short$fuel_number))
binomial2<-xB/(1+xB)

xB<-exp(( -1.259443 )+ 0.036959*(FWI)+2.271e-04*min(spread_vars_short$fuel_number))
binomial2min<-xB/(1+xB)

xB<-exp(( -1.259443 )+ 0.036959*(FWI)+2.271e-04*mean(spread_vars_short$fuel_number))
binomial2mean<-xB/(1+xB)

plot(FWI,binomial2,xlab="Fire Weather Index",ylab="Spread Probability",ylim=c(0,1),col="red",type="l",lwd=3.0,cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="Probablity of Spread")
lines(FWI,binomial2mean,lwd=3.0)
lines(FWI,binomial2min,col="blue",lwd=3.0)
legend(0,1.0,legend=c("High Fine Fuels","Median Fine Fuels"," Low Fine Fuels"),
       lty=c(1,1,1),lwd=c(3,3,3),col=c("red","black","blue"))

#--------------------------------------
### Max spread. Code written by Shelby
# Take the max spread per FWI and WS fit to that.
spread.vars.complete <- climate_day_complete
head(spread.vars.complete)
# aggregate by day keeping observation with max expansion

spread.vars.maxexp <- aggregate(spread.vars.complete$expansion, by=list(spread.vars.complete$date), FUN=max)
names(spread.vars.maxexp) <- c("date", "expansion")
maxdaily <- left_join(spread.vars.maxexp, spread.vars.complete, by=c("date", "expansion"))
head(maxdaily)

#aggregate one more time
maxdaily$FWI <- as.numeric(paste(maxdaily$FWI))
maxdailyunique <- aggregate(cbind(effective_wsv, FWI)~ date + expansion + FireName, data=maxdaily, FUN=max)
nrow(maxdailyunique)
head(maxdailyunique)

plot(maxdailyunique$expansion~maxdailyunique$effective_wsv)
plot(maxdailyunique$expansion~maxdailyunique$FWI)

maxlm <- lm(maxdailyunique$expansion~maxdailyunique$effective_wsv + maxdailyunique$FWI)
summary(maxlm)

#                               Estimate  Std. Error t value Pr(>|t|)  
# (Intercept)                    854.46     444.34   1.923   0.0675 .
# maxdailyunique$effective_wsv   -71.86      47.08  -1.526   0.1412  
# maxdailyunique$FWI              24.06      29.38   0.819   0.4216

# plot with FWI on the x-axis
FWI <- seq(0,max(maxdailyunique$FWI),.5)
maxsprd <- maxlm$coefficients[1] + 
  maxlm$coefficients[2]*mean(maxdailyunique$effective_wsv, na.rm=T) +
  maxlm$coefficients[3]*(FWI)
plot(FWI, maxsprd)
# plot with wind on the x-axis
Wind <- seq(0,max(maxdailyunique$effective_wsv),.5)
maxsprd <- maxlm$coefficients[1]+ 
  maxlm$coefficients[2]*Wind +
  maxlm$coefficients[3]*mean(maxdailyunique$FWI, na.rm=T)
plot(Wind, maxsprd)


# ## Do outlier analysis and see if any should be removed
cooks.d <- cooks.distance(maxlm)
plot(cooks.d, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/(nrow(maxdailyunique)), col="red")  # add cutoff line
text(x=1:length(cooks.d)+1, y=cooks.d, labels=ifelse(cooks.d>4/(nrow(maxdailyunique)), names(cooks.d),""), col="red")  # add labels
#removing outliers
#influential row numbers
influential <- as.numeric(names(cooks.d)[(cooks.d > (4/(nrow(maxdailyunique))))])
maxdaily_noouts <- maxdailyunique[-influential,]

#fit GLM
maxglm <- glm(expansion ~ FWI + effective_wsv, data=maxdaily_noouts)
summary(maxglm)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    559.343    237.643   2.354   0.0289 *
#   FWI           -4.951     15.965  -0.310   0.7597  
# effective_wsv   -1.690     26.476  -0.064   0.9497  

  