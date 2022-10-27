#-------------------------------------------------------
# CLUSTER ANALYSIS -- UNSUPERVISED CLASSIFICATION  
## RESOURCE: https://www.r-exercises.com/2018/02/28/advanced-techniques-with-raster-data-part-1-unsupervised-classification/
# Shared by Tom Brussel
#-------------------------------------------------------

# Open needed packages
library(ncdf4)
library(raster)
library(rgdal)
library(dplyr)
library(tibble)
library(tidyverse)
library(qmap)
library(lubridate)
library(data.table)
library(pbapply)
library(sf)
library(cluster)
library(clusterCrit)

rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

### Create a raster stack from needed climate raster files
### Imported raster files

setwd("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/climate_regions/ClimateInputs/tmax")
May_tmax <- raster("may_tmax.tif")
Jun_tmax <- raster("june_tmax.tif")
Jul_tmax <- raster("july_tmax.tif")
Aug_tmax <- raster("aug_tmax.tif")
Sep_tmax <- raster("sept_tmax.tif")
Oct_tmax <- raster("oct_tmax.tif")

setwd("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/climate_regions/ClimateInputs/tmin")
May_tmin <- raster("may_tmin.tif")
Jun_tmin <- raster("june_tmin.tif")
Jul_tmin <- raster("july_tmin.tif")
Aug_tmin <- raster("aug_tmin.tif")
Sep_tmin <- raster("sept_tmin.tif")
Oct_tmin <- raster("oct_tmin.tif")

setwd("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/climate_regions/ClimateInputs/ppt")
May_ppt <- raster("may_ppt.tif")
Jun_ppt <- raster("June_ppt.tif")
Jul_ppt <- raster("july_ppt.tif")
Aug_ppt <- raster("aug_ppt.tif")
Sep_ppt <- raster("sept_ppt.tif")
Oct_ppt <- raster("oct_ppt.tif")

climate_brick <- stack(May_tmin, Jun_tmin, Jul_tmin, Aug_tmin, Sep_tmin, Oct_tmin,
                 May_tmax, Jun_tmax, Jul_tmax, Aug_tmax, Sep_tmax, Oct_tmax,
                 May_ppt,  Jun_ppt,  Jul_ppt,  Aug_ppt,  Sep_ppt,  Oct_ppt)

## remove objects I don't need anymore                
rm(May_tmin, Jun_tmin, Jul_tmin, Aug_tmin, Sep_tmin, Oct_tmin,
   May_tmax, Jun_tmax, Jul_tmax, Aug_tmax, Sep_tmax, Oct_tmax,
   May_ppt,  Jun_ppt,  Jul_ppt,  Aug_ppt,  Sep_ppt,  Oct_ppt)

## read in landscape shapefile
setwd("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous")
landscape <- raster("FinalProducts/landscape_mask.tif")

# reproject both landscape shp and climate raster; match resolution of landscape to climate
desired_crs <- crs("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
reproj_landscape <- projectRaster(landscape, crs=crs(desired_crs))
reproj_climate   <- projectRaster(climate_brick, crs=crs(desired_crs))
extent(reproj_climate) <- extent(resample_landscape)
resample_landscape <- resample(reproj_landscape, reproj_climate, method='ngb')
reproj_climate; resample_landscape # check that res and extent match
plot(resample_landscape); plot(reproj_climate$may_tmin)

# clip climate raster to landscape and trim empty rows and columns
climate_mask <- mask(reproj_climate, resample_landscape)
climate <- trim(climate_mask, values=NA)
plot(climate$may_tmin) # check

rm(climate_brick, landscape, resample_landscape, reproj_climate, 
   reproj_landscape, climate_mask)

#-------------------------------------------------------
# Cluster analysis 

setwd("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/climate_regions")

### Extract all values from the raster into a data frame
rstDF <- values(climate)

# Check NA's in the data
idx <- complete.cases(rstDF)

# Initiate the raster datasets that will hold all clustering solutions 
# from 2 groups/clusters up to 30
rstKM <- raster(climate[[1]])
rstCLARA <- raster(climate[[1]])

for(nClust in 2:10){
  
  cat("-> Clustering data for nClust =",nClust,"......")
  
  # Perform K-means clustering
  km <- kmeans(rstDF[idx,], centers = nClust, iter.max = 50)
  
  # Perform CLARA's clustering (using manhattan distance)
  cla <- clara(rstDF[idx, ], k = nClust, metric = "manhattan")
  
  # Create a temporary integer vector for holding cluster numbers
  kmClust <- vector(mode = "integer", length = ncell(climate))
  claClust <- vector(mode = "integer", length = ncell(climate))
  
  # Generate the temporary clustering vector for K-means (keeps track of NA's)
  kmClust[!idx] <- NA
  kmClust[idx] <- km$cluster
  
  # Generate the temporary clustering vector for CLARA (keeps track of NA's too ;-)
  claClust[!idx] <- NA
  claClust[idx] <- cla$clustering
  
  # Create a temporary raster for holding the new clustering solution
  # K-means
  tmpRstKM <- raster(climate[[1]])
  # CLARA
  tmpRstCLARA <- raster(climate[[1]])
  
  # Set raster values with the cluster vector
  # K-means
  values(tmpRstKM) <- kmClust
  # CLARA
  values(tmpRstCLARA) <- claClust
  
  # Stack the temporary rasters onto the final ones
  if(nClust==2){
    rstKM    <- tmpRstKM
    rstCLARA <- tmpRstCLARA
  }else{
    rstKM    <- stack(rstKM, tmpRstKM)
    rstCLARA <- stack(rstCLARA, tmpRstCLARA)
  }
  
  cat(" done!\n\n")
}

# Write the clustering solutions for each algorithm
writeRaster(rstKM,"KMeans_temp_precip.tif", overwrite=TRUE)
writeRaster(rstCLARA,"CLARA_temp_precip.tif", overwrite=TRUE)

### ------------------------------------------------------------------------------
## Now, to evaluate how the clustering went: 

#### Start a data frame that will store all silhouette values for k-means and CLARA   
clustPerfSI <- data.frame(nClust = 2:10, SI_KM = 10, SI_CLARA = NA)


for(i in 1:nlayers(rstKM)){ # Iterate through each layer
  
  cat("-> Evaluating clustering performance for nClust =",(2:10)[i],"......")
  
  # Extract random cell samples stratified by cluster
  # I decreased the size values to roughly half the number of cells on landscape (n=495)
  # value was originally 2000 but loop wouldn't run because size > number of cells on landscape
  cellIdx_RstKM <- sampleStratified(rstKM[[i]], size = 250) 
  cellIdx_rstCLARA <- sampleStratified(rstCLARA[[i]], size = 250)
  
  # Get cell values from the Stratified Random Sample from the raster 
  # data frame object (rstDF)
  rstDFStRS_KM <- rstDF[cellIdx_RstKM[,1], ]
  rstDFStRS_CLARA <- rstDF[cellIdx_rstCLARA[,1], ]
  
  # Make sure all columns are numeric (intCriteria function is picky on this)
  rstDFStRS_KM[] <- sapply(rstDFStRS_KM, as.numeric)
  rstDFStRS_CLARA[] <- sapply(rstDFStRS_CLARA, as.numeric)
  
  # Compute the sample-based Silhouette index for: 
  #    
  # K-means
  clCritKM <- intCriteria(traj = rstDFStRS_KM, 
                          part = as.integer(cellIdx_RstKM[,2]), 
                          crit = "Silhouette")
  # and CLARA
  clCritCLARA <- intCriteria(traj = rstDFStRS_CLARA, 
                             part = as.integer(cellIdx_rstCLARA[,2]), 
                             crit = "Silhouette")
  
  # Write the silhouette index value to clustPerfSI data frame holding 
  # all results
  clustPerfSI[i, "SI_KM"]    <- clCritKM[[1]][1]
  clustPerfSI[i, "SI_CLARA"] <- clCritCLARA[[1]][1]
  
  cat(" done!\n\n")
  
}

write.csv(clustPerfSI, file = "clustPerfSI_temp_precip.csv", row.names = FALSE)

#print table with silhouette values

knitr::kable(clustPerfSI, digits = 3, align = "c", 
             col.names = c("#clusters","Avg. Silhouette (k-means)","Avg. Silhouette (CLARA)"))

# The number of clusters with the largest silhouette index has the optimal number of clusters
# The clustering algorithm output with the largest silhouette index is also optimal

# We can make a plot for comparing the two algorithms:
jpeg(file="climate_silhouette_index.jpeg")
plot(clustPerfSI[,1], clustPerfSI[,2], 
     xlim = c(2,10), ylim = range(clustPerfSI[,2:3]), type = "l", 
     ylab="Avg. Silhouette Index", xlab="# of clusters",
     main="Silhouette index by # of clusters")
# Plot Avg Silhouette values across # of clusters for K-means
lines(clustPerfSI[,1], clustPerfSI[,2], col="red")
# Plot Avg Silhouette values across # of clusters for CLARA
lines(clustPerfSI[,1], clustPerfSI[,3], col="blue")
# Grid lines
abline(v = 1:10, lty=2, col="light grey")
# Add legend
legend("topright", legend=c("K-means","CLARA"), col=c("red","blue"), lty=1, lwd=1)
dev.off()

### -------------------------------------------------------
## Reproject, plot, and save k-means plot with 3 clusters
# Band = 2 corresponds with 3 groups / clusters, which have the highest silhouette index
clst3 = rstKM[[2]]
clst3_proj <- projectRaster(clst3, crs=crs(desired_crs))

# Transform raster to matrix to data frame
clst3_proj = rasterToPoints(clst3_proj)
clst3_proj = as.data.frame(clst3_proj)
colnames(clst3_proj) [3]<- "Cluster"

# Plot
p2 = ggplot() +
  geom_raster(data = clst3_proj , aes(x = x, y = y, fill=Cluster)) +
  theme(panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Easting", y="Northing") +
  ggtitle("Siskiyou Climate Regions\nbased on K-Means Clustering") 

p2

ggsave("map_kmeans_temp_precip_3clusters.jpeg", p2)

### -----------------------------------------------------
### Reproject, plot, and save CLARA plot with 3 clusters
# Band = 2 corresponds with 3 groups / clusters

clst3_CLARA = rstCLARA[[2]]
clst3_CLARA_proj <- projectRaster(clst3_CLARA, crs=crs(desired_crs))

# Transform raster to matrix to data frame
clst3_CLARA_proj = rasterToPoints(clst3_CLARA_proj)
clst3_CLARA_proj = as.data.frame(clst3_CLARA_proj)
colnames(clst3_CLARA_proj) [3]<- "Cluster"

# Plot
p3 = ggplot() +
  geom_raster(data = clst3_CLARA_proj , aes(x = x, y = y, fill=Cluster)) +
  theme(panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Easting", y="Northing") +
  ggtitle("Siskiyou Climate Regions\nbased on CLARA Clustering") 

p3

install.packages("gridExtra")               
library("gridExtra") 

ggsave("climate_regions.jpeg", grid.arrange(p2, p3, ncol=2), height = 5, width = 9, units = "in")
