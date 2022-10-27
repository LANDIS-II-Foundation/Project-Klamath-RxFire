library(raster)
library(sf)
library(rgdal)
library(terra)

wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous" 
setwd(wd)

filePath_SppDist <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/LEMMA_2017/rasters/IntFiles_logical"
filePath_StandDist <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/LEMMA_2017/rasters/IntFile_notLogical"
filePath_DomTrees <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/LEMMA_2017/rasters/CharFiles"
filePath_DomTreesCodes <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/LEMMA_2017/documentation"
filePath_output <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/FinalProducts/NECNDistributionMaps"


rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

# read in and reproject landscape shape file
landscapeShFile <- "FinalProducts/landscape_shape/landscape_shape.shp"
landscapeShp <- st_read(landscapeShFile)
desired_crs <- crs("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
landscape.reproj <- st_transform(landscapeShp, desired_crs)
landscape <- as_Spatial(landscape.reproj)
rm(landscapeShFile, landscape.reproj, landscapeShp) # make some space

# Read in Ecoregions Raster and create object from extent
Ecoregions <- raster("FinalProducts/FinalEcoRegions.tif")
desired_extent <- extent(Ecoregions)

# Read in species files that will need to be converted to logical 
# these were put in a separate folder than other LEMMA files
Spp_InputPath <- list.files(path = filePath_SppDist, pattern = "*.tif", full.names=TRUE)
Spp <- lapply(Spp_InputPath, raster)
SppStack <- stack(Spp)
Spp_reproj <- projectRaster(SppStack, Ecoregions, method = 'bilinear', res=30)
# writeRaster(Spp_reproj, filename=names(Spp_reproj), bylayer=TRUE, format="GTiff")

rm(all_Spp_Files, Spp) # make some space

# reproject and save rasters - these take a long time to process. 
Spp_reproj <- projectRaster(SppStack, Ecoregions, method = 'bilinear', res=30)
writeRaster(Spp_reproj, filename=names(Spp_reproj), bylayer=TRUE, format="GTiff")

# change to binary values - we only need to know whether or not the species is there
# convert to SpatRaster and use terra package to expand raster to box with NAs - convert NAs to zeroes
# return to raster
logicalFx <- function(x) {as.integer(as.logical(x))}
Spp_logical <- calc(Spp_reproj, logicalFx)

# use mask function to clip the rasters to our study area
Spp_mask <- mask(Spp_logical, landscape) 

# convert to SpatRaster and use terra package to expand raster to box with NAs - convert NAs to zeroes
Spp_rast <- rast(Spp_mask)
Spp_expand <- expand(Spp_rast, desired_extent)
Spp_expand[is.nan(Spp_expand)] <- 0 # remove nans

Final_Spp_distribution <- stack(Spp_expand) # convert back to rasterstack

# Rename stacks
names(Final_Spp_distribution) <- c("arme_dist", "cade27_dist", "chch7_dist", "chla_dist", "lide3_dist", "piat_dist", 
                                   "pije_dist", "pila_dist", "pimo3_disst", "pipo_dist", "psme_dist", "quch2_dist")
# write to file
setwd(filePath_output)
writeRaster(Final_Spp_distribution, names(Final_Spp_distribution), bylayer=TRUE, format='GTiff', overwrite=TRUE)

rm(Spp_reproj, Spp_logical, Spp_mask, SppStack, logicalFx, Spp_rast, Spp_expand, filePath_SppDist)

### -----------------------------------
# Repeat for stand age and height maps
# change filePath to reflect file with these data
# setwd(filePath_DomTrees)
# DomTrees_Files <- list.files(path = filePath_DomTrees, pattern = "*.tif", full.names=TRUE)
# DomTrees <- lapply(DomTrees_Files, raster)
# DomTreesStack <- stack(DomTrees)
# DomTrees_reproj <- projectRaster(DomTreesStack, Ecoregions, method = 'ngb', res=30)
# 
# rm(DomTrees_Files, DomTrees, DomTreesStack) # make some space
# 
# # use mask function to clip the rasters to our study area
# DomTrees_mask <- mask(DomTrees_reproj, landscape) 
# 
# # convert to SpatRaster and use terra package to expand raster to box with NAs - convert NAs to zeroes
# DomTrees_rast <- rast(DomTrees_mask)
# DomTrees_expand <- expand(DomTrees_rast, desired_extent)
# DomTrees_expand[is.nan(DomTrees_expand)] <- 0 # remove nans
# 
# Final_DomTrees_distribution <- stack(DomTrees_expand) # convert back to rasterstack
# 
# # Rename stacks
# names(Final_DomTrees_distribution) <- c("Final_CONPLBA", "Final_FORTYPBA", "Final_HDWPLBA", "Final_TREEPLBA")
# 
# # write to file
# setwd(filePath_output)
# writeRaster(Final_DomTrees_distribution, names(Final_DomTrees_distribution), bylayer=TRUE, format='GTiff')
# 
# rm(DomTrees_reproj, DomTrees_mask, DomTrees_rast, DomTrees_expand)

### -----------------------------------
# Repeat for dominant tree maps
# change filePath to reflect file with these data
setwd(filePath_StandDist)
Stand_Files <- list.files(path = filePath_StandDist, pattern = "*.tif", full.names=TRUE)
Stands <- lapply(Stand_Files, raster)
StandsStack <- stack(Stands)
Stands_reproj <- projectRaster(StandsStack, Ecoregions, method = 'bilinear', res=30)

rm(Stand_Files, Stands, StandsStack, Stand_InputPath, Stands, StandsStack) # make some space

# use mask function to clip the rasters to our study area
Stands_mask <- mask(Stands_reproj, landscape) 

# convert to SpatRaster and use terra package to expand raster to box with NAs - convert NAs to zeroes
Stands_rast <- rast(Stands_mask)
Stands_expand <- expand(Stands_rast, desired_extent)
Stands_expand[is.nan(Stands_expand)] <- 0 # remove nans

Final_Stands_distribution <- stack(Stands_expand) # convert back to rasterstack

# Rename stacks
names(Final_Stands_distribution) <- c("Final_StandAge", "Final_StandHeight")
StandAge <- raster(Final_Stands_distribution, 1)
StandHeight <- raster(Final_Stands_distribution, 2)

# round up species ages to multiples of ten to match tree ages
roundUp <- function(x) 10*ceiling(x/10)
StandAge_recalc <- calc(StandAge, roundUp)

# write to file
FinalProducts_file <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/FinalProducts"
setwd(FinalProducts_file)
writeRaster(StandAge_recalc, filename="Final_StandAge", datatype='INT4S', format='GTiff', overwrite=TRUE)
writeRaster(StandHeight, filename="Final_StandHeight", datatype='INT4S', format='GTiff', overwrite=TRUE)

rm(Stands_reproj, Stands_mask, Stands_rast, Stands_expand, Final_Stands_distribution, StandAge, filePath_StandDist, roundUp)


