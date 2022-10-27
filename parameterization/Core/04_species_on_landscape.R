
### --------------------------------------
# Script to find the percentage of plots on landscape with each species 
### --------------------------------------

library(sf)
library(rgdal)
library(raster)
library(tidyverse)
options(scipen=999) ## removes scientific notation

wd("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous")
setwd(wd)

## input landscape shapefile
landscape_proj <- crs("+proj=longlat +datum=WGS84 +no_defs") # creates new landscape CRS to project data in lat/long
landscapeShFile <-"ecoregion_shape/ecoregion_shape.shp"
landscape <- st_read(landscapeShFile)
landscape.reproj <- st_transform(landscape, landscape_proj) #reporojects landscape to lat/long
plot(st_geometry(landscape.reproj), axes=TRUE)

## read and bind shrubs and plots data, join datasets, and create spatial feature
CAspp <- read.csv("FIA/CA_FIA/CA_P2VEG_SUBPLOT_SPP.csv")
CAplots <- read.csv("FIA/CA_FIA/CA_PLOTGEOM.csv")
ORspp <- read.csv("FIA/OR_FIA/OR_P2VEG_SUBPLOT_SPP.csv")
ORplots <- read.csv("FIA/OR_FIA/OR_PLOTGEOM.csv")
Spp <- rbind(CAspp, ORspp)
plots <- rbind(CAplots, ORplots)
Plotspp <- left_join(Spp, plots, by=c("PLT_CN" = "CN"))
plots_sf <- st_as_sf(Plotspp, coords = c("LON", "LAT"), na.fail = FALSE, crs=landscape_proj)

### clip FIA plots to landscape and create dataframe of species in landscape
clip <- st_intersection(plots_sf, landscape.reproj)
plot_clip <- clip[c(1,2,3,7,8,12:15)] ##create new object with only needed columns
plot(st_geometry(plot_clip), axes=TRUE) # check geometry
plotdf <- as_tibble(plot_clip) # convert sf to tibble
plotdf2 <- plotdf %>%      # separate geometry into lat/long
  mutate(LAT = unlist(map(plotdf$geometry,2)),
         LON = unlist(map(plotdf$geometry,1)))

### write object to shapefile
# st_write(obj = plot_clip, dsn = file.path("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/FIA/FIAPlotClip_V2.shp"))

### clean environment and make space
rm(CAspp, CAplots, ORspp, ORplots,Spp, plots, Plotspp, landscape_proj, landscapeShFile, landscape,
   landscape.reproj, clip, plots_sf, plotdf)

### move each species and % coverage to a column
### removed data privious to year 2011
### collapse layers into single row of coverage of each species in a plot, rather than a subplot
plotdf3 <- spread(plotdf2, VEG_SPCD, COVER_PCT)
plotdf3a <- plotdf3[plotdf3$INVYR.x > 2010, ] 
PlotSppSummary <- SubplotSppCover <- plotdf3a %>% 
  group_by(PLT_CN, GROWTH_HABIT_CD) %>%
  summarise_at(
    vars('2FB': 'ZIGAD'),
    sum,
    na.rm=TRUE
  )

PlotSppSummary[is.na(PlotSppSummary)] <- 0 # transforms NAs to zeroes

### sum of total number of plots on landscape
TotalPlots <- length(unique(PlotSppSummary$PLT_CN))
print(TotalPlots)

######################################################################################
### CALCULTE SHRUBS ON LANDSCAPE AT 1, 5, AND 10% THRESHOLDS

### remove all nonshrub species 
Plots.with.Shrubs <- PlotSppSummary[PlotSppSummary$GROWTH_HABIT_CD =="SH", ] 

### remove character vectors and return tibble to dataframe
### remove columns with zero species
Plots.Shrubs.df <- as.data.frame(Plots.with.Shrubs)
Shrubs.In.Plots <-  Plots.Shrubs.df[, colSums(Plots.Shrubs.df != 0.0) > 0.0]

### create function to find percentage of cells with a given species
PlotPct <- function(x) {
  output <- (sum(as.logical(x))) / TotalPlots
  return(output)
}

### create data frame with only species data
### find percentage of subplots with each species and write to a data frame
Shrubspecies <- Shrubs.In.Plots[,3:115] # where columns 3-151 are the percent cover of species in study area
Shrub.species.pct <- apply(Shrubspecies, 2, PlotPct)
Shrub.species.pct.df <- as.data.frame(Shrub.species.pct)

### create lists of species in more than 1%, 5%, and 10% of cells on landscape
threshold.1pct <- 0.01
threshold.5pct <- 0.05
threshold.10pct <- 0.1

Shrub.species.1pct <- subset(Shrub.species.pct.df, Shrub.species.pct.df[, 1] > threshold.1pct)
Shrub.species.5pct <- subset(Shrub.species.pct.df, Shrub.species.pct.df[, 1] > threshold.5pct)
Shrub.species.10pct <- subset(Shrub.species.pct.df, Shrub.species.pct.df[, 1] > threshold.10pct) ## has zero species

Shrub.species.1pct # look at datasets
Shrub.species.5pct
Shrub.species.10pct

### Clear environment
rm(plotdf3, plotdf3a, Plots.with.Shrubs, Plots.Shrubs.df, Shrubs.In.Plots, Shrubspecies, 
   Shrub.species.pct, Shrub.species.pct.df, plotdf2, SubplotSppCover)

######################################################################################
## REPEAT PROCESS FOR TREES ON LANDSCAPE
### remove all nonshrub species 
Plots.with.Trees <- PlotSppSummary[PlotSppSummary$GROWTH_HABIT_CD =="LT", ] 

### remove character vectors and return tibble to dataframe
### remove columns with zero species
Plots.Trees.df <- as.data.frame(Plots.with.Trees)
Trees.In.Plots <-  Plots.Trees.df[, colSums(Plots.Trees.df != 0.0) > 0.0]

### create data frame with only species data
### find percentage of subplots with each species and write to a data frame
Treespecies <- Trees.In.Plots[,3:40] # where columns 3-40 are the percent cover of species in study area
Tree.species.pct <- apply(Treespecies, 2, PlotPct)
Tree.species.pct.df <- as.data.frame(Tree.species.pct)

### create lists of species in more than 1%, 5%, and 10% of cells on landscape
Tree.species.1pct <- subset(Tree.species.pct.df, Tree.species.pct.df[, 1] > threshold.1pct)
Tree.species.5pct <- subset(Tree.species.pct.df, Tree.species.pct.df[, 1] > threshold.5pct)
Tree.species.10pct <- subset(Tree.species.pct.df, Tree.species.pct.df[, 1] > threshold.10pct) ## has zero species

Tree.species.1pct # look at datasets
Tree.species.5pct
Tree.species.10pct

### At this point both the shrub and tree species and percent coverage meeting the 1% threshold 
### are copied and pasted into an Excel spreadsheet and PLANTS IDs are identified by scientific and
### common name. 

### Clear environment
rm(Plots.with.Trees, Plots.Trees.df, Trees.In.Plots, Treespecies, Tree.species.pct, Tree.species.pct.df, 
   plotdf2,threshold.10pct, threshold.1pct, threshold.5pct, Tree.species.1pct, Tree.species.5pct, 
   Shrub.species.1pct, Shrub.species.5pct, Shrub.species.10pct, Tree.species.10pct)
