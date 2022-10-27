# -----------------------------------------------
## Script to create buffer around landscape, identify plt_cns within buffer, and create conditions table 
# -----------------------------------------------

library(sf)
library(rgdal)
library(raster)
library(rgeos)

wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous"
setwd(wd)
removeTmpFiles(0.5) # good practice - raster files fill up temp folder FAST
options(scipen=999)

# Used external hard drive to avoid filling up temporary files
rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

## input landscape shapefile
desired_CRS <- "+proj=longlat +datum=WGS84 +no_defs" # creates new landscape CRS to project data in lat/long
landscapeShFile <-"FinalProducts/landscape_shape/landscape_shape.shp"
landscape <- readOGR(landscapeShFile)
landscape_reproj <- spTransform(landscape, desired_CRS)
plot(landscape_reproj, axes=TRUE)

# create buffer around landscape
landscape_buffer <- gBuffer(landscape_reproj, byid=FALSE, width=1.0, capStyle="ROUND")
plot(landscape_buffer, axes=TRUE)
landscape_buffer_sf <- st_as_sf(landscape_buffer)

## read and bind shrubs and plots data, join datasets, and create spatial feature
CAplots <- read.csv("FIA/CA_PLOTGEOM.csv")
ORplots <- read.csv("FIA/OR_PLOTGEOM.csv")
plots <- rbind(CAplots, ORplots)
plots_sf <- st_as_sf(plots, coords = c("LON", "LAT"), na.fail = FALSE, crs = desired_CRS)

### clip FIA plots to landscape and create dataframe of species in landscape
clip <- st_intersection(plots_sf, landscape_buffer_sf)
plot(clip, axes=TRUE)

## filter out plot not in the Central California Coast Ecoregion (M261A)
## Source: https://doi.org/10.2737/WO-GTR-76D
eco_clip <- filter(clip, grepl("M261A", ECOSUBCD)) 
plot(eco_clip)  

# rbind and join conditions table to plot_clip
CAcond <- read.csv("FIA/CA_COND.csv")
ORcond <- read.csv("FIA/OR_COND.csv")
FIAConditions <- rbind(CAcond, ORcond)

cond_clip <- left_join(eco_clip, FIAConditions, by="CN")

### write object to shapefile
st_write(obj = cond_clip, dsn = file.path("FinalProducts/FIA_plot_buffer/FIA_plot_buffer.shp"), layer="PLT_CN")

rm(ORplots, plots, plots_sf, landscape_reproj, desired_CRS, landscape, 
   landscapeShFile, landscape_buffer, clip, CAplots, CAcond, ORcond)

### -----------------------------------------------------

cond_clip <- readOGR("FinalProducts/FIA_plot_buffer/FIA_plot_buffer.shp")

library(plyr)

### Create data set with all trees
CAtrees <- read.csv("FIA/CA_TREE.csv")
ORtrees <- read.csv("FIA/OR_TREE.csv")
TREES <- rbind(CAtrees, ORtrees)

### join tree data and plots, remove trees with height (HT) <= 0 
Plots <- eco_clip[c("CN", "geometry")] ## create new object with only needed columns
colnames(Plots) <- c("PLT_CN", "geometry")
Plots_Trees <- left_join(Plots, TREES, by="PLT_CN")

### get rid of zeros and missing cells (9999)
### find number of unique CNs
PLT_CN_excludeMissingCells <- filter(Plots_Trees, PLT_CN > 100000)
PLT_CN_excludeMissingCells <- as.data.frame(PLT_CN_excludeMissingCells)
PLT_CN_unique_vector <- unique(PLT_CN_excludeMissingCells[,"PLT_CN"]) #number of PLT CNs

### remove trees without any height data
### subset to years needed 
### remove zeros and rows with no data

tree_data_HT <- filter(PLT_CN_excludeMissingCells, HT > 0)
colnames(tree_data_HT) 
tree_data <- tree_data_HT[,c("PLT_CN","CN","INVYR","SPCD","CONDID","HT","DIA","BHAGE", 
                             "TPA_UNADJ","TPAMORT_UNADJ","TPAREMV_UNADJ","TPAGROW_UNADJ",
                             "DRYBIO_BOLE","DRYBIO_TOP","DRYBIO_STUMP","DRYBIO_SAPLING",
                             "DRYBIO_WDLD_SPP","DRYBIO_BG","CARBON_AG","CARBON_BG")]

years <- c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
tree_data_SelectYears <- tree_data[tree_data$INVYR %in% years,]
tree_data_SelectYears[is.na(tree_data_SelectYears)] <- 0
Trees_RemoveZeroes <- filter(tree_data_SelectYears, INVYR > 0)

### covert to data frame. Check dataframe
### subset to only needed plots
tree_data_df <- as.data.frame(Trees_RemoveZeroes)
PLT_CN_unique_trees <-unique(tree_data_df[,"PLT_CN"])
print(length(PLT_CN_unique_trees))

tree_data_SelectPlots <- tree_data_SelectYears[tree_data_SelectYears$PLT_CN %in% PLT_CN_unique_vector,]

spp_file <- read.csv("tree_ages/InlandCaliforniaFSV_Coefficients.csv")
spp <- spp_file[,c("SPCD", "PLANTS.ID")]
TREE_df <- tree_data_SelectPlots[tree_data_SelectPlots$SPCD %in% spp$SPCD,]

TREE_spp <- left_join(TREE_df, spp, by="SPCD")
head(TREE_spp)
unique(TREE_spp$SPCD)
write.csv(TREE_spp, "FinalProducts/IC_map_script_inputs_and_outputs/tree_df.csv")

rm(CAtrees, eco_clip, ORtrees, Plots_Trees, PLT_CN_excludeMissingCells, PLT_CN_unique_trees,
   PLT_CN_unique_trees, PLT_CN_unique_vector, spp, spp_file, tree_data, tree_data_df, tree_data_HT, 
   TREES, Trees_RemoveZeroes, years, TREE_df, tree_data_SelectPlots, tree_data_SelectYears)

### --------------------------------------------------------

## join FIA conditions table to plot_cn
Plots_Cond <- left_join(Plots, FIAConditions, by="PLT_CN")
Plots_Cond[is.na(Plots_Cond)] <- 0
Conditions_ExcludeMissingValues <- filter(Plots_Cond, SICOND > 0)
print(nrow(Plots_Cond))
print(nrow(Conditions_ExcludeMissingValues))

#Remove plots that are not forested.  COND class 1 is forested.
COND_only_forests <- subset(Conditions_ExcludeMissingValues, Conditions_ExcludeMissingValues$COND_STATUS_CD == 1) 
COND_only_forests <- as.data.frame(COND_only_forests)
print(nrow(COND_only_forests))

### Use this to fill in the missing data, i.e. avg site index for entire landscape.
average_Site_Condition<-round(mean(COND_only_forests$SICOND), digits=-1)

rm(CAcond, ORcond, FIAConditions, Plots_Cond, Conditions_ExcludeMissingValues, Plots)

#### --------------------------------------------------------
# Use this to calculate the average stand age and site index and fill in any missing data.
COND_matrix <- NULL
for (z in TREE_spp$PLT_CN) {  #for each PLT_CN
  each_plot_subset <- filter(COND_only_forests, PLT_CN==z)
  number_plots <- nrow(each_plot_subset)
  stand_age_mean <- mean(each_plot_subset$STDAGE) #summing across cohort
  site_index_avg <- mean(each_plot_subset$SICOND_FVS) #summing across cohort
  if (number_plots == 0 || site_index_avg == 0) {
    site_index_mean <- average_Site_Condition
    stand_age <- 0
  }
  else {
    site_index_mean <- site_index_avg
    stand_age <- stand_age_mean
  }
  plot_matrix <- cbind.data.frame(z, stand_age, site_index_mean)
  colnames(plot_matrix)<-c("PLT_CN", "STDAGE_mean", "SICOND_mean")
  COND_matrix <- rbind(COND_matrix, plot_matrix)
}

print(nrow(COND_matrix))

# write COND_matrix to csv
COND_df <- as.data.frame(COND_matrix)
COND_df ## check to make sure stand age and site condition populated

unique_COND_df <- unique(COND_df)
write.csv(unique_COND_df, "FinalProducts/IC_map_script_inputs_and_outputs/site_conditions.csv")

rm(z, stand_age_mean, site_index_avg, site_index_mean, each_plot_subset, number_plots, stand_age, COND_df,
   COND_matrix, plot_matrix, average_Site_Condition, COND_only_forests, stand_age, COND_only_forests)

## ----------------------------------
# merge COND_df with TREE_df

Merged_Tree_Con <- left_join(TREE_spp, unique_COND_df, by="PLT_CN")

# Merged_Tree_Con <- NULL
# for (row.i in 1:nrow(TREE_spp)) {
#   Cond.i <- COND_df$PLT_CN[row.i]
#   Tree.i <- TREE_df$PLT_CN[row.i] 
#   if (Cond.i == Tree.i) {
#     StandAge <- COND_df$STDAGE_mean
#     SiteCondition <- COND_df$SICOND_mean
#   }
#   Merged_Tree_Con <- cbind.data.frame(TREE_df, StandAge, SiteCondition)
# }

cond_mat_revised <- cond_mat %>% mutate(SICOND_mean =  case_when(SICOND_mean == 0 ~ 80,
                                                                 SICOND_mean != 0 ~ SICOND_mean))
cond_mat_revised$SICOND_mean
write.csv(cond_mat_revised, "FinalProducts/IC_map_script_inputs_and_outputs/merged_tree_conditions.csv")

### ------------------------ 
# adding foresttype to condition matrix
cond_mat <- read.csv("FinalProducts/IC_map_script_inputs_and_outputs/merged_tree_conditions.csv")

CAcond <- read.csv("FIA/CA_COND.csv")
ORcond <- read.csv("FIA/OR_COND.csv")
FIAConditions <- rbind(CAcond, ORcond)

fortypcd <- FIAConditions[,c("PLT_CN", "FORTYPCD")]
fortypcd_join <- left_join(cond_mat, fortypcd, by="PLT_CN")
fortypcd_distinct <- fortypcd_join %>% group_by(PLT_CN) %>% dplyr::summarise(FORTYPCD = max(FORTYPCD)) %>% distinct()
# fortypes <- as.data.frame(fortypcd_join$FORTYPCD)
# write.csv(fortypes, "FIA/fortypes.csv") # write to CSV, fill in cells with dom tree plants.id, return to R
fortypes <- read.csv("FIA/fortypes.csv")

fortyp_cond_join <- left_join(fortypcd_distinct, fortypes, by="FORTYPCD")

write.csv(fortyp_cond_join, "FinalProducts/IC_map_script_inputs_and_outputs/FIA_forest_type.csv")
