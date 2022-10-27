# Script used to populate each raster cell of IC map with FIA data
# Plot selection based on coordinates, spp. distribution maps, forest type, and stand age
#
# Original script written by Matthew Duveneck then heavily modified by Jared Oyler and Melissa Lucash
# Adapted and modifed by Alison Deak using 2017 LEMMA GNN data
#
# First, the script pulls the coords data from a raster of the landscape
# Then it reads in all the FIA data from the study area
# Then it pulls in all the species distribution maps 
# The script goes to each raster cell and asks: which PLT_CN makes the most sense for this cell?
# The best plot is the one which is the closest to the cell, actually contains ALL the species 
# (plot species = spp distribution maps), and is the most similar in stand age. 

library(sp)
library(sf)
library(raster)
library(rgdal)
library(plyr)
library(tidyverse)

memory.limit(2000000) # needed to increase memory limit to run for loop
options(scipen=99999)
start_time <- date()

# Used external hard drive to avoid filling up temporary files
# rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

wd <- ("/Users/alisondeak/Desktop/Landis-Siskiyous/")
setwd(wd)

#### --------------------------------------

# Pulled in one map to have a reference for "base" map
# DIDN'T D0 - Extract and reproject coordinates from Albers to WGS84 lon,lat (coordinates of FIA plot data)
### I changed this to the UTM NAD83 projection for consistency with all of my maps. 
desired_CRS <- '+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'
one_map <- raster(paste0(wd, "ecoregions/spatial_data/ecoregions.tif"))
x_y_df <- as.data.frame(coordinates(one_map))
coordinates(x_y_df) <- ~ x + y
proj4string(x_y_df) <- proj4string(one_map) 
UTM_coords_df <- as.data.frame(spTransform(x_y_df, CRS(desired_CRS)))
colnames(UTM_coords_df) <- c("Northing", "Easting")

### -----------------------------------------

# Switch from maps over to FIA data.  
# use shapefile of FIA plots to get easting/northing of each PLT_CN
FIA_plots_shp <- st_read(paste0(wd, "tree_ages/FIA/FIA_plot_buffer/FIA_plot_buffer.shp"))
FIA_plots_shp <- st_transform(FIA_plots_shp, desired_CRS)
FIA_plots_df <- as.data.frame(FIA_plots_shp)

# separate geometry into easting/northing
plot_matrix <- FIA_plots_df %>%         
  mutate(EASTING  = unlist(map(FIA_plots_df$geometry,2)),
         NORTHING = unlist(map(FIA_plots_df$geometry,1)))
plot_matrix <- plot_matrix[,c("CN", "EASTING", "NORTHING")] # can't find unique values with additional columns
colnames(plot_matrix) <- c("PLT_CN", "EASTING", "NORTHING")
plot_matrix <- unique(plot_matrix)

# Add plot numbers as row names so we can quickly access easting/northing data 
# using plot number and convert from dataframe to matrix
row.names(plot_matrix) <- as.character(plot_matrix[,"PLT_CN"])
plot_matrix <- plot_matrix[,c("EASTING", "NORTHING")]
plot_matrix <- as.matrix(plot_matrix)

# Used this to find total number of plants
cn_cnts <- nrow(plot_matrix)
print(cn_cnts)

### --------------------------------------------
# This directory is where the maps where the spp everywhere are located.  
spp_occ_r <- list.files(paste0(wd, 'IC_map/inputs/NECNDistributionMaps'), pattern = "*.tif", full.names=TRUE)
spp_occ_r <- stack(spp_occ_r)
spp_occ_df <- as.data.frame(spp_occ_r, nrow = ncell(spp_occ_r))
spp_occ_df[is.na(spp_occ_df)] <- 0

# change column names to match PLANTS IDs of species in plot_matrix
colnames(spp_occ_df) <- c("ARME", "CADE27", "CHCH7", "LIDE3", "PIJE", "PILA", "PIMO3", "PSME")

# write.csv(spp_occ_df, "species_occurrence.csv") # useful for checking data frame later

### ---------------------------------------
# create stand age dataframe (based on LEMMA data)
StandAge <- raster("Final_StandAge.tif")
StandAge_df <- as.data.frame(StandAge)
roundUp <- function(x) 10*ceiling(x/10) # Round up stand ages to multiples of ten
StandAge_df <- roundUp(StandAge_df)

### ---------------------------------------
# LEMMA forest type - created in script 8.1 lines 57-60
fortyp <- read.csv("IC_map_script_inputs_and_outputs/fortypba_df.csv")
fortyp_splt <- str_split_fixed(fortyp$FORTYPBA, '/', 2)
fortyp_df <- cbind(fortyp, fortyp_splt)
fortyp_df <- fortyp_df[,c("1","2")]
colnames(fortyp_df) <- c("dom_veg_1", "dom_veg_2")
fortyp_df <- fortyp_df %>% mutate_all(na_if,"")

# FIA forest type - created in script 5, line 188-205
FIA_fortyp <- read.csv("IC_map_script_inputs_and_outputs/FIA_forest_type.csv")
FIA_fortyp <- unique(FIA_fortyp[,2:4])
rownames(FIA_fortyp) <- as.character(FIA_fortyp[,"PLT_CN"])
FIA_fortyp <- FIA_fortyp[complete.cases(FIA_fortyp),]


rm(fortyp, fortyp_splt)

### -----------------------------------------

# input csv with PLT_CN, each species in PLT_CN, and their predicted age
spp_file <- read.csv("IC_map_script_inputs_and_outputs/Final_FIASpeciesData_Age.csv")
spp_file <- spp_file[,c("PLT_CN", "PLANTS.ID", "Age")] # removed an additional unneeded column with ID number.
spp_file <- unique(spp_file)

# create data frame with each species' max age (proxy for stand age) calculated from FIA data within cell. 
# column names are PLANTS IDs and row names are PLT_CNs
PLT_species <- spp_file %>%
  group_by(PLT_CN, PLANTS.ID) %>%
  dplyr::summarise(Age = max(Age)) %>%
  pivot_wider(id_cols=PLT_CN, names_from = 'PLANTS.ID', values_from=Age, values_fill=NA)
PLT_species <- PLT_species[c("PLT_CN", "ARME", "CADE27", "CHCH7", "LIDE3", "PIJE", "PILA", "PIMO3", "PSME")]
PLT_age_df <- as.data.frame(PLT_species)
row.names(PLT_age_df) <- as.character(PLT_age_df[,"PLT_CN"])
PLT_age_df <- PLT_age_df[,2:ncol(PLT_age_df)]
PLT_age_df[is.na(PLT_age_df)] <- 0

# subset tree species with species distribution maps
# create binary data frame with species in plot indicated by '1'
PLT_species[2:ncol(PLT_species)] <- sapply(PLT_species[2:ncol(PLT_species)], as.logical)
PLT_species[2:ncol(PLT_species)] <- sapply(PLT_species[2:ncol(PLT_species)], as.numeric)

# again, assign PLT_CN as row name
uspecies_treedata <- as.data.frame(PLT_species)
row.names(uspecies_treedata) <- as.character(uspecies_treedata[,"PLT_CN"])
uspecies_treedata <- uspecies_treedata[,2:ncol(uspecies_treedata)]
uspecies_treedata[is.na(uspecies_treedata)] <- 0

### -----------------------------------------
# create stand age dataframe based on predicted ages of trees in FIA plots
con_df <- read.csv("IC_map_script_inputs_and_outputs/merged_tree_conditions.csv") # input csv with average stand age for each plt cn
FIA_stand_age <- con_df %>% select(PLT_CN, STDAGE_mean) %>% distinct()
FIA_stand_age$STDAGE_mean <- roundUp(FIA_stand_age$STDAGE_mean)
FIA_stand_age$PLT_CN <- as.character(FIA_stand_age$PLT_CN)
row.names(FIA_stand_age) <- as.character(FIA_stand_age[,"PLT_CN"])

# Remove all unneeded objects before running for loop 
rm(FIA_plots_shp, FIA_plots_df, spp_file, StandAge, desired_CRS, x_y_df, spp_occ_r, con_df, cn_cnts) 

### --------------------------------------------

# This is where I loop through all the maps for every raster cell and
# look at stand age, veg type, and plot distance to determine the best PLT_CN for each cell

# # Set up to run script in parallel to speed up processing time
# library(foreach)
# library(doParallel)
# library(parallel)

# # determine how many cores/processors you have:
# cores <- parallel::detectCores()-1
# 
# # Establish the clusters to use. Subtract at least one from total number of
# # cores so that computer can continue to maintain other basic functions
# cl <- parallel::makeCluster(cores)
# doParallel::registerDoParallel(cl)
# 
# # specify that %dopar% should run sequentially
# registerDoSEQ()

SELECTED_N_MAT <- matrix(0, nrow = nrow(spp_occ_df), ncol=1)
i_process <- as.matrix(rowSums(spp_occ_df) != 0)

for (P in 1:nrow(i_process)) { # helpful for debugging
# SELECTED_N_MAT <- foreach(P=1:nrow(i_process), .combine='rbind') %dopar% {
  if (i_process[[P]] == TRUE) { 
    Easting_P <- UTM_coords_df[P,2]
    Northing_P <- UTM_coords_df[P,1]  
    P_species <- spp_occ_df[P,]
    StandAge_P <- StandAge_df[P,]
    fortype_P <- fortyp_df[P,]
    matching_spp <- match_df(uspecies_treedata, P_species)
    
    if(nrow(matching_spp) > 0) {
      # if FIA plot species matches LEMMA species in cell
      spp_plt_cns <- row.names(matching_spp)
      matching_spp_pltcns <- FIA_stand_age[spp_plt_cns,]
      matching_ages <- any(matching_spp_pltcns$STDAGE_mean == StandAge_P)
    
    if(matching_ages == TRUE) {
      # if FIA plot stand age is equal to LEMMA stand age 
      age_plt_cns <- which(matching_spp_pltcns$STDAGE_mean == StandAge_P)
      matching_age_pltcns <- matching_spp_pltcns[age_plt_cns, "PLT_CN"]
      age_plt_UTM <- as.matrix(plot_matrix[matching_age_pltcns, c("EASTING", "NORTHING"), drop=FALSE]) # get coords of plots
      age_plt_dists <- spDistsN1(age_plt_UTM, c(Northing_P, Easting_P)) # find distances of plots
      selected_cn <- matching_age_pltcns[which.min(age_plt_dists)]
      }
    else {
      # Find nearest plot where with matching species
      spp_plt_UTM <- plot_matrix[spp_plt_cns, , drop=FALSE] 
      spp_plt_dists <- spDistsN1(spp_plt_UTM, c(Northing_P, Easting_P)) 
      selected_cn <- spp_plt_cns[which.min(spp_plt_dists)]
    }
    }
    else {
      # no grid cell had species that were all found at the grid cell -- select nearest plot with forest type and stand age
      matching_fortyp <- any(FIA_fortyp$FORTYPCD_SPP == fortype_P$dom_veg_1, na.rm = TRUE)
      
      if(matching_fortyp == TRUE) {
        # if LEMMA forest type matches FIA forest types
        matching_fortyp_rows <- which(FIA_fortyp$FORTYPCD_SPP == fortype_P$dom_veg_1)
        fortyp_plt_cns <- rownames(FIA_fortyp[matching_fortyp_rows,])
        matching_fortyp_age_cells <- any(FIA_stand_age[fortyp_plt_cns,2] == StandAge_P, na.rm=TRUE)
        
        if (matching_fortyp_age_cells == TRUE) { 
          # if FIA fortyp stand age = cell stand age, find nearest plot and select
          matching_fortyp_age <- which(FIA_stand_age[fortyp_plt_cns,2] == StandAge_P)
          matching_fortyp_age_cns <- rownames(FIA_stand_age[matching_fortyp_age,])
          fortyp_age_plt_UTM <- as.matrix(plot_matrix[matching_fortyp_age_cns, c("EASTING", "NORTHING"), drop=FALSE]) # get coords of plots
          fortyp_age_plt_dists <- spDistsN1(fortyp_age_plt_UTM, c(Northing_P, Easting_P)) # find distances of plots
          selected_cn <- matching_fortyp_age_cns[which.min(fortyp_age_plt_dists)]
        }
        else { 
          # if fortyp stand age doesn't match, find closest plot with matching forest type
          fortyp_plt_UTM <- plot_matrix[fortyp_plt_cns, , drop=FALSE] 
          fortyp_plt_dists <- spDistsN1(fortyp_plt_UTM, c(Northing_P, Easting_P)) 
          selected_cn <- fortyp_plt_cns[which.min(fortyp_plt_dists)]
          }
        } 
      else {
        # if unable to match by species or forest type, select closest plot
        plt_dists <- spDistsN1(plot_matrix, c(Northing_P, Easting_P)) 
        selected_cn <- rownames(plot_matrix)[which.min(plt_dists)]      
      }
    } 
  }
  else {
    # row sum of spp_occ_df equal to zero
    selected_cn <- 0
  }
  SELECTED_N_MAT[P,1] <- selected_cn
}

write.csv(SELECTED_N_MAT, "plot_codes_matrix_8spp.csv") # TO BE SAFE!!!

# parallel::stopCluster(cl)

end_time <- date()

### ----------------------------------------------------------------

SELECTED_N_DF <- as.data.frame(SELECTED_N_MAT)
colnames(SELECTED_N_DF) <- "PLT_CN"

#print(SELECTED_N_MAT)
SELECTED_N_DF[is.na(SELECTED_N_DF)] <- 0
number_rows <- nrow(SELECTED_N_DF)
print(number_rows) # 8,828,190

#Evaluating how well the script worked.
#How many real PLT_CNs got assigned to the map.
number_final_plots <- filter(SELECTED_N_DF, PLT_CN != 99999 & PLT_CN != 0)
print(nrow(number_final_plots)) # 1,104,633

#How many zeros originally
emptys <- as.data.frame(rowSums(spp_occ_df))
number_zeros_original<-nrow(filter(emptys, rowSums(spp_occ_df)==0))
print(number_zeros_original) # 7,033,073

#How many zeros got assigned
number_zeros<-filter(SELECTED_N_DF, PLT_CN==0)
print(nrow(number_zeros)) #7,033,073

#How many raster cells weren't able to be "matched" in any of the 5 maps.
number_99999<-(filter(SELECTED_N_DF, PLT_CN==99999))
print(nrow(number_99999)) #690,484

### ------------------------------------------------------------

# Converting the matrix to map codes to X, rather than the long PLT_CN numbers of FIA

unique_plot_CN <- as.numeric(unique(SELECTED_N_DF[,"PLT_CN"]))

unique_plot_CN_ordered<-sort(unique_plot_CN)
number_rows_plots<-length(unique_plot_CN_ordered)

Map_code_nozero <- seq(2, (number_rows_plots-1))
number_rows_mapcodes <- length(Map_code_nozero)
Map_code <- c(0, 1, Map_code_nozero)
Map_code_rows <- length(Map_code)

final_code_LUT <- as.data.frame(cbind(unique_plot_CN_ordered, Map_code))
#final_codes<-merge(x=SELECTED_N_MAT, y=final_code_LUT, by.x="PLT_CN", by.y="unique_plot_CN_ordered", all=TRUE)

SELECTED_N_DF$PLT_CN <- as.numeric(SELECTED_N_DF$PLT_CN)
final_codes <- left_join(SELECTED_N_DF, final_code_LUT, by=c("PLT_CN"="unique_plot_CN_ordered"))
colnames(final_codes)<-c("PLT_CN", "Map_code")

#Merge_test<-merge(final_codes, tree_data_merged, "PLT_CN", by.y="PLT_CN", all.x=TRUE)
#Merged_NAs_Plots<-Merge_test[!Merge_test(Merged_Tree_Cond$PLT_CN),]
#write.csv(Merge_test,file="I:/Research/Shares/lucash_lab/Lucash/VIFF/Making_IC_Map/MergeTest.txt")

write.csv(final_codes,file="final_codes.csv")

