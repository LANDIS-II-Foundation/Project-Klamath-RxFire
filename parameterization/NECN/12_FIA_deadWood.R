# Script reads FIA data output and computes the amount of dead coarse roots and makes a pretty map.
# modified from Melissa Lucash's script FIA_DeadWood_VIFF.R

##---------------------
library(rgdal)   #Needed for rasters
library(raster)  #Needed for rasters
library(tidyverse)

rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous" 
setwd(wd)

options(scipen=999) #This removes scientific notation in printing. Plot CNs are very long

##-------------------
# InitalDeadWoodSoil Map
##-------------------
years<-c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

Map_codes <-read.csv("FinalProducts/IC_map_script_inputs_and_outputs/final_IC_map_codes.csv")
Map_codes_unique <- Map_codes %>% filter(Map_code > 0)
PLT_CN_vector_unique <- Map_codes %>% filter(Map_code > 0) %>% dplyr::select(PLT_CN) %>% distinct()

print(nrow(PLT_CN_vector_unique))

Dead_data_CA  <- read.csv("FIA/CA_COND_DWM_CALC.csv")
Dead_data_OR  <- read.csv("FIA/OR_COND_DWM_CALC.csv")
Dead_data_all <- rbind(Dead_data_CA, Dead_data_OR)

Dead_data_InvYears <- filter(Dead_data_all, INVYR %in% years)

Dead_data <- Dead_data_InvYears[c(2, 9, 30, 43, 45, 56, 58, 69, 71, 81, 83)]
colnames(Dead_data)<-c("State", "Plot_CN", "CWD_CARBON_COND", "FWD_SM_CARBON_COND", "FWD_SM_CARBON_ADJ", 
                       "FWD_MD_CARBON_COND", "FWD_MD_CARBON_ADJ", "FWD_LG_CARBON_COND", "FWD_LG_CARBON_ADJ", 
                       "PILE_CARBON_COND", "PILE_CARBON_ADJ")
All_dead_Wood <- rowSums(Dead_data[c(3, 4, 6, 8,10)])
Final_Dead_Data <- cbind(Dead_data, All_dead_Wood)

fia.dead.plot <- Map_codes %>%
  filter(Map_code > 0) %>%
  left_join(Final_Dead_Data, by=c("PLT_CN"="Plot_CN")) %>%
  group_by(PLT_CN) %>%
  summarise(plot.dead.biomass = sum(All_dead_Wood,na.rm=T))

nrow(unique(fia.dead.plot)) #check

colnames(fia.dead.plot) <- c("Plot_CN", "all_dead_wood")

Merged_MapCode_dead <- left_join(Map_codes, fia.dead.plot, by=c("PLT_CN" ="Plot_CN"))

rm(All_dead_Wood, Dead_data, Dead_data_all, Dead_data_InvYears, Dead_data_CA, Dead_data_OR)

## Calculate FIA biomass

# Summing belowground biomass on every plot of FIA data. 
tree_data_OR <- read.csv("FIA/OR_TREE.csv")
tree_data_CA <- read.csv("FIA/CA_TREE.csv")
tree_data_all <- rbind(tree_data_CA, tree_data_OR)
tree_data_InvYears <- filter(tree_data_all, INVYR %in% years)

rm(tree_data_CA, tree_data_OR, tree_data_all)

tree_data_subset <- tree_data_InvYears[c(1:2, 4:5, 15:16, 121:122)]
tree_data_subset[is.na(tree_data_subset)] <-0

spp_file <- read.csv("tree_ages/InlandCaliforniaFSV_Coefficients.csv")
spp_code <- spp_file[,"SPCD"]

tree_data <- filter(tree_data_subset, SPCD %in% spp_code)

# Mattson and Zhang found an average of 3.3 Mg C/Ha in dead roots and 4.16 Mg C/Ha in live 
# roots in Mixed Conifer forests in the Sierra Nevada 
# CARBON_BG is defined as the carbon (pounds) of coarse roots > 0.1 inch in root diameter.
# So I assume dead coarse roots are ~44% of root volume

fia.cohort.plot <- Map_codes %>%
  left_join(tree_data, by="PLT_CN") %>%
  filter(Map_code > 0) %>%
  group_by(PLT_CN) %>%
  summarise(plot.biomass = sum(CARBON_BG,na.rm=T), # this is in lbs
            plot.BG.biomass_gm2 = (plot.biomass * 2 * 453.592) / 4046.86,
            plot.BG.biomass.perplot = plot.BG.biomass_gm2 * 0.44) %>%
  distinct()

mean_BG_C <- mean(fia.cohort.plot$plot.BG.biomass.percent)

final_fia_cohort_plot <- fia.cohort.plot %>% 
  mutate(plot.BG.biomass.perplot = ifelse(plot.BG.biomass.perplot == 0,
                                          mean_BG_C,
                                          plot.BG.biomass.perplot)) %>%
  left_join(Map_codes_unique, by="PLT_CN") 

final_fia_cohort_plot <- final_fia_cohort_plot[,c("PLT_CN","plot.biomass","plot.BG.biomass_gm2",
                                                  "plot.BG.biomass.perplot","Map_code" )]

colnames(final_fia_cohort_plot) <- c("PLT_CN", "BG_Biomass_acres_perplot", "BG_Biomass_gm2_perplot", "BG_dead_biomass_gm2_perplot", "Map_code")

final_IC_map <- raster("FinalProducts/final_IC_map.tif")
final_IC_map[final_IC_map==0] <- NA
IC_df <- as.data.frame(final_IC_map)

dead_coarse_roots_df <- IC_df %>% 
  left_join(final_fia_cohort_plot, by=c("final_IC_map"="Map_code")) 

dead_coarse_roots <- dead_coarse_roots_df %>% 
  dplyr::select(BG_dead_biomass_gm2_perplot) %>% as.matrix()

dead_coarse_roots[is.na(dead_coarse_roots)] <- 0

final_dead_coarse_roots <- final_IC_map
values(final_dead_coarse_roots) <- dead_coarse_roots
plot(final_dead_coarse_roots)

write.csv(dead_coarse_roots_df, file="FinalProducts/soils/dead_belowground.csv")
writeRaster(final_dead_coarse_roots, "FinalProducts/soils/dead_roots.tif", datatype='INT4S',overwrite=TRUE)

##-------------------
# InitalDeadWoodSurface Map
##-------------------

dead_down <- raster("soils/carbon_sd_mg_ha.img")
dead_standing <- raster("soils/carbon_dd_mg_ha.img")
surface_dead <- (dead_down + dead_standing) * 10 # convert from mg/ha to g/m^2

# final_IC_map[final_IC_map==0] <- NA

surf_dead_reproj <- projectRaster(surface_dead, final_IC_map, method = 'ngb', res=30)
surf_dead_crop <- crop(surf_dead_reproj, final_IC_map)
surf_dead_mask <- mask(surf_dead_crop, final_IC_map)
surf_dead_mask[is.na(surf_dead_mask)] <- 0
plot(surf_dead_mask)

jpeg("soils/dead_wood.jpg")
par(mfrow=c(1,2), mar = c(5, 5, 5, 7))
plot(surf_dead_mask, main= "Dead surface wood, g/m^2")
plot(final_dead_coarse_roots, main= "Dead coarse roots, g/m^2")
dev.off()

writeRaster(surf_dead_mask, "FinalProducts/soils/dead_surface.tif", datatype='INT4S', overwrite=TRUE)
