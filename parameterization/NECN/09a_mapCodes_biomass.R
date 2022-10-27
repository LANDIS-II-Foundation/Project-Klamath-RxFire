options(scipen=999) #This removes scientific notation in printing. Plot CNs are very long

library(tidyverse)
library(raster)
rasterOptions(tmpdir = "/Volumes/LaCie/Rtmp")

#Set up directories
dir <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous"
setwd(dir)

### ----------------------------------------------------------------
final_codes <- read.csv("FinalProducts/IC_map_script_inputs_and_outputs/final_codes.csv")
final_codes <- final_codes[,c("PLT_CN", "Map_code")]

## Read in template raster
raster_temp <- raster("FinalProducts/landscape_mask.tif")
mask_df <- as.data.frame(raster_temp)

### ------------------------------------------------------------

# create raster from final codes
map_codes_raster <- raster(nrow=nrow(raster_temp), ncol=ncol(raster_temp), 
                           ext=extent(raster_temp), crs=crs(raster_temp))
values(map_codes_raster) <- final_codes$Map_code

plot(map_codes_raster)

map_codes_df <- as.data.frame(map_codes_raster, xy=T)

###--------------------------------------------------------------------------------
# replace developed, agricultural, mining, and open water map codes with 0 to indicate they are inactive

EVT <- raster("landfire/landfire_existingvegtype.tif")
EVT_codes <- read.csv("landfire/landfire_existingvegtype.csv")
EVT_codes <- EVT_codes %>% dplyr::select(VALUE, EVT_NAME, EVT_PHYS)
EVT_reproj <- projectRaster(EVT, raster_temp, method="ngb") # reproject
EVT_mask <- mask(EVT_reproj, raster_temp)
EVT_mask[is.na(EVT_mask)] <- 0
plot(EVT_mask)

#----- join to map code and landfire code tables
EVT_df <- as.data.frame(EVT_mask, xy=T)
IC_EVT_bind <- cbind(final_codes, EVT_df)
colnames(IC_EVT_bind) <- c("PLT_CN", "Map_code","x", "y", "EVT_code")

EVT_codes_df <- IC_EVT_bind %>%
  left_join(EVT_codes, by=c("EVT_code"="VALUE")) %>% 
  dplyr::select(PLT_CN, Map_code, x, y, EVT_code, EVT_NAME, EVT_PHYS)

# replace developed land, agricultural lands, roads, mines, and open water with map code of zero
EVT_codes_reclass <- EVT_codes_df %>% 
  mutate(Map_code = ifelse(EVT_PHYS %in% c("Developed-Low Intensity", 
                                            "Developed-Medium Intensity", 
                                            "Developed-High Intensity", 
                                            "Quarries-Strip Mines-Gravel Pits-Well and Wind Pads",
                                            "Open Water",
                                            "Developed-Roads",
                                            "Agricultural"),0, Map_code))
final_map_codes <- EVT_codes_reclass %>% 
  dplyr::select(PLT_CN, Map_code) %>% 
  distinct() %>%
  arrange(Map_code) %>%
  subset(Map_code>0)

write.csv(final_map_codes,file="FinalProducts/IC_map_script_inputs_and_outputs/final_IC_map_codes.csv")

# final_map_codes <- read.csv("FinalProducts/IC_map_script_inputs_and_outputs/final_IC_map_codes.csv")

IC_map_codes_raster <- map_codes_raster
values(IC_map_codes_raster) <- EVT_codes_reclass$Map_code
plot(IC_map_codes_raster)

writeRaster(IC_map_codes_raster, "FinalProducts/final_IC_map.tif", datatype='INT4S', overwrite=T)

rm(EVT_codes, EVT, EVT_codes_df, EVT_df, EVT_mask, EVT_reproj, final_codes,
   IC_EVT_bind, map_codes_raster, spp_ages, unique_EVT_codes)

#--------------------------------------------------------------------------
final_spp <- read.csv("FinalProducts/Final_landscape_species.csv")
EVT_codes_reclass <- read.csv("FinalProducts/IC_map_script_inputs_and_outputs/final_IC_map_codes.csv")
all_spp_ages <- read.csv("FinalProducts/IC_map_script_inputs_and_outputs/Final_FIASpeciesData_Age.csv")

final_trees <- EVT_codes_reclass %>%
  left_join(all_spp_ages, by = "PLT_CN") %>%
  left_join(final_spp, by = c("PLANTS.ID"="PLANTS_ID")) %>%
  dplyr::select(PLT_CN, Map_code, SpeciesName, type, Age, CARBON_AG, TPA_UNADJ) %>%
  filter(type == "Tree") %>%
  mutate(biomass = (CARBON_AG * TPA_UNADJ) * 0.11208493) %>% # convert from lbs/acre to g/m^-2
  group_by(Map_code, SpeciesName, Age) %>% summarise(biomass = round(sum(biomass))) %>%
  dplyr::select(Map_code, SpeciesName, Age, biomass) %>%
  distinct() %>%
  filter(Map_code > 0 & biomass > 0) %>% # need to remove map code zeros and biomasses of zero
  arrange(Map_code)

colnames(final_trees) <- c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass")

p2 <- ggplot(final_trees, aes(x=CohortAge, y=CohortBiomass)) +
  geom_bar(stat = 'identity', aes(fill=SpeciesName)) +
  ggtitle("Biomass of trees per age cohort") +
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p2
ggsave(filename="final_tree_biomass.jpg", plot=p2)

#--------  
# create function to calculate shrub biomass
# equation from Johnson et al. (2017) allometric model for shrub biomass in the PWW Douglas-fir group
# calculate biomass in MgC/ha^-1 (metric ton/hectare)
ShrubBiomass <- function (MaxCover, MaxHT) {
  biomass_MgC <- (11.4 * (MaxCover * MaxHT)) / (0.6 + (MaxCover * MaxHT)) 
  return(biomass_MgC) }

final_shrubs <- EVT_codes_reclass %>%
  left_join(all_spp_ages, by = "PLT_CN") %>%
  left_join(final_spp, by = "FG_Short") %>%
  dplyr::select(PLT_CN, Map_code, SpeciesName, type, Age, LAYER, COVER_PCT) %>%
  filter(type == "Shrub") %>%
  mutate(height = case_when(LAYER == 1 ~ 0.61, # this is in meters
                            LAYER == 2 ~ 1.83,
                            LAYER == 3 ~ 4.88,
                            # height of plants in layer 4 based on literature of greatest height of species
                            LAYER == 4 & SpeciesName == "Frangula" ~ 6.10, 
                            LAYER == 4 & SpeciesName == "Rhododen" ~ 3.66,
                            LAYER == 4 & SpeciesName == "WhipMode" ~ 0.30,
                            LAYER == 4 & SpeciesName == "Arctosta" ~ 5.18)) %>%
  #mutate(COVER_PCT = COVER_PCT * 0.01) %>%
  #find biomass, convert to g/m^-2, round up to avoid biomass of 0
  mutate(biomass = ceiling(ShrubBiomass(COVER_PCT, height) * 100)) %>% 
  dplyr::select(Map_code, SpeciesName, Age, biomass) %>%
  distinct() %>%
  filter(Map_code > 0 & biomass > 0)  # need to remove map code zeros and biomasses of zero

colnames(final_shrubs) <- c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass")

p3 <- ggplot(final_shrubs, aes(x=CohortAge, y=CohortBiomass)) +
  geom_bar(stat = 'identity', aes(fill=SpeciesName)) +
  ggtitle("Biomass of shrubs per age cohort") +
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p3
ggsave(filename="final_shrub_biomass.jpg", plot=p3)

final_IC_file <- final_trees %>% rbind(final_shrubs) %>% arrange(MapCode)

write.csv(final_IC_file, "FinalProducts/final_IC_file.csv")

  

