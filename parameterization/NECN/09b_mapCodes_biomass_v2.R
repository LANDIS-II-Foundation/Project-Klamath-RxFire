options(scipen=999) #This removes scientific notation in printing. Plot CNs are very long

library(tidyverse)
library(raster)
rasterOptions(tmpdir = "/Volumes/Alison.Deak_440-223-4897/R_temp_dir")
options(scipen = 999) # necessary so that the full PLT_CNs will read in.


#Set up directories
dir <- "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/"
w_dir <- "/Volumes/Alison.Deak_440-223-4897/MS Research/LargeDataForLandis/"

### ----------------------------------------------------------------
final_codes <- read.csv(paste0(dir, "IC_map/outputs/map_codes_to_CN_crosswalk.csv"))[,2:3]

## Read in template raster
raster_temp <- raster(paste0(dir, "landscape/landscape_mask.tif"))
mask_df <- as.data.frame(raster_temp)

### ------------------------------------------------------------

# create raster from final codes
# map_codes_raster <- raster(nrow=nrow(raster_temp), ncol=ncol(raster_temp), 
#                            ext=extent(raster_temp), crs=crs(raster_temp))
# values(map_codes_raster) <- final_codes$Map_code
# plot(map_codes_raster)
# map_codes_df <- as.data.frame(map_codes_raster, xy=T)

map_codes_raster <- raster(paste0(dir, "IC_map/ic_map.tif"))
map_codes_df <- as.data.frame(map_codes_raster, xy=T) %>% 
  left_join(final_codes, by=c(IC_map = "Map_code"))

###--------------------------------------------------------------------------------
# replace developed, agricultural, mining, and open water map codes with 0 to indicate they are inactive

EVT <- raster(paste0(w_dir, "landfire/landfire_existingvegtype.tif"))
EVT_codes <- read.csv(paste0(w_dir, "landfire/landfire_existingvegtype.csv"))
EVT_codes <- EVT_codes %>% dplyr::select(VALUE, EVT_NAME, EVT_PHYS)
EVT_reproj <- projectRaster(EVT, raster_temp, method="ngb") # reproject
EVT_mask <- mask(EVT_reproj, raster_temp)
EVT_mask[is.na(EVT_mask)] <- 0
plot(EVT_mask)

#----- join to map code and landfire code tables
EVT_df <- as.data.frame(EVT_mask, xy=T)
IC_EVT_bind <- cbind(map_codes_df, EVT_df)
IC_EVT_bind <- IC_EVT_bind[,c(1:4,7)]
colnames(IC_EVT_bind) <- c("x", "y", "Map_code","PLT_CN", "EVT_code")

EVT_codes_df <- IC_EVT_bind %>%
  left_join(EVT_codes, by=c("EVT_code"="VALUE")) %>% 
  dplyr::select(x, y, PLT_CN, Map_code, EVT_code, EVT_NAME, EVT_PHYS)

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

write.csv(final_map_codes, paste0(dir, "IC_map/outputs/map_codes_to_CN_crosswalk.csv"))
# final_map_codes <- read.csv("FinalProducts/IC_map_script_inputs_and_outputs/final_IC_map_codes.csv")

IC_map_codes_raster <- map_codes_raster
values(IC_map_codes_raster) <- EVT_codes_reclass$Map_code
plot(IC_map_codes_raster)

sort(unique(values(IC_map_codes_raster)))

writeRaster(IC_map_codes_raster, paste0(dir, "IC_map/IC_map.tif"), datatype='INT4S', overwrite=T)

rm(EVT_codes, EVT, EVT_codes_df, EVT_df, EVT_mask, EVT_reproj, final_codes, IC_EVT_bind, map_codes_raster)

#--------------------------------------------------------------------------
# IC_map_codes_raster <- raster(paste0(dir, "IC_map/IC_map.tif"))
final_spp <- read.csv(paste0(dir, "landscape/Final_landscape_species.csv"))
# EVT_codes_reclass <- read.csv("FinalProducts/IC_map_script_inputs_and_outputs/final_IC_map_codes.csv")
all_spp_ages <- read.csv(paste0(dir, "tree_ages/FIA/Final_FIASpeciesData_Age.csv"))

FIA_dir <- "/Volumes/Alison.Deak_440-223-4897/MS Research/LargeDataForLandis/FIA/"
setwd(FIA_dir)
CA_TREE <- read.csv("CA_TREE.csv")
OR_TREE <- read.csv("OR_TREE.csv")
TREE <- rbind(CA_TREE, OR_TREE) 
rm(CA_TREE, OR_TREE)

final_trees <- final_map_codes %>%
  left_join(all_spp_ages, by = "PLT_CN") %>%
  left_join(final_spp, by = c("PLANTS.ID"= "PLANTS_ID")) %>%
  left_join(TREE) %>%
  dplyr::select(PLT_CN, Map_code, LANDIS_NAME, type, Age, CARBON_AG, TPA_UNADJ) %>%
  filter(type == "Tree") %>%
  mutate(biomass = (CARBON_AG * TPA_UNADJ) * 0.112085 * 2) %>% # convert from C lbs/acre to biomass g/m^-2
  group_by(Map_code, LANDIS_NAME, Age) %>% 
  dplyr::summarise(biomass = round(sum(biomass))) %>%
  dplyr::select(Map_code, LANDIS_NAME, Age, biomass) %>%
  distinct() %>%
  filter(Map_code > 0 & biomass > 0) %>% # need to remove map code zeros and biomasses of zero
  arrange(Map_code)

head(final_trees)

colnames(final_trees) <- c("MapCode", "LANDIS_NAME", "CohortAge", "CohortBiomass")

p2 <- ggplot(final_trees, aes(x=CohortAge, y=CohortBiomass)) +
  geom_bar(stat = 'identity', aes(fill=LANDIS_NAME)) +
  ggtitle("Total biomass of trees per age cohort") +
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p2

tree_bio <- 
  final_trees %>%
  group_by(LANDIS_NAME, CohortAge) %>%
  summarise_at(vars(CohortBiomass), list(meanAGB=mean, maxAGB=max))

p6 <- ggplot(tree_bio, aes(x=CohortAge, y=meanAGB, color=LANDIS_NAME)) +
  geom_line() + 
  ggtitle("Mean biomass per species") + 
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p6

p7 <- ggplot(tree_bio, aes(x=CohortAge, y=maxAGB, color=LANDIS_NAME)) +
  geom_line() + 
  ggtitle("Max biomass per species") + 
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p7

ggsave(paste0(dir, "tree_ages/tree_age_plots/final_tree_biomass.jpg"), plot=p2)
ggsave(paste0(dir, "tree_ages/tree_age_plots/mean_biomass_per_tree_spp.jpg"), plot=p6)
ggsave(paste0(dir, "tree_ages/tree_age_plots/max_biomass_per_tree_spp.jpg"), plot=p7)

#--------  
setwd(FIA_dir)
shrub_data_CA <- read.csv("CA_P2VEG_SUBPLOT_SPP.csv")
shrub_data_OR <- read.csv("OR_P2VEG_SUBPLOT_SPP.csv")
shrub_data <- rbind(shrub_data_CA, shrub_data_OR)
names(shrub_data)
rm(shrub_data_CA, shrub_data_OR)

shrubs <- final_spp %>% 
  subset(final_spp$type == "Shrub") %>%
  dplyr::select(PLANTS_ID, type, Longevity, LANDIS_NAME, Max_HT)
  
shrubs_in_plots <- subset(shrub_data, shrub_data$VEG_SPCD %in% shrubs$PLANTS_ID)

# Riccardi et al 2007 allometric biomass equation: Loading (Mg/ha) = (A+B) * X1 * %
names(shrubs_in_plots)
shrub_data_subset <- shrubs_in_plots[,c("PLT_CN","INVYR","VEG_SPCD","LAYER","COVER_PCT")]

# equation to calculate biomass and convert to g/m2
ShrubBiomass <- function (A, B, X1, N) {
  biomass_tons_acre <- (A+B) * X1 * 100 * N
  return(biomass_tons_acre) }

final_shrubs <- final_map_codes %>%
  dplyr::select(PLT_CN, Map_code) %>%
  left_join(shrub_data_subset) %>%
  left_join(all_spp_ages, by=c("VEG_SPCD"="PLANTS.ID", "PLT_CN"="PLT_CN", "INVYR"="INVYR", 
                               "COVER_PCT"="COVER_PCT", "LAYER" = "LAYER")) %>%
  left_join(shrubs, by=c(VEG_SPCD="PLANTS_ID")) %>%
  mutate(height = case_when(
    LAYER == 1 ~ 0.61, # this is in meters
    LAYER == 2 ~ 1.83,
    LAYER == 3 ~ 4.88,
    # height of plants in layer 4 based on literature of greatest height of species
    LAYER == 4 & LANDIS_NAME == "Frangula" ~ 6.10, 
    LAYER == 4 & LANDIS_NAME == "Rhododen" ~ 3.66,
    LAYER == 4 & LANDIS_NAME == "WhipMode" ~ 0.30)) %>%
  # calculate biomass in tons per acre
  mutate(biomass_tons_acre = case_when(
    VEG_SPCD == "RUPA" ~ ShrubBiomass(A=0.7885, B=0.2508, X1=COVER_PCT, N=9), 
    VEG_SPCD == "RULEL" | VEG_SPCD == "RUUR" ~ ShrubBiomass(A=14.4183, B=12.5609, X1=COVER_PCT, N=9),
    VEG_SPCD == "RHOC" | VEG_SPCD == "RHMA3" ~ ShrubBiomass(A=-7.22, B=17.307, X1=COVER_PCT, N=9),
    VEG_SPCD == "GASH" ~ ShrubBiomass(A=11.699, B=3.518, X1=COVER_PCT, N=9),
    LANDIS_NAME == "Ceanothu" ~ ShrubBiomass(A=0, B=0.108, X1=COVER_PCT, N=2000),
    VEG_SPCD == "ARCA5" | VEG_SPCD == "ARCO3" | VEG_SPCD == "ARVI4" | VEG_SPCD == "ARPA6" ~ ShrubBiomass(A=0, B=0.141, X1=COVER_PCT, N=2000),
    VEG_SPCD == "ARNE" ~ ShrubBiomass(A=0, B=0.00219, X1=COVER_PCT, N=2000),
    VEG_SPCD == "MANE2" ~ ShrubBiomass(A=14.218, B=1.984, X1=COVER_PCT, N=9),
    LANDIS_NAME == "Vacciniu" ~ ShrubBiomass(A=-7.22, B=17.307, X1=COVER_PCT, N=9),
    VEG_SPCD == "RHMA3" ~ ShrubBiomass(A=0, B=0.372, X1=COVER_PCT, N=2000),
    LANDIS_NAME == "Frangula" | LANDIS_NAME == "Garrya" | LANDIS_NAME == "Quercus"|
    LANDIS_NAME == "WhipMode" ~ ShrubBiomass(A=0, B=3000, X1=COVER_PCT/100, N=1) * height)) %>%
  mutate(biomass = biomass_tons_acre / 224.55) %>%   ## convert to biomass in gC/m2
  filter(!is.na(biomass)) %>%
  mutate(biomass = ifelse(biomass > 5000, 5000, biomass)) %>%
  dplyr::select(Map_code, LANDIS_NAME, Age, biomass) %>%
  distinct() %>%
  filter(Map_code > 0 & biomass > 0) %>% # need to remove map codes of zero and biomasses of zero
  mutate(Age = ifelse(is.na(Age), 30, Age))
  
colnames(final_shrubs) <- c("MapCode", "LANDIS_NAME", "CohortAge", "CohortBiomass")
head(final_shrubs, 500)

p3 <- ggplot(final_shrubs, aes(x=CohortAge, y=CohortBiomass)) +
  geom_bar(stat = 'identity', aes(fill= LANDIS_NAME)) +
  ggtitle("Biomass of shrubs per age cohort") +
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p3

shrub_bio <- 
  final_shrubs %>%
  group_by(LANDIS_NAME, CohortAge) %>%
  summarise_at(vars(CohortBiomass), list(meanAGB=mean, maxAGB=max))

p4 <- ggplot(shrub_bio, aes(x=CohortAge, y=meanAGB, color=LANDIS_NAME)) +
  geom_line() + 
  ggtitle("Mean biomass per species") + 
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p4

p5 <- ggplot(shrub_bio, aes(x=CohortAge, y=maxAGB, color=LANDIS_NAME)) +
  geom_line() + 
  ggtitle("Max biomass per species") + 
  xlab("Age") + ylab("Biomass (g/m^-2)") +
  theme_minimal()
p5

setwd(dir)
ggsave(paste0(dir,"tree_ages/tree_age_plots/final_shrub_biomass.jpg"), plot=p3)
ggsave(paste0(dir,"tree_ages/tree_age_plots/mean_biomass_per_shrub_spp.jpg"), plot=p4)
ggsave(paste0(dir,"tree_ages/tree_age_plots/max_biomass_per_shrub_spp.jpg"), plot=p5)

##--------- WRITE FINAL IC FILE
final_IC_file <- final_trees %>% rbind(final_shrubs) %>% 
  dplyr::select(MapCode, LANDIS_NAME, CohortAge, CohortBiomass) %>% arrange(MapCode)

write.csv(final_IC_file, paste0(dir, "IC_map/outputs/IC_mapcode_spp_age_biomass.csv"))


  

