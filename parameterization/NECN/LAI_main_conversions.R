#####################################################################################
#####################################################################################
###                                                                               ###
###  CALCULATING LAI FROM FIA, PLOTTING WITH SEEDLINGS, AND FITTING RELATIONSHIP  ###
###                                                                               ###
#####################################################################################
#####################################################################################

###  CALCULATING LAI FROM FIA, PLOTTING WITH SEEDLINGS, AND FITTING RELATIONSHIP  ###
#### Original written by Christopher Gerstle, altered for ReburnsAK by Shelby Weiss 
#### and later altered by Alison Deak 
#### Last altered: Aug 17 2021

#### load packages ####
library(dplyr)
library(plyr)

####  Read in Data  ####
options(scipen = 999) # necessary so that the full PLT_CNs will read in.
NECN_dir <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/NECN/functional_group_parameters"
fia_dir <- "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/LargeDataForLandis/FIA"
setwd(fia_dir)

tree_data_CA <- read.csv("CA_TREE.csv")
tree_data_OR <- read.csv("OR_TREE.csv")
tree_data <- rbind(tree_data_CA, tree_data_OR)

species_table <- read.csv("REF_SPECIES.csv") #Species reference table

setwd(NECN_dir)
SLA_lu <- read.csv("SLA_lookup.csv")
SLA_lu_subset <- subset(SLA_lu, SLA_lu$TraitID == c(3115, 3116, 3117))
SLA_lu_subset$OrigValueStr <- as.numeric(SLA_lu_subset$OrigValueStr)
SLA_bySpp <- aggregate(OrigValueStr ~ SpeciesName, data=SLA_lu_subset, FUN = "mean")

Spp <- read.csv("/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/FinalProducts/Final_landscape_species.csv")
SLA_withID <- SLA_bySpp %>% 
  left_join(Spp, by=c("SpeciesName" = "scientific_name")) %>%
  dplyr::select(SpeciesName, OrigValueStr, PLANTS_ID, SPCD, FunctionalGroup)
  
##################################
####  Calculating Leaf Areas  ####
##################################

#### Calculate Leaf Biomass from REF_SPECIES table ####
#### These calculations were based on a script written by M. Lucash
spp_table_short <- species_table[c("SPCD", "COMMON_NAME", "GENUS", "SPECIES", "JENKINS_TOTAL_B1", "JENKINS_TOTAL_B2", "JENKINS_FOLIAGE_RATIO_B1", "JENKINS_FOLIAGE_RATIO_B2")] #columns needed to calculate foliar ratio

TREE <- join(tree_data, spp_table_short) #join back with tree data

TREE <- TREE[!is.na(TREE$DIA),] #if dbh absent, remove

TREE$PLOT.YEAR <- paste(TREE$PLT_CN, TREE$INVYR, sep=".") #create a plot-inventory year variable

TREE <- TREE[TREE$STATUSCD == 1,] #only include live trees

# calculate foliage ratio, total AG biomass, and foliage biomass using formulas from the Appendix table M-3 in FIA manual (FIA Database Description and User Guide for Phase 2 (version: 8.0)). 
TREE$foliage_ratio <- exp(TREE$JENKINS_FOLIAGE_RATIO_B1 + TREE$JENKINS_FOLIAGE_RATIO_B2 / (TREE$DIA * 2.54)) # this calculation of foliage ratio is from FIA manual. Sections 9.5.42 and 9.5.43
TREE$total_AG_biomass_Jenkins <- exp(TREE$JENKINS_TOTAL_B1 + TREE$JENKINS_TOTAL_B2 * log(TREE$DIA*2.54)) * 2.2046 # biomass in pounds
TREE$foliage_biomass_Jenkins <- TREE$total_AG_biomass_Jenkins * TREE$foliage_ratio 
#TREE$foliage_biomass_g <- TREE$foliage_biomass_Jenkins * 453.592 # tree biomass in grams
TREE$foliage_biomass_kg <- TREE$foliage_biomass_Jenkins * 0.453592 # tree biomass in kilograms
TREE$foliage_biomass_kg_per_ac <- TREE$foliage_biomass_kg * TREE$TPA_UNADJ

## calculate hemisurface leaf area, or one-half total leaf area per unit ground area (HSLA; m2) by multiplying foliage mass (kg) by SLA (SLA's provided in SLA_lu table)
TREE <- left_join(TREE, SLA_withID, by="SPCD")
names(TREE)[222] <- "SLA"
TREE$HSLA <- TREE$SLA * TREE$foliage_biomass_kg # this gives the total leaf area per tree in m2
TREE$HSLA_per_ac <- TREE$HSLA * TREE$TPA_UNADJ # this gives foliage surface area (m2) per acre
TREE$treeLAI <- TREE$HSLA_per_ac * 0.000247105 # this gives the total leaf area per tree per acre

# aggregate by summing HSLA by PLOT.YEAR
TREE_short <- TREE[c('PLT_CN','INVYR',"SpeciesName","PLANTS_ID",'CARBON_AG','TPAGROW_UNADJ','DIA','HT', 
                     "JENKINS_FOLIAGE_RATIO_B1", "JENKINS_FOLIAGE_RATIO_B2","PLOT.YEAR", "FunctionalGroup",
                     "foliage_ratio","total_AG_biomass_Jenkins","foliage_biomass_Jenkins", "SPCD",
                     "foliage_biomass_kg","foliage_biomass_kg_per_ac","SpeciesName","SLA","treeLAI")]

each_TREE_short_agg <- aggregate(TREE_short$treeLAI ~ TREE_short$INVYR + TREE_short$PLT_CN + TREE_short$SpeciesName, FUN=sum) # gives total LAI for one plot
names(TREE_short_agg) <- c("INYR", "PLT_CN", "PLOT.LAI")

plot_TREE_short_agg <- aggregate(TREE_short$treeLAI ~ TREE_short$SpeciesName + TREE_short$INVYR + TREE_short$PLT_CN, FUN=sum) # gives total LAI for one plot

# write.csv(TREE_short, "foliage_ratios_and_LAI.csv")
write.csv(each_TREE_short_agg, "each_tree_year_LAI.csv")
write.csv(plot_TREE_short_agg, "each_plot_year_LAI.csv")

# foliage_ratios <- aggregate(TREE_short$foliage_ratio ~ TREE_short$FunctionalGroup, FUN=mean)
