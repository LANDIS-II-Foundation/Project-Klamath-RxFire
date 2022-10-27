## ---------------------------------------------------
# This script is used to calculate tree ages and shrub ages within each plt_cn 
## --------------

library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(rgeos)

dir <- "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/"
removeTmpFiles(0.5) # good practice - raster files fill up temp folder FAST
options(scipen=999)

### -------------------------------------
# Create loops inputting known tree age and estimating unknown tree ages of from height
### -------------------------------------

# merge tree conditions and coefficients data frames

Merged_Tree_Con <- read.csv(paste0(dir, "IC_map/inputs/merged_tree_conditions.csv"))
Merged_Tree_Con <- subset(Merged_Tree_Con, select = -PLANTS.ID ) # we don't need this, in other df
CA_Coefficients <- read.csv(paste0(dir, "tree_ages/InlandCaliforniaFSV_Coefficients.csv"))
TreeCoefficients_CA <- left_join(Merged_Tree_Con, CA_Coefficients, "SPCD")
TreeCoefficients_CA #check data frame

rm(Merged_Tree_Con, CA_Coefficients)

### --------------------------------------

# PACIFIC MADRONE (ARME), GIANT CHINQUAPIN (CHCH7), and TANOAK (LIDE3)
# Age solved for using Inland California and Southern Cascades FVS equation 4.7.2.5 large tree 
# height growth for species indexes: 8,9,10. Species Codes: 361, 431, and 631

# solve for Age
# library(ryacas)
# yac_str("OldSolve((SI / (b0 + (b1 / A))) * 0.80== H, A)") ### returns "b1/((0.80*SI)/H-b0)"

# Create function to find age based on height
Spp8910_AgeFx <- function (SI, b0, b1, H) {
  Age = b1 / ((0.80 * SI) / H - b0)
  return(Age) }

# subset species 8,9,10
SppIndex_8910 <- filter(TreeCoefficients_CA, SPCD==361 | SPCD==431 | SPCD==631)

SppIndex_8910$TreeAge <- 0
SppIndex_8910 <- SppIndex_8910 %>%  
  mutate(TreeAge = ifelse(BHAGE > 0, BHAGE, Spp8910_AgeFx(SI=SICOND_mean, b0=b0, b1=b1, H=HT))) %>%
  mutate(TreeAge = ifelse(TreeAge > Longevity, Longevity, ifelse(TreeAge < 0, STDAGE_mean, TreeAge))) 

# function to round tree ages up to a multiple of ten - written by Melissa Lucash
roundUp <- function(x) 10*ceiling(x/10) 

SppIndex_8910$TreeAge <-  roundUp(SppIndex_8910$TreeAge)

plot(SppIndex_8910$HT, SppIndex_8910$TreeAge)
boxplot(SppIndex_8910$TreeAge ~ SppIndex_8910$PLANTS.ID)

# Plot and save predicted versus known age to test efficacy
SppIndex_8910_withAge <- filter(SppIndex_8910, BHAGE > 0)
SppIndex_8910_withAge <- SppIndex_8910_withAge %>%  
  mutate(TreeAge =  Spp8910_AgeFx(SI=SICOND_mean, b0=b0, b1=b1, H=HT))

# plots_wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/tree_ages/tree_age_plots"
# setwd(plots_wd)
# 
# jpeg("CA_FVS_spp8910_ages.jpg", width = 350, height = 350)
# par(mar = c(5, 5, 5, 5))
# plot(SppIndex_8910_withAge$BHAGE, SppIndex_8910_withAge$TreeAge, xlim=c(0,500), ylim=c(0,500), 
#      main = "Predicted versus known tree ages\n for species indices 8, 9, and 10 based on \nInland CA and S Cascades FVS equations", 
#      xlab = "Known age", ylab = "Predicted Age", pch = 16)
# abline(coef=c(0,1), col="red")
# dev.off()

rm(Spp8910_AgeFx, SppIndex_8910_withAge)

## ---------------------------------------------------------

# INCENSE CEDAR (CADE27), SUGAR PINE (PILA), WESTERN WHITE PINE (PIMO3), JEFFREY PINE (PIJE), and DOUGLAS FIR (PSME)
# Age solved for using Inland California and Southern Cascades FVS equation 4.7.2.1 large 
# tree height growth for species index 3 and 4. 
# Species Codes: 81, 117, 116, 119, and 202

### Solve for age and term values 
### Equation 4.7.2.1
# H = (((SI - 4.5) * TOPTRM / BOTTRM) + 4.5) * 1.05
# TOPTRM = 1 - exp(-exp (b0 + (b1 * ln(SI - 4.5)) + (b2 * ln(A)))))
# BOTTRM = 1 - exp(-exp (b0 + (b1 * ln(SI - 4.5)) + (b2 * ln(50))))
# yac_str("Solve((1 - exp(-exp (b0 + (b1 * log(SI - 4.5)) + (b2 * log(A))))), A)")
## too large to solve --- solved by hand to get Age = exp(((log(-log(abs(1-TopTerm)))-b0)-(b1*log(SI - 4.5))) / b2)

# yac_str("OldSolve((((SI - 4.5) * TopTrm / BtmTrm) + 4.5) * 1.05 == H, TopTrm)")
# Returns:"((H/1.05-4.5)*BtmTrm)/(SI-4.5)"

# Create function to find age based on height
Spp34_BtmTermFx <- function(b0, b1, b2, SI) {
  BtmTerm = 1 - exp(-exp (b0 + (b1 * log(SI - 4.5)) + (b2 * log(50))))
  return(BtmTerm)}

Spp34_TopTermFx <- function(SI, BtmTerm, H) {
  TopTerm = ((H/1.05-4.5)*BtmTerm)/(SI-4.5)
  return(TopTerm) }

Spp34_AgeFx <- function(TopTerm, b0, b1, SI, b2) {
  Age = exp(((log(-log(abs(1-TopTerm)))-b0)-(b1*log(SI - 4.5))) / b2)
  return(Age)}

# subset species 3 and 4
SppIndex_34 <- filter(TreeCoefficients_CA, SPCD==81 | SPCD==117 | SPCD==202 | SPCD==116 | SPCD==119)

# Find the BtmTerm values and append to SppIndex_34
SppIndex_34$BtmTerm = 0
SppIndex_34$TopTerm = 0 
SppIndex_34 <- SppIndex_34 %>% 
  mutate(BtmTerm = Spp34_BtmTermFx(b0=b0, b1=b1, b2=b2, SI=SICOND_mean)) %>%
  mutate(TopTerm = Spp34_TopTermFx(SI=SICOND_mean, BtmTerm=BtmTerm, H=HT)) %>%
  mutate(TreeAge = ifelse(BHAGE > 0, BHAGE, Spp34_AgeFx(TopTerm=TopTerm, b0=b0, b1=b1, SI=SICOND_mean, b2=b2))) %>%
  mutate(TreeAge = ifelse(TreeAge > Longevity, Longevity, ifelse(TreeAge < 0, StandAge, TreeAge))) 

SppIndex_34$TreeAge <-  roundUp(SppIndex_34$TreeAge)

plot(SppIndex_34$HT, SppIndex_34$TreeAge)
boxplot(SppIndex_34$TreeAge ~ SppIndex_34$PLANTS.ID)

# check for accuracy
SppIndex_34_withAge <- filter(SppIndex_34, BHAGE > 0)
SppIndex_34_withAge <- SppIndex_34_withAge %>%  
  mutate(TreeAge = Spp34_AgeFx(TopTerm=TopTerm, b0=b0, b1=b1, SI=SICOND_mean, b2=b2))

# plot(SppIndex_34_withAge$BHAGE, SppIndex_34_withAge$TreeAge, xlim=c(0,500), ylim=c(0,500), 
#      main = "Predicted versus known tree ages for species indices 3 and 4\n based on Inland CA and S Cascades FVS equations", 
#      xlab = "Known age", ylab = "Predicted Age", pch = 16)
# abline(coef=c(0,1), col="red")
# 
# jpeg("CA_FVS_spp34_ages.jpg", width = 350, height = 350)
# plot(SppIndex_34_withAge$BHAGE, SppIndex_34_withAge$TreeAge, xlim=c(0,500), ylim=c(0,500), 
#      main = "Predicted versus known tree ages for species \nindices 3 and 4 based on Inland CA\n and S Cascades FVS equations", 
#      xlab = "Known age", ylab = "Predicted Age", pch = 16)
# abline(coef=c(0,1), col="red")
# dev.off()
# 
# PSME <- filter(SppIndex_34, BHAGE > 0 & SPCD == 202)
# jpeg("CA_FVS_PSME_age.jpg", width = 350, height= 350)
# boxplot(PSME$BHAGE, main="PSME predicted ages")
# dev.off()

# remove term columns no longer needed and clean up environment
SppIndex_34<- subset(SppIndex_34, select= -c(TopTerm, BtmTerm))
rm(Spp34_AgeFx, Spp34_BtmTermFx, SppIndex_34_withAge, Spp34_TopTermFx)

## ----------------------------------------------------------------------------
#### REMOVED THIS SPECIES FROM LIST!
# KNOBCONE PINE (PIAT)
# Age solved for using Inland California and Southern Cascades FVS equation 4.7.2.3 large tree 
# height growth for species index 3. Species Code: 103.

# solve for Age H = SI * [b0 + (b1* A) + (b2* A^2)] * 1.10
# yac_str("OldSolve(SI * (b0 + (b1* A) + (b2* A^2)) * 1.10 == H, A)") ### returns (Sqrt(1.21*(SI*b1)^2-4.4*SI*b2*(1.10*SI*b0-H))-1.10*SI*b1)/(2.2*SI*b2)

#create function to find age based on height FIX
# Spp6_AgeFx <- function (SI, b0, b1, b2, H) {
#   Age = (sqrt(1.21*(SI*b1)^2-4.4*SI*b2*(1.10*SI*b0-H))-1.10*SI*b1)/(2.2*SI*b2)
#   return(Age) }
# 
# # subset species 8,9,10
# SppIndex_6 <- filter(TreeCoefficients_CA, SPCD==103)
# 
# SppIndex_6$TreeAge <- 0
# SppIndex_6 <- SppIndex_6 %>%  
#   mutate(TreeAge = ifelse(BHAGE > 0, BHAGE, Spp6_AgeFx(SI=SICOND_mean, b0=b0, b1=b1, b2=b2, H=HT))) %>%
#   mutate(TreeAge = ifelse(TreeAge > Longevity, Longevity, ifelse(TreeAge < 0, StandAge, TreeAge))) 
# 
# SppIndex_6$TreeAge <-  roundUp(SppIndex_6$TreeAge)
# 
# plot(SppIndex_6$HT, SppIndex_6$TreeAge)
# boxplot(SppIndex_6$TreeAge ~ SppIndex_6$PLANTS.ID)
# 
# # Plot and save predicted versus known age to test efficacy
# SppIndex_6_withAge <- filter(SppIndex_6, BHAGE > 0)
# SppIndex_6_withAge <- SppIndex_6_withAge %>%  
#   mutate(TreeAge =  Spp6_AgeFx(SI=SICOND_mean, b0=b0, b1=b1, b2=b2, H=HT))
# 
# jpeg("CA_FVS_spp6_ages.jpg", width = 350, height = 350)
# plot(SppIndex_6_withAge$BHAGE, SppIndex_6_withAge$TreeAge, xlim=c(0,500), ylim=c(0,500), 
#      main = "Predicted versus known tree ages for \nspecies index 6  based on Inland CA and\n S Cascades FVS equations", 
#      xlab = "Known age", ylab = "Predicted Age", pch = 16)
# abline(coef=c(0,1), col="red")
# dev.off()
# 
# rm(Spp6_AgeFx, SppIndex_6_withAge)

## -------------------------------------------------------------
# Bind data frames with tree species and create plots
# CA_Final_TreeAges <- rbind(SppIndex_34, SppIndex_8910, SppIndex_6)
CA_Final_TreeAges <- rbind(SppIndex_34, SppIndex_8910)

# Create box plot showing age for each species
# library(ggplot2)
# dev.new()
# 
ggplot(CA_Final_TreeAges) +
  geom_line(data = CA_Final_TreeAges, aes(x = PLANTS.ID, y = Longevity, group = 1, color="Longevity")) +
  geom_boxplot(data = CA_Final_TreeAges, aes(PLANTS.ID, TreeAge, fill=as.factor(Species_Index))) +
  labs(title = "Tree Ages based on Inland California\nand Southern Cascades Site Index Equations",
       x = "Species ID",
       y = "Age",
       fill = c("Species Index"),
       color = "Legend") +
  theme_light()
ggsave(paste0(dir, "tree_ages/tree_age_plots/tree_ages.pdf"), width = 8, height = 4)
# 
# dev.new()
# hist(CA_Final_TreeAges$TreeAge, breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750))
# hist(CA_Final_TreeAges$StandAge, breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750))

# write_csv(CA_Final_TreeAges,"FinalProducts/final_tree_ages.csv")

## -----------------------------------------------------------------------------------

# input and bind FIA P2VEG table
CA_P2VEG <- read.csv("/Volumes/Alison.Deak_440-223-4897/MS Research/LargeDataForLandis/FIA/OR_P2VEG_SUBPLOT_SPP.csv")
OR_P2VEG <- read.csv("/Volumes/Alison.Deak_440-223-4897/MS Research/LargeDataForLandis/FIA/CA_P2VEG_SUBPLOT_SPP.csv")
P2VEG <- rbind(CA_P2VEG, OR_P2VEG)

# Remove plots surveyed prior to 2010 and keep only species found in the Shrub.species.10pct df
P2VEG_post2010 <- filter(P2VEG, INVYR > 2010)

# Input CSV with species chosen to be included in landscape. This CSV includes PLANTS.ID, 
# scientific and common name, percentage of landscape, longevity, and functional 
# group but we will subset to shrubs and their PLANTS.ID, longevity, 
# and functional group (FG_Short)
ShrubsCSV <- read.csv(paste0(dir, "landscape/Final_landscape_species.csv"))
Shrub_types <- filter(ShrubsCSV, type =="Shrub") 
Shrubs <- Shrub_types[,c("PLANTS_ID", "LANDIS_NAME", "Longevity")]

# filter P2VEG table for species in Shrubs table
SelectShrubs <- P2VEG_post2010[P2VEG_post2010$VEG_SPCD %in% Shrubs$PLANTS_ID,]

unique(SelectShrubs$VEG_SPCD) # check
unique(Shrubs$PLANTS_ID)

# join with site conditions dataframe created in FIA_tree_data_transformation script (stand age)
# this input only has forested plots inventoried after 2010

COND_df <- read.csv(paste0(dir, "IC_map/inputs/site_conditions.csv"))

P2VEG_cond <- left_join(SelectShrubs, COND_df, by="PLT_CN")

# replace NAs with zeroes and round stand age up to multiple of ten
P2VEG_cond$STDAGE_mean[is.na(P2VEG_cond$STDAGE_mean)] <- 0 # transforms NAs to zeroes

roundUp <- function(x) 10*ceiling(x/10) 
P2VEG_cond$STDAGE_mean <- roundUp(P2VEG_cond$STDAGE_mean)
unique(P2VEG_cond$STDAGE_mean)

# join shrub longevity data to P2VEG table
P2VEG_longevity <- left_join(P2VEG_cond, Shrubs, by=c("VEG_SPCD" = "PLANTS_ID"))

# create new column to insert shrub age and find age
# If stand age < shrub longevity then Age = stand age
# If stand age > shrub longevity then Age = shrub longevity
# if stand age = 0 then Age =10

P2VEG_longevity$Age <- 0

ShrubAges <- P2VEG_longevity %>%  
  mutate(Age = ifelse(STDAGE_mean < Longevity, STDAGE_mean, Longevity)) %>%
  mutate(Age = ifelse(Age == 0, 10, Age))

rm(CA_P2VEG, COND_df, OR_P2VEG, P2VEG, P2VEG_post2010, Shrubs, P2VEG_cond, P2VEG_longevity, 
   roundUp, SelectShrubs, ShrubsCSV)

### ---------------------------------
# add functional group column with PLANTS.ID -- trees do not have functional group
# prep and bind final tree ages and shrub ages to file
CA_Final_TreeAges$LANDIS_NAME <- CA_Final_TreeAges$PLANTS.ID 
CA_Final_TreeAges$LAYER <- 0
CA_Final_TreeAges$COVER_PCT <- 0
FinalTreeAges <- dplyr::select(CA_Final_TreeAges, 
                               c(PLT_CN, INVYR, PLANTS.ID, Longevity, TreeAge, LANDIS_NAME, 
                                 LAYER, COVER_PCT, TPA_UNADJ, CARBON_AG))

# add TPAGROW_UNADJ and CARBON_AG columns to final shrub ages
ShrubAges$TPA_UNADJ <- 0
ShrubAges$CARBON_AG <- 0
FinalShrubAges <- dplyr::select(ShrubAges, 
                                c(PLT_CN, INVYR, VEG_SPCD, Longevity, Age, LANDIS_NAME, 
                                  LAYER, COVER_PCT, TPA_UNADJ, CARBON_AG))

# colnames don't match -- need to fix
colnames(FinalTreeAges) <- c("PLT_CN", "INVYR", "PLANTS.ID", "Longevity", "Age", "LANDIS_NAME",
                             "LAYER", "COVER_PCT", "TPA_UNADJ", "CARBON_AG") 

# change FinalShrubAges column names to match FinalTreeAges column names
names(FinalShrubAges)[names(FinalShrubAges) == "VEG_SPCD"] <- "PLANTS.ID"

# bind dataframs and save to CSV file
FinalSpeciesAges <- rbind(FinalTreeAges, FinalShrubAges)
# write.csv(FinalSpeciesAges, paste0(dir, "tree_ages/FIA/Final_FIASpeciesData_Age.csv"))

ggplot(FinalShrubAges) +
  geom_line(data = FinalShrubAges, aes(x = LANDIS_NAME, y = Longevity, group = 1, color="Longevity")) +
  geom_boxplot(data = FinalShrubAges, aes(LANDIS_NAME, Age, fill=as.factor(LANDIS_NAME))) +
  labs(title = "Shrub Functional Group Ages",
       x = "Functional Group",
       y = "Age",
       fill = c("LANDIS_NAME"),
       color = "Legend") +
  theme_light() 
ggsave(paste0(dir, "tree_ages/tree_age_plots/shrub_ages.pdf"), width = 8, height = 4)

