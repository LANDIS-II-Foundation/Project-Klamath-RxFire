library(tidyverse)

# F00018 - Genus
# F00019 - Species
# F00253 - Root C content
# F00261 - Root N content
# F00358 - Root lignin content
# F00413 - Root C/N ratio
# F01270 - Aboveground biomass per ground area (g)
 
FRED <- read.csv("/Users/alisondeak/Downloads/FRED3_Entire_Database_2021.csv")
FRED <- select(FRED, c("F00018","F00019","F00055","F00253","F00261","F00358","F00413","F01270"))
arcto <- filter(FRED, F00018=="Arctostaphylos")
ceano <- filter(FRED, F00018=="Ceanothus")
frang <- filter(FRED, F00018=="Frangula")
Garry <- filter(FRED, F00018=="Garrya") # no return
Gault <- filter(FRED, F00018=="Gaultheria")
Mahon <- filter(FRED, F00018=="Mahonia")
Querc <- filter(FRED, F00018=="Quercus")
Rhodo <- filter(FRED, F00018=="Rhododendron")
Rubus <- filter(FRED, F00018=="Rubus")
Vacci_FR <- filter(FRED, F00018=="Vaccinium")
Whipp <- filter(FRED, F00018=="Whipplea")

Vacci_FR$F00413 <- as.numeric(Vacci_FR$F00413)
Vacci_FR_CN <- mean(Vacci_FR$F00413, na.rm = TRUE)

Rhodo$F00413 <- as.numeric(Rhodo$F00413)
Rhodo_FR_CN <- mean(Rhodo$F00413, na.rm=TRUE)

Rubus$F00413 <- as.numeric(Rubus$F00413)
Rubus_FR_CN <- mean(Rubus$F00413, na.rm=TRUE)

##------------------------------
GRooTFullVersion <- read.csv("/Users/alisondeak/Downloads/GRooTFullVersion.csv", header=T, na.strings=c("", "NA"))
str(GRooTFullVersion)
unique(GRooTFullVersion$traitName)

GRooTFullVersion <- select(GRooTFullVersion, c("genus", "species", "traitName","traitValue"))
GRooT_traits <- filter(GRooTFullVersion, traitName=="Root_C_N_ratio" | traitName=="Root_lignin_concentration" )

Arcto_GRooT <- filter(GRooT_traits, genus=="Arctostaphylos") # no return
ceano_GRoot <- filter(GRooT_traits, genus=="Ceanothus") 
frang_GRooT <- filter(GRooT_traits, genus=="Frangula") # no return
Garry_GRooT <- filter(GRooT_traits, genus=="Garrya") # no return
Gault_GRooT <- filter(GRooT_traits, genus=="Gaultheria") # no return
Mahon_GRooT <- filter(GRooT_traits, genus=="Mahonia") # no return
Querc_GRooT <- filter(GRooT_traits, genus=="Quercus") 
Rhodo_GRooT <- filter(GRooT_traits, genus=="Rhododendron") 
Rubus_GRooT <- filter(GRooT_traits, genus=="Rubus") 
Vacci_GRooT <- filter(GRooT_traits, genus=="Vaccinium") 
Whipp_GRooT <- filter(GRooT_traits, genus=="Whipplea") # no return
