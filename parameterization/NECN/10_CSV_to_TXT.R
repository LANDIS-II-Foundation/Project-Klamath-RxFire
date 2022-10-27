#####This code takes the outputs of the tree age script and imputation script and creates maps
#####for the IC file as well as biomass and age comparisons.

##### This uses Alec Kretchun's method of IC.txt file writing with some tweaking
#####Load libraries

library(dplyr)
options(scipen=999) # scipen removes scientific notation from FIA data

# Set working directory
dir <- ("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous//IC_map/")

IC_output_file <- paste0(dir, "initial_communities.txt")
IC_file <- read.csv(paste0(dir, "outputs/IC_mapcode_spp_age_biomass.csv"))
head(IC_file)

#### This writes an IC text file with for LANDIS-II
uniqueMapCodes<-unique(IC_file$MapCode)
print(uniqueMapCodes)

output <- matrix()
# l=2
for (l in uniqueMapCodes){ # Loop for individual mapcode
  ic.mapcode.select <- subset(IC_file, IC_file$MapCode==l)
  IC.mapcode <- noquote(paste("MapCode ", l)) 
  cat(NULL, file=IC_output_file, sep="\n", append=TRUE) #write blank space
  cat(IC.mapcode, file=IC_output_file, "\n", append=TRUE) #write  mapcode
  for (k in unique(ic.mapcode.select$LANDIS_NAME)){##Loop for individual species
    
    ic.spp.select <- subset(ic.mapcode.select, ic.mapcode.select$LANDIS_NAME==k)
    ic.cohort.biomass.list <-NULL
    ic.spp.list<-NULL
    for (m in unique(ic.spp.select$CohortAge)){
      ic.cohort.select <- subset(ic.spp.select, ic.spp.select$CohortAge==m)
      ic.cohort.biomass.gm <- as.numeric(sum(ic.cohort.select$CohortBiomass)) #converting to numeric
      ic.cohort.biomass <- paste(m, " (", round(ic.cohort.biomass.gm), ")", sep="")
      ic.cohort.biomass.list <- c(ic.cohort.biomass.list, ic.cohort.biomass)
      ic.spp.list <- c(k, ic.cohort.biomass.list)
    }
    
    print(ic.spp.list)
    cat(ic.spp.list, file=IC_output_file, "\n", append=TRUE)
  }
}
print("done")
rm(l)



