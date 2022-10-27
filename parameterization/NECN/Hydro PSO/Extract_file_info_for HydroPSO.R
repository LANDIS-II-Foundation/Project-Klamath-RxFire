# ---------------------------------------------------------------------------------
# Extract_file_info.R
# Kelsey Ruckert, klr324@psu.edu and Melissa Lucash, lucash@pdx.edu
# Code originally written on Oct. 19 2017 by Kelsey
# Code significantly modified by Melissa in Nov 2017 
# Original code did not work properly and had to be adjusted to loop through parameter values and not names.
# 
# This script extracts parameter values, row location, decimal points, character start and end 
# location for use with the HydroPSO package of R to calibrate biomass, soil C pools and NEE in a single cell.

# Copyright 2017 by the Authors

# THIS CODE IS PROVIDED AS-IS WITH NO WARRANTY (NEITHER EXPLICIT NOT IMPLICIT).
# We SHARE THIS CODE IN HOPES THAT IT IS USEFUL, BUT I AM NOT LIABLE FOR THE 
# BEHAVIOR OF THIS CODE IN YOUR OWN APPLICATION.  YOU ARE FREE TO SHARE THIS 
# CODE SO LONG AS THE AUTHOR(S) AND VERSION HISTORY REMAIN INTACT, see 
# <http://www.gnu.org/licenses/>.
#
# The code triggers an error in cline[i] but still works properly. 
# This code works only for unique values.
# ---------------------------------------------------------------------------------
library(stringr)
options(scipen=9999)

#Here's the parameter file
hydro_dir<-("C:/Users/13146/Documents/ReburnsAK/Sensitity_Analysis_and_Calibration_Automation/HydroPSO_Calibration_AK/")
setwd(hydro_dir)

param_ranges<-read.csv("PSO.in/ParamInputs.csv", header=TRUE)

#Here's the NECN input data
dgs_file <- readLines("DGS_Succession_AKinput_TEST.txt")

# Function to extract decimal places:
# https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

# Create empty vectors
cline <- 
val <- 
dec <-
rownum <- rep(NA, nrow(param_ranges))
char_loc <- mat.or.vec(nrow(param_ranges), 2)

# Loop through all the parameter and extract info
for(i in 1:nrow(param_ranges)){
  # Extract line with the specified parameter
  cline[i] <- grep(param_ranges[i,3], dgs_file, value=TRUE)
  rownum[i] <- which(dgs_file == cline[i])
  # Extract parameter value
  val[i] <- param_ranges[i,3]
  dec[i] <- decimalplaces(val[i])
  # Extract row number
  
  # Extract character start and end location within the row
  char_loc[i, ] <- str_locate(pattern = as.character(val[i]), cline[i])
  
}

row_matrix<-cbind(rownum, char_loc, dec)
colnames(row_matrix)<-c("row number", "start column", "end column", "decimal places")
row_matrix
write.csv (row_matrix, "Extract_file_output.csv")
