library(tidyverse)

wd <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/NECN/functional_group_parameters/temp_curve"
setwd(wd)


#starting coefficients
ppdf_1<- 14
ppdf_2<- 35
ppdf_3<- 4
ppdf_4<- 4

#Temp values, go from 0 to 50oC
temp<-sort(runif(1000, -40, 100))

#Calculate fraction here
fraction<-(ppdf_2 - temp)/(ppdf_2- ppdf_1)

#This is the curve based on the coefficients listed above
Landis_Relative_production<-ifelse(fraction>0, (exp(ppdf_3/ppdf_4*(1-(fraction ^ ppdf_4)))*(fraction ^ ppdf_3)),0)
plot(temp, Landis_Relative_production, type="l", lwd=3, ylab="Maximum Relative GPP", xlab="Soil temp (C) at 10cm")

##Observed_Relative_production is being substited for space, so frequency will be used in place
Observed_Relative_production <- PSME_temps$temp_pts_merge
temp <- PSME_temps$known_temp_pct
lines(temp, Observed_Relative_production, col="blue", type="p")  #Plotted it with blue circles so you could compare it to the curve above.



PSME_temps <- read.csv("PSME_temp_freq_table.csv")
summary(PSME_temps)


