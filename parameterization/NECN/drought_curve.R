library(raster)
library(tidyverse)

setwd("/Users/alisondeak/Desktop/Landis-Siskiyous/NECN/functional_group_parameters/Drought and Temp Curves")

params <- read.csv("DroughtCurve.csv")

field_cap <- raster('/Users/alisondeak/Desktop/Landis-Siskiyous/soils/final_products/field_capacity.tif')
field_cap[field_cap==0] <- NA
afiel_1 <- field_cap %>% as.data.frame() %>% as.tibble() %>% summarise(mean=mean(field_capacity, na.rm=T))
print(afiel_1) #0.208

wilt_point <- raster("/Users/alisondeak/Desktop/Landis-Siskiyous/soils/final_products/wilting_point.tif")
wilt_point[wilt_point==0] <- NA
afiel_2 <- wilt_point %>% as.data.frame %>% as.tibble() %>% summarise(mean=mean(wilting_point, na.rm=T))
print(afiel_2) #0.125

# moisture curve values- conifers
pprpts_1 <- 0 
pprpts_2 <- 0.7  
pprpts_3 <- 0.5 

moisture_availability = data.frame(as.numeric(seq(0, 1, 0.01)))
colnames(moisture_availability) <- "moisture_availability"

wc <- afiel_1 - afiel_2
intercept <- pprpts_1 + (pprpts_2 * wc)

slope_value <- 1.0 / (pprpts_3 - intercept)
slope <- data.frame(1:nrow(moisture_availability)) 
colnames(slope) <- 'slope'
slope$slope <- as.numeric(rep(slope_value))
head(slope)

pprpts_3 = data.frame(1:nrow(moisture_availability))
colnames(pprpts_3) <- 'pprpts_3'                     
pprpts_3$pprpts_3 <- 0.8
head(pprpts_3)

relative_production <- 1.0 + slope * (moisture_availability - pprpts_3)
colnames(relative_production) <- "RP"
head(relative_production)

Conifer_rp <- relative_production %>% mutate(RP=ifelse(RP > 1, 1, RP)) %>% mutate(RP=ifelse(RP<0.1, 0.1, RP)) %>% as.data.frame()
Conifer_values <- cbind(moisture_availability, Conifer_rp)
head(Conifer_values)
plot(Conifer_values, main="Conifer moisture curve")

##--------------

# moisture curve values- hardwoods
pprpts_1 <- 0 
pprpts_2 <- 0.8  
pprpts_3 <- 0.75 

moisture_availability = data.frame(as.numeric(seq(0, 1, 0.01)))
colnames(moisture_availability) <- "moisture_availability"

wc <- afiel_1 - afiel_2
intercept <- pprpts_1 + (pprpts_2 * wc)

slope_value <- 1.0 / (pprpts_3 - intercept)
slope <- data.frame(1:nrow(moisture_availability)) 
colnames(slope) <- 'slope'
slope$slope <- as.numeric(rep(slope_value))
head(slope)

pprpts_3 = data.frame(1:nrow(moisture_availability))
colnames(pprpts_3) <- 'pprpts_3'                     
pprpts_3$pprpts_3 <- 0.8
head(pprpts_3)

relative_production <- 1.0 + slope * (moisture_availability - pprpts_3)
colnames(relative_production) <- "RP"
head(relative_production)

HW_rp <- relative_production %>% mutate(RP=ifelse(RP > 1, 1, RP)) %>% mutate(RP=ifelse(RP<0.1, 0.1, RP)) %>% as.data.frame()
hardwood_values <- cbind(moisture_availability, HW_rp)
head(hardwood_values)
plot(hardwood_values, main="Hardwood moisture curve")

##-------

# moisture curve values- shrubs_xeric
pprpts_1 <- 0 
pprpts_2 <- 0.5  
pprpts_3 <- 0.8 

moisture_availability = data.frame(as.numeric(seq(0, 1, 0.01)))
colnames(moisture_availability) <- "moisture_availability"

wc <- afiel_1 - afiel_2
intercept <- pprpts_1 + (pprpts_2 * wc)

slope_value <- 1.0 / (pprpts_3 - intercept)
slope <- data.frame(1:nrow(moisture_availability)) 
colnames(slope) <- 'slope'
slope$slope <- as.numeric(rep(slope_value))
head(slope)

pprpts_3 = data.frame(1:nrow(moisture_availability))
colnames(pprpts_3) <- 'pprpts_3'                     
pprpts_3$pprpts_3 <- 0.8
head(pprpts_3)

relative_production <- 1.0 + slope * (moisture_availability - pprpts_3)
colnames(relative_production) <- "RP"
head(relative_production)

shrubs_xeric_rp <- relative_production %>% mutate(RP=ifelse(RP > 1, 1, RP)) %>% mutate(RP=ifelse(RP<0.1, 0.1, RP)) %>% as.data.frame()
shrubs_xeric_values <- cbind(moisture_availability, shrubs_xeric_rp)
head(shrubs_xeric_values)

      ##-------

# moisture curve values- shrubs_mesic
pprpts_1 <- 0 
pprpts_2 <- 0.7 
pprpts_3 <- 0.7 

moisture_availability = data.frame(as.numeric(seq(0, 1, 0.01)))
colnames(moisture_availability) <- "moisture_availability"

wc <- afiel_1 - afiel_2
intercept <- pprpts_1 + (pprpts_2 * wc)

slope_value <- 1.0 / (pprpts_3 - intercept)
slope <- data.frame(1:nrow(moisture_availability)) 
colnames(slope) <- 'slope'
slope$slope <- as.numeric(rep(slope_value))
head(slope)

pprpts_3 = data.frame(1:nrow(moisture_availability))
colnames(pprpts_3) <- 'pprpts_3'                     
pprpts_3$pprpts_3 <- 0.8
head(pprpts_3)

relative_production <- 1.0 + slope * (moisture_availability - pprpts_3)
colnames(relative_production) <- "RP"
head(relative_production)

shrubs_mesic_rp <- relative_production %>% mutate(RP=ifelse(RP > 1, 1, RP)) %>% mutate(RP=ifelse(RP<0.1, 0.1, RP)) %>% as.data.frame()
shrubs_mesic_values <- cbind(moisture_availability, shrubs_mesic_rp)
head(shrubs_mesic_values)

plot(hardwood_values, type="l", col="red")
lines(Conifer_values, col="green")
lines(shrubs_xeric_values, col="blue")
lines(shrubs_mesic_values, col="purple")
legend("topleft", legend=c("conifers", "xeric shrubs", "hardwoods", "mesic shrubs"), 
       col=c("green", "blue", "red", "purple"), lty=1)


