# Script to find tree ages for tree in Fire Tree Mortality database 
# To be used in Mortality script

library(tidyverse)
options(scipen=999) #This removes scientific notation in printing. Plot CNs are very long
dir <-  "/Users/alisondeak/Desktop/Landis-Siskiyous/"
w.dir <- "/Volumes/Alison.Deak_440-223-4897/LargeDataForLandis/SCRPPLE/"

CA_Coefficients <- read.csv(paste0(dir, "tree_ages/InlandCaliforniaFSV_Coefficients.csv"))
FTM_coef <- read.csv(paste0(w.dir, "fire_tree_mortality/Data/Species_BarkThickness.csv"))
FTM_tree <- read.csv(paste0(w.dir, "fire_tree_mortality/Data/FTM_trees.csv"))

TreeCoef <- FTM_tree %>%
  left_join(CA_Coefficients, by=c("Species"="PLANTS.ID")) %>%
  left_join(FTM_coef, by=c("Species"="PLANTS_Species_Code")) %>%
  mutate(HT = HT_m * 3.28084) # convert height from meters to feet

TreeCoef$SICOND_mean <- rep(74) # based on mean of all FIA plots in LANDIS landscape
TreeCoef #check data frame

### --------------------------------------

# PACIFIC MADRONE (ARME), GIANT CHINQUAPIN (CHCH7), and TANOAK (LIDE3)
# Age solved for using Inland California and Southern Cascades FVS equation 4.7.2.5 large tree 
# height growth for species indexes: 8,9,10. Species Codes: 361, 431, and 631

# Create function to find age based on height
Spp8910_AgeFx <- function (SI, b0, b1, H) {
  Age = b1 / ((0.80 * SI) / H - b0)
  return(Age) }

# subset species 8,9,10
SppIndex_8910 <- filter(TreeCoef, SPCD==361 | SPCD==431 | SPCD==631) %>%  
  mutate(TreeAge = Spp8910_AgeFx(SI=SICOND_mean, b0=b0, b1=b1, H=HT)) %>%
  mutate(Bark = BT_coef*DBH_cm)

plot(SppIndex_8910$HT, SppIndex_8910$TreeAge)
plot(SppIndex_8910$TreeAge, SppIndex_8910$Bark)

## ---------------------------------------------------------

# INCENSE CEDAR (CADE27), SUGAR PINE (PILA), WESTERN WHITE PINE (PIMO3), JEFFREY PINE (PIJE), and DOUGLAS FIR (PSME)
# Age solved for using Inland California and Southern Cascades FVS equation 4.7.2.1 large 
# tree height growth for species index 3 and 4. 
# Species Codes: 81, 117, 116, 119, and 202

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
SppIndex_34 <- TreeCoef %>%
  filter(HT < 171) %>%
  filter(SPCD==81 | SPCD==117 | SPCD==202 | SPCD==116 | SPCD==119)

# Find the BtmTerm values and append to SppIndex_34
SppIndex_34$BtmTerm = 0
SppIndex_34$TopTerm = 0 
SppIndex_34 <- SppIndex_34 %>% 
  mutate(BtmTerm = Spp34_BtmTermFx(b0=b0, b1=b1, b2=b2, SI=SICOND_mean)) %>%
  mutate(TopTerm = Spp34_TopTermFx(SI=SICOND_mean, BtmTerm=BtmTerm, H=HT)) %>%
  mutate(TreeAge = Spp34_AgeFx(TopTerm=TopTerm, b0=b0, b1=b1, SI=SICOND_mean, b2=b2)) %>%
  dplyr::select(-c(BtmTerm, TopTerm)) %>%
  mutate(Bark = BT_coef*DBH_cm)

plot(SppIndex_34$HT, SppIndex_34$TreeAge)
plot(SppIndex_34$TreeAge, SppIndex_34$Bark)
boxplot(SppIndex_34$TreeAge ~ SppIndex_34$Species)

tree_bark_DF <- rbind(SppIndex_34, SppIndex_8910) 

tree_bark_DF <- tree_bark_DF[,c("YrFireName","BT_coef","Species","TreeAge","DBH_cm","Bark","yr1status")]
colnames(tree_bark_DF) <- c("yr_firename","BT_coeff","Species","Age","DBH","Bark","Dead")


##--------------
# create shrub dataset to find bark thickness parameters to be used in Mortality.R script
shrub_coef <- read.csv(paste0(dir, "SCRPPLE/Inputs/shrub_bark_thickness_lookup.csv"), header = T)

# Calculate bark thickness
shrub_bark_DF <- NULL
for (i in 1:nrow(shrub_coef)) {
  species <- shrub_coef[i,]
  # create a normal distribution based off of known mean, min, and max species values
  BT_parms <- as.data.frame(rnorm(250, mean=shrub_coef$mean_basalarea_cm2, sd=shrub_coef$stdev_basalarea))
  colnames(BT_parms) <- "basal_area"
  # remove values out of known range
  BT_parms <- filter(BT_parms, basal_area >= species$Min_basalarea_cm2 & basal_area <= species$max_basalarea_cm2)
  # calculate DBH
  BT_parms$DBH <- (sqrt(BT_parms$basal_area) / pi) * 2
  # calculate bark thickness
  BT_parms$BT_coeff <- rep(species$BT_coeff)
  BT_parms$Bark <- BT_parms$DBH * BT_parms$BT_coeff
  # add age, assuming its linear to basal area, based on longevity
  range <- species$longevity / nrow(BT_parms)
  BT_parms$Age <- round(seq(range, species$longevity, by=range))
  BT_parms$Species <- rep(species$LANDIS_NAME)
  
  # calculate mortality - based off of Hoffman et al, 2012, figure 2a
  BT_parms$Dead <- ifelse(BT_parms$Bark <= 0.1, 1, 
                     ifelse(BT_parms$Bark > 0.1 & BT_parms$Bark <= 0.2, rbinom(1,1,0.85), rbinom(1,1,0.8)))
  BT_parms$yr_firename <- rep("estimated")
  BT_parms <- BT_parms[,c("yr_firename","BT_coeff","Species","Age","DBH","Bark","Dead")]
  # merge 
  shrub_bark_DF <- rbind(shrub_bark_DF, BT_parms) 
}

FD_write <- rbind(tree_bark_DF, shrub_bark_DF)
unique(FD_write$Species)
write.csv(FD_write, paste0(dir,"SCRPPLE/bark_mortality.csv"))

boxplot(shrub_bark_DF$Bark ~ shrub_bark_DF$Species)

##--------------------------------
# Fitting DBHAge and Maximum Bark coefficents.
# This section estimates the parameters for “DBHAge” and “Maximum Bark Thickness”, 
# used at the species level to relate age to dbh to bark thickness.

# FD_write <- read.csv(paste0(dir, "SCRPPLE/bark_mortality.csv"))
head(FD_write)

ggplot(FD_write, aes(x=Age, y=Bark, color=Species)) + geom_point()

### Here is the function used in the model 
FitDBHfunction<-function(Age,par,Obs){
  DBH_out<-(par[1]*Age)/(Age+par[2])
  DBH_out <- subset(DBH_out, !is.na(DBH_out))
  return(-sum(dnorm(DBH_out,mean=Obs,sd=3,log=T), na.rm=T))
}

boxplot(FD_write$Bark ~ FD_write$Species)

DF<-NULL
# i=Vacciniu
for(i in unique(FD_write$Species)){
  ## Isolate one species
  OneSp<-FD_write[FD_write$Species==i,]
  ### Optimize the function to the data 
  opt1=optim(c(60,400), f=FitDBHfunction, Age=OneSp$Age, Obs=OneSp$DBH,
             lower=c(0,0), upper=c(200,150), method="L-BFGS-B")
  ## Get parameters
  par1<-as.numeric(opt1$par[1])
  par2<-as.numeric(opt1$par[2])
  print(par2)
  ### Look at the plot
  DBH_out<-(par1*OneSp$Age)/(OneSp$Age+par2)
  plot(OneSp$Age,DBH_out,main=paste0("Species ",i))
  points(OneSp$Age,OneSp$DBH,col="red")
  MaxBark<-par1*OneSp$BT_coeff
  
  score<-summary(lm(DBH_out~OneSp$DBH))$r.squared
  ### create a dataframe of values
  OutRow<-data.frame(Spp=i,maxDBH=par2,MaxBark=MaxBark[1],score=score)
  DF<-rbind(OutRow,DF)
}

print(DF)

# Spp       maxDBH    MaxBark        score
# 1      Vacciniu   0.00000000  0.1696386 0.4997752247
# 2  Rhododendron   0.00000000  0.1057759 0.5006149159
# 3      Ceanothu   0.00000000  0.1120708 0.5004037390
# 4      Arctosta   0.09464961  0.1131576 0.0007947778
# 5         LIDE3  60.35495198  3.7122610 0.7097753215
# 6         CHCH7 126.35188439  9.0000000 0.7351464665
# 7          ARME  30.48149467  3.8192975 0.2831590634
# 8          PSME  16.22575358  3.0653678 0.5309228335
# 9         PIMO3 138.83484569  4.6935953 0.4941133190
# 10         PILA 107.47397630 11.0107650 0.8449473248
# 11         PIJE   0.59047656  3.1472042 0.0936998205
# 12       CADE27  29.48975633  4.5265235 0.7486910664

#---------------------------------------------------------------------------
# Fitting the individual level mortality 
# Here we fit a general linear model to Bark and RDNBR (which we are using for cohort level mortality)

# Using CVS_percent (renamed Severity_percent) from FTM dataset in place of RDNBR
# defined as: the pre-fire crown volume that was scorched or consumed by fire (values 0 to 100). 
# If measured, this is the CVS from field measurements. 

tree_bark_DF <- rbind(SppIndex_34, SppIndex_8910) 
tree_bark_DF <- tree_bark_DF[,c("YrFireName","BT_coef","Species","TreeAge","DBH_cm","Bark","yr1status", "CVS_percent")]
colnames(tree_bark_DF) <- c("yr_firename","BT_coeff","Species","Age","DBH","Bark","Dead", "Severity_percent")
tree_bark_DF_comp <- tree_bark_DF[complete.cases(tree_bark_DF),]

glm1 <- with(tree_bark_DF_comp, glm(Dead ~ Bark + Severity_percent, family = "binomial"))  
summary(glm1)  

# Call:
#   glm(formula = Dead ~ Bark + Severity_percent, family = "binomial")
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2189  -0.1452  -0.0587   0.4430   4.4681  
# 
# Coefficients:
#   Estimate Std. Error z value            Pr(>|z|)    
# (Intercept)      -5.180317   0.137506  -37.67 <0.0000000000000002 ***
#   Bark             -0.603774   0.022568  -26.75 <0.0000000000000002 ***
#   Severity_percent  0.075682   0.001418   53.39 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 24157.3  on 19999  degrees of freedom
# Residual deviance:  9025.1  on 19997  degrees of freedom
# AIC: 9031.1
# 
# Number of Fisher Scoring iterations: 7

BarkThickness <- seq(0,10,.1)

ysim_LowFlame <- predict(glm1,data.frame(Bark = BarkThickness, 
                                         Severity_percent = (rep(33, length(BarkThickness)))),
                       type="response",se=T)

ysim_MedianFlame <- predict(glm1,data.frame(Bark = BarkThickness, 
                                          Severity_percent = rep(66, length(BarkThickness))),
                          type="response",se=T)
ysim_MaxFlame <- predict(glm1,
                       data.frame(Bark = BarkThickness, 
                                  Severity_percent = rep(max(100,na.rm = T),length(BarkThickness))),
                       type="response",se=T)

### Plotting the model 
plot(FD_write$Bark, FD_write$Dead, pch = 16, xlab = "Bark thickness", ylab = "Percent canopy killed", 
     ylim=c(0,1.0), xlim=c(0,10))

lines(BarkThickness,ysim_MedianFlame$fit+ysim_MedianFlame$se.fit,col="black",lwd=1.0,lty=3.0)
lines(BarkThickness,ysim_MedianFlame$fit,col="orange",lwd=3.0)
lines(BarkThickness,ysim_MedianFlame$fit-ysim_MedianFlame$se.fit,col="black",lwd=1.0,lty=3.0)
lines(BarkThickness,ysim_MaxFlame$fit+ysim_MaxFlame$se.fit,col="red",lwd=1.0,lty=3.0)
lines(BarkThickness,ysim_MaxFlame$fit,col="red",lwd=3.0)
lines(BarkThickness,ysim_MaxFlame$fit-ysim_MaxFlame$se.fit,col="red",lwd=1.0,lty=3.0)

lines(BarkThickness,ysim_LowFlame$fit+ysim_LowFlame$se.fit,col="blue",lwd=1.0,lty=3.0)
lines(BarkThickness,ysim_LowFlame$fit,col="blue",lwd=3.0)
lines(BarkThickness,ysim_LowFlame$fit-ysim_LowFlame$se.fit,col="blue",lwd=1.0,lty=3.0)
legend(5.5, 0.975, 
       legend=c("High severity (67-100%)","Mixed severity (34-66%)","Low severity (0-33%)"),
       lty=c(1,1,1), box.lty = 0,
       col=c("red","black","blue")) 


