# Script adapted from Zachary Robbins Nov. 2020 script "ignitions_FWI_11_2020" posted on GitHub

library(RColorBrewer)
library(pscl)
library(plyr)
library(sf)
library(rgdal)

red<-RColorBrewer::brewer.pal(9,'YlOrRd')

# Set up directories
w.dir <- "/Volumes/LaCie/SCRPPLE/"
out.dir <- "/Users/alisondeak/Desktop/SPA Lab/RScript&Data/Landis-Siskiyous/"

####  2a Look at the K. Short data. 
ignition_dat <- readOGR(paste0(w.dir, "Short_ignitions_CA_OR/Short_ignitions_CA_OR.shp", sep=""))
ignition_dat <- spTransform(ignition_dat, CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs"))
plot(ignition_dat$geometry,axes=T)
head(ignition_dat)

### 2b Looking at our study area
StudyArea <- readOGR(paste0(out.dir, "FinalProducts/landscape_shape/landscape_shape.shp", sep=""))
StudyArea <- spTransform(StudyArea, CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs"))

Int <- ignition_dat[StudyArea,]
plot(StudyArea,col="red")
plot(Int,add=T)

### 2c Subsetting viewing the data
### Ligntning 
l_fire_dat <- Int[Int$NWCG_CAUSE == "Natural",]
head(l_fire_dat)
l_fire_days <- as.data.frame(cbind(l_fire_dat$FIRE_YEAR, l_fire_dat$DISCOVERY1)) #Extracting year and julian day
colnames(l_fire_days) <- c("YEAR", "J_DAY")
l_fire_days_sort <- l_fire_days[order(l_fire_days[,1]),] #sorting b
#plot no of fires/yr
l_fire_days_sort_count<- cbind(l_fire_days_sort, rep(1, nrow(l_fire_days_sort)))
l_fires_count <- cbind.data.frame(unique(l_fire_days_sort_count$YEAR) , 
                                  tapply(l_fire_days_sort_count$`rep(1, nrow(l_fire_days_sort))`,l_fire_days_sort_count$YEAR, sum))
colnames(l_fires_count) <- c("YEAR", "COUNT")
barplot(l_fires_count$COUNT, main ="No of ign/yr Lightning Observed",col=red,names.arg=l_fires_count$YEAR)

### Human
h_fire_dat <- Int[Int$NWCG_CAUSE == "Human",]
h_fire_days <- as.data.frame(cbind(h_fire_dat$FIRE_YEAR, h_fire_dat$DISCOVERY1)) #Extracting year and julian day
colnames(h_fire_days) <- c("YEAR", "J_DAY")
h_fire_days_sort <- h_fire_days[order(h_fire_days[,1]),] #sorting by year
#plot no of fires/yr
h_fire_days_sort_count<- cbind(h_fire_days_sort, rep(1, nrow(h_fire_days_sort)))
h_fires_count <- cbind.data.frame(unique(h_fire_days_sort_count$YEAR) , 
                                  tapply(h_fire_days_sort_count$`rep(1, nrow(h_fire_days_sort))`,h_fire_days_sort_count$YEAR, sum))
colnames(h_fires_count) <- c("YEAR", "COUNT")
barplot(h_fires_count$COUNT, main ="No of ign/yr Human Accidental Observed",col=red,names.arg=h_fires_count$YEAR)

## Match them up with Fire weather index(FWI) data generated from the historic record of weather 
## fed through LANDIS-II Fire weather to get the models calculation of FWI. 
ign_types <- c("Lightning", "HumanAccidental")
fire_days_list <- list(l_fire_days_sort, h_fire_days_sort) ##organizing all ignition types into a list
##Import daily historic FWI data
FWI_dat <- read.csv(paste0(out.dir, "SCRPPLE/Climate-future-input-log.csv"))
FWI_dat<-with(FWI_dat,aggregate(FWI,by=list(Year=Year,Timestep=Timestep),FUN=mean))
FWI_dat <- FWI_dat[,c(1,2,3)]
colnames(FWI_dat)[3]<-"FWI"
FWI_dat$ID <- paste(FWI_dat$Year, "_", FWI_dat$Timestep, sep="") #creating date identifier out of date and julian day
igns_list <- list()
for (i in 1:length(ign_types[1:2])){#THIS DOESN'T INCLUDE RX BURNS BUT THATS CAUSE WE ARE PROVIDING THOSE TO SCRAPPLE DIRECT
  ign_type_select <- fire_days_list[[i]] ##selecting each ignition type individually
  fire_days_count <- ddply(ign_type_select, .(ign_type_select$YEAR, ign_type_select$J_DAY), nrow) #finds duplicate rows in fire data
  colnames(fire_days_count) <- c("YEAR", "JD", "No_FIRES") #Renaming columns for consistency
  fire_days_count$ID <- paste(fire_days_count$YEAR, "_", fire_days_count$JD, sep="") #creating date identifier out of date and julian day
  ##Merging dataframes by year and julian day
  fire_days_short <- subset(fire_days_count, fire_days_count$YEAR < 2018) ##restricting fire records to climate file years
  FWI_short <- subset(FWI_dat, FWI_dat$Year > 1991) #restricting climate data to fire history records
  merge_col <- FWI_short$ID
  FWI_fire_merge <- join(FWI_short, fire_days_short, type="left") ##Merging based on unique date id
  FWI_fire_number <- FWI_fire_merge[,c(1,3,7)] #pulling out FWI and number of fires
  FWI_fire_number[is.na(FWI_fire_number)] <- 0 #converting NAs to 0, meaning 0 fires
  plot(FWI_fire_number[,2], FWI_fire_number[,3], main =ign_types[i], xlab = "dailyFWI", ylab = "noFires") #plotting FWI against no of fires just to look at pattern
  igns_list[[i]] <- FWI_fire_number 
}

#Lighting
print("Lighting")
summary(igns_list[[1]])
#Humnan
print("Human")
summary(igns_list[[2]])

# Fit and decide whether to use Poisson or Zero-inflated Poisson model
# Poisson
Lightning<-as.data.frame(igns_list[[1]])
POIS_mod_L <- glm(No_FIRES~FWI, data=Lightning, family="poisson")
summary(POIS_mod_L) # b0 = -5.800479, b1 = 0.082444

Accidental<-as.data.frame(igns_list[[2]])
POIS_mod_A <- glm(No_FIRES~FWI,data=Accidental, family="poisson")
summary(POIS_mod_A) # b0 = -4.940328, b1 = 0.075218

# Zero-inflated Poisson
###Lightning - doesn't fit as well
zeroinf_mod_L <- zeroinfl(No_FIRES~FWI,data=Lightning, dist="poisson")
summary(zeroinf_mod_L)
Tst<-predict(zeroinf_mod,type="zero")

###Human accidental - fits just as well as poisson
zeroinf_mod_A<- zeroinfl(No_FIRES~FWI,data=Accidental, dist="poisson")
summary(zeroinf_mod_A)

####----
# Testing - Accidental
FWI_dat<- read.csv(paste0(w.dir, "SCRPPLE/Climate-future-input-log.csv"))
FWI_dat<-with(FWI_dat,aggregate(FWI,by=list(Year=Year,Timestep=Timestep),FUN=mean))
FWI_dat<-FWI_dat[FWI_dat$Year %in% seq(1992,2016),]
FWI_Test<-FWI_dat$x
Pb0<-(-4.940328)
Pb1<-(0.075218)
###Possion model 
Acc_Ign_Test_Pois<-rpois(9125, lambda = exp((Pb0+(Pb1*FWI_Test))))

##ZIPS model 
binomb0=(5.25921)
binomb1=(-0.22869)
b0=(-1.21572)
b1=(-0.03518)
alpha<-exp(binomb0+binomb1*FWI_Test)
zeroprob= alpha/(alpha+1)
Acc_Ign_Test<-ifelse((zeroprob<=runif(length(zeroprob),0,1)),
                     round(rpois(9125, lambda = exp((b0+(b1*FWI_Test))))),
                     0)
### Aggregate to the year 
Acc_Ign_df<-cbind(FWI_dat[,c(1,2)],Acc_Ign_Test)
Acc_Years<-aggregate(list(Ignitions=Acc_Ign_df$Acc_Ign_Test),by=list(Year=Acc_Ign_df$Year),FUN=sum)
PAcc_Ign_df<-cbind(FWI_dat[,c(1,2)],Acc_Ign_Test_Pois)
PAcc_Years<-aggregate(list(Ignitions=PAcc_Ign_df$Acc_Ign_Test_Pois),by=list(Year=PAcc_Ign_df$Year),FUN=sum)

# jpeg(paste0(out.dir,"SCRPPLE/plots/ignition_results_accidental.jpg"))
plot(PAcc_Years$Year,PAcc_Years$Ignitions,col="darkgreen",
     pch=16,cex=1.3,ylab="Accidental Ignition",xlab="Year", 
     main="comparing ignitions results - Accidental ignitions")
points(Acc_Years$Year,Acc_Years$Ignitions,col="purple",pch=16,cex=1.3)
points(h_fires_count$YEAR,h_fires_count$COUNT,col="orange",pch=16,cex=1.3)
legend(2009,23,legend=c("Accidental Poisson","Accidental ZIPS","Karen Short"),
       pch=c(16,16,16),col=c("darkgreen","purple","orange"))
# dev.off()

## Checking lightning ignition results

FWI_dat<- read.csv(paste0(w.dir, "SCRPPLE/Climate-future-input-log.csv"))
FWI_dat<-with(FWI_dat,aggregate(FWI,by=list(Year=Year,Timestep=Timestep),FUN=mean))
FWI_dat<-FWI_dat[FWI_dat$Year %in% seq(1992,2016),]
FWI_Test<-FWI_dat$x
Pb0<-(-5.800479)
Pb1<-(0.082444)
###Possion model 
Lig_Ign_Test_Pois<-rpois(9125, lambda = exp((Pb0+(Pb1*FWI_Test))))
##ZIPS model 
binomb0=(5.25921)
binomb1=(-0.22869)
b0=(-1.21572)
b1=(-0.03518)
alpha<-exp(binomb0+binomb1*FWI_Test)
zeroprob= alpha/(alpha+1)
Lig_Ign_Test<-ifelse((zeroprob<=runif(length(zeroprob),0,1)),
                     round(rpois(9125, lambda = exp((b0+(b1*FWI_Test))))),
                     0)
### Aggregate to the year 
Lig_Ign_df<-cbind(FWI_dat[,c(1,2)],Lig_Ign_Test)
Lig_Years<-aggregate(list(Ignitions=Lig_Ign_df$Lig_Ign_Test),by=list(Year=Lig_Ign_df$Year),FUN=sum)
PLig_Ign_df<-cbind(FWI_dat[,c(1,2)],Lig_Ign_Test_Pois)
PLig_Years<-aggregate(list(Ignitions=PLig_Ign_df$Lig_Ign_Test_Pois),by=list(Year=PLig_Ign_df$Year),FUN=sum)

# jpeg(paste0(out.dir,"SCRPPLE/plots/ignition_results_lightning.jpg"))
plot(PLig_Years$Year,PLig_Years$Ignitions,col="darkgreen",ylim=c(0,100),pch=16,cex=1.3,ylab="Lightning Ignition",xlab="Year")
points(Lig_Years$Year,Lig_Years$Ignitions,col="purple",pch=16,cex=1.3)
points(l_fires_count$YEAR,l_fires_count$COUNT,col="orange",pch=16,cex=1.3)
legend(1995,100,legend=c("Lightning Possion","Lightning ZIPS","Karen Short"),pch=c(16,16,16),col=c("darkgreen","purple","orange"))
# dev.off()
