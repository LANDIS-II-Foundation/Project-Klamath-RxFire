 
##Loading required libraries.
library (plyr)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('hydroPSO')
library(hydroPSO)
#install.packages('hydroTSM')
library(hydroTSM)
#install.packages('hydroGOF')
library(hydroGOF)

##Definition of working directory: input, output and model files paths.
model.drty <- "C://Users/EWP/Desktop/HydroPSO_Calibration/"
setwd(model.drty)                   

##Customized I/O functions (R scripts) to interface MF2005 with hydroPSO.

source("read.LANDIS.R") ### R script to read in the output data you generate with the program.
source("read.lik.R")                ## R script for the goodness of fit file

##Goodness-of-fit function, either customized or pre-defined GOFs of hydroPSO (or hydroGOF).
### R function or pre-defined GOF used in hydroPSO (FILL WITH THE NAMES OF THE PREDEFINED GOFs FUCNTIONS).
gof.FUN      <- "read.lik"

### Additional arguments of gof.FUN (only if required).
gof.FUN.args <- list() 

#obs <-read.table(paste(model.drty,"UMBSyrSoil2.txt", sep=""), fill=TRUE, header=F, sep="\t", skip=1)
# obs <-read.csv(paste(model.drty,"all_rel_max_biomass_and_lai_from_fia.csv", sep=""), fill=TRUE, header=T)
# obs <- subset(obs, obs$Species=="Ceanothu") #change to species you're working with
# obs <- obs[1:2,]
obs_bio <- c(13694, 25900)  # based on greatest biomass and lai calculated from FIA data
obs_lai <- c(5.6, 5.5) 
obs <- c(obs_bio, obs_lai)

tot_path <- paste(model.drty)
print(tot_path)

###MAIN model function
model.FUN="LANDIS"                                                        ### Keyword to calibrate a model different from a test function.     
model.FUN.args=list(                                                        ### List with the arguments to be passed to 'model.FUN'.
  model.drty=model.drty,                                                      ### Model directory.  
  param.files=paste(model.drty, "PSO.in/ParamFiles_NothDens.txt", sep=""),            ### Name of the file storing the name of the files that have to be modified for each parameter.
  exe.fname="./BatchFile.bat",                                                    ### Name of the executable (or batch) file for the model to be calibrated.
  stdout="stdout.txt",                                                                  ### OPTIONAL, model messages to the screen
  stderr="stderr.txt",
  ###Function for reading 'simulated equivalents' of the model to be calibrated
  out.FUN="read.LANDIS",   ### Name of the R function to read the output of the model contained in 'fname'.
  #out.FUN.args=list(obs,sim),
  out.FUN.args=list(tot_path), ### List with the arguments to be passed to 'out.FUN'.
  verbose=TRUE,                                                       ### MODFLOW-2000 list file summarizing the results (local variable for read.hsim.R).
  ### Number of observation wells (local variable for read.hsim.R).
  ###Function for comparing simulated equivalents against observations 
  gof.FUN=gof.FUN,                                                         ### Name of the GoF function used to compare simulations and observations.
  gof.FUN.args=gof.FUN.args,                                               ### List with additional arguments to be passed to 'out.FUN' (additional to 'sim' and 'obs').
  obs=obs                                                                 ### Vector of observed values.
) ##END model.FUN.args


##MAIN PSO ALGORITHM. For hydroPSO fine-tuning parameters, see Zambrano-Bigiarini and Rojas, 2012. 
hydroPSO(
  fn="hydromod",                                                          ### Default name to calibrate a model.
  model.FUN="hydromod",                                                   ### Default keyword of hydroPSO.
  model.FUN.args=model.FUN.args,                                          ### Arguments for the model to be calibrated.
  method="spso2011",                                                      ### A valid PSO variant: spso2011, spso2007, fips, wfips, ipso.
  control=list(
    MinMax="max",                                                         ### Maximization of the objective function (Gaussian Likelihood).
    npart=35,                                                             ### Number of particles.
    maxit=10,                                                            ### Number of iterations.
    reltol=1e-8,                                                         ### Numeric tolerance to stop successive hydroPSO iterations. 
    c1=2.05,                                                              ### Acceleration coefficient c1.
    c2=2.05,                                                              ### Acceleration coefficient c2.
    use.IW=FALSE,                                                         ### Parameters for defining the inertia weight scheme employed.
    use.CF=TRUE,                                                          ### Using the Clerc's contriction factor
    use.TVlambda=TRUE,TVlambda.type="linear",TVlambda.rng=c(1.0,0.5),TVlambda.exp=1,  ### Parameters for defining a time-varying velocity constraint lambda.
    topology="gbest",                                              ### Parameters for defining a random topology with K informants
    drty.out="PSO.out",                                          ### Directory that will store the results of hydroPSO.
    boundary.wall="reflecting",
    normalise=FALSE,  #turned off true because of non-conformable array error - IPL
    REPORT=1,                                                             ### Interval to report messages to screen.
    ### control for multicore/Parallel options
    parallel="none",
    par.nnodes=1,
    par.pkgs=c("hydroGOF","hydroTSM")
    ###
  ) ##END control options
) ##END MAIN hydroPSO ALGORITHM
dev.off()

# Writing the file 'Particles_GofPerIter.txt', with the GoF for each particle in each iteration
drty.out <- paste0(model.drty, "PSO.out/")


#plot model performance
read_results(drty.out="PSO.out", MinMax=NULL, beh.thr=NA, 
             modelout.cols=NULL, nsim=NULL, verbose=TRUE)
plot_results(do.png = TRUE, MinMax = "max", do.pairs=TRUE,legend.pos="topleft")
dev.off()


###########################
# Plot best results
agb <- read.csv(paste0(model.drty, "NECN-succession-log-short.csv"))[,c(1,4)]
names(agb)[1] <- "Year"
sim_LAI <- read.csv(paste0(model.drty, "NECN-calibrate-log.csv"))[,c(1,4,11,14:18)]
sim_LAI <- aggregate(sim_LAI, by=list(sim_LAI$Year, sim_LAI$SpeciesName), FUN=max)[,-c(1,2)]

agb <- left_join(agb, sim_LAI, by="Year")[,-3]

# Always use this when reading in FIA plots so that you get the full plot name. 
options(scipen=999)

#directory with FIA data
FIA_results_dir<- "C://Users/EWP/Desktop/HydroPSO_Calibration/Results/"

max_observed <-read.csv(paste0(FIA_results_dir, "tree_max_biomass_lai_from_FIA_FINAL.csv", sep=""), fill=TRUE, header=T)


MaxBiomass_bs <- subset(max_observed, max_observed$Species=="NothDens")
MaxBiomass_bs <- MaxBiomass_bs[,-c(1,4)]
MaxBiomass_bs <- MaxBiomass_bs[,c("StandAge","Max_Biomass","Max_LAI","Dataset")]
colnames(MaxBiomass_bs) <- c("Year", "AGB","TreeLAI", "Dataset")
agb$Dataset <- "Simulated"
agb <- agb[,c("Year","AGB","TreeLAI","Dataset")]
combo <- rbind(MaxBiomass_bs, agb)
# binned_FIA <- read.csv(paste0(model.drty, "Rel_Max_Biomass_from_FIA_first30yrs.csv"))
# binned_FIA <- subset(binned_FIA, binned_FIA$Species=="PicMar_biomass")
# binned_FIA <- binned_FIA[,-c(1,4)]
# colnames(binned_FIA) <- c("Year", "AGB")
# binned_FIA$Dataset <- "BinnedMaxes_FIA"
# binned_FIA$LAI <- NA
# combo2 <- rbind(combo, binned_FIA)

#plot biomass and LAI
ggplot(data=combo, aes(x=Year, y=AGB, color=Dataset)) +
  geom_point() 

ggsave("NothDens_biomass_1startcohort_35particles.png",
       plot = last_plot(),
       path = "PSO.out/",
       dpi = 300,
       limitsize = TRUE)

ggplot(data=combo, aes(x=Year, y=TreeLAI, color=Dataset)) +
  geom_point() 

ggsave("NothDens_LAI_1startcohort_35particles.png",
       plot = last_plot(),
       path = "PSO.out/",
       dpi = 300,
       limitsize = TRUE)

agb <- read.csv(paste0(model.drty, "NECN-succession-log-short.csv"))[,c(1,4)]
names(agb)[1] <- "Year"
sim_LAI <- read.csv(paste0(model.drty, "NECN-calibrate-log.csv"))[,c(1,4,11,14:22)]
sim_LAI <- aggregate(sim_LAI, by=list(sim_LAI$Year, sim_LAI$SpeciesName), FUN=max)[,-c(1,2)]

agb <- left_join(agb, sim_LAI, by="Year")[,-3]

#plot limits
limits <- agb %>%
  pivot_longer(GrowthLimitLAI:GrowthLimitLAIcompetition, names_to = "limit_type", values_to = "limit")

limits_plot <- ggplot(limits, aes(x=Year, y=limit, color=limit_type)) +
  geom_line()
limits_plot

ggsave("NothDens_limits_plot",
       plot = last_plot(),
       path = "PSO.out/",
       dpi = 300,
       limitsize = TRUE)


combo_fia <- subset(combo, combo$Dataset=="FIA")
ggplot(data=combo_fia, aes(x=Year, y=TreeLAI, color=Dataset)) +
  geom_point() 
