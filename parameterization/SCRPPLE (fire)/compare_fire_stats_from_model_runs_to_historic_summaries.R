###############################################################################
### Compare fire stats from model run to Karen Short and MTBS Severity data ###
######################## Written by Shelby Weiss ##############################
################ Updated by Alison Deak, February 2022 ########################

library(tidyverse)
library(patchwork)
options(scipen = 99999999)

# folder with model run inputs/outputs
# model_dir <- "/Volumes/Alison.Deak_440-223-4897/model_analysis/Siskiyous/"
# data_dir <- "/Volumes/Alison.Deak_440-223-4897/SCRPPLE/calibration/output/"

SCENARIO <- "51 years, Historical, 10x-Rx Scenario" #, high Rx scenario (5 Rx fires/year, max fire size: 14 hectares"

# model_dir <- "/Volumes/Alison.Deak_440-223-4897/model_analysis/2022-05-22/a"
model_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/Historical/rep-1/3x-Rx/"
# data_dir <- "E://MS Research/SCRPPLE/calibration/output/"
data_dir <- "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/SCRPPLE/Outputs/"

# hectares per cell
ha_per_cell <- 0.09
# the number is the total number of active cells (determined from the Landis-log file)
totalarea <- 8828190 * ha_per_cell

##------------------------
# read in summarized historic data
fire.stat.hist <- read.csv(paste0(data_dir, "Fire_History_Stats_FULL_v2.csv"))[,-1] %>%
  dplyr::select(Name, avg_ha_per_fire, max_ha_burned, avg_ha_burned_per_yr,avg_fires_yr_Human, avg_fires_yr_Natural)
hist_size_grouped <- read.csv(paste0(data_dir, "fires_grouped-by-size_KarenShort_1992-2017_v2.csv"))[,-1]

# read in simulation results
all.firedat <- read.csv(paste0(model_dir,"scrapple-summary-log.csv")) %>%
  filter(SimulationYear > 1) %>%
  mutate(TotalBurnedSites_notRx = TotalBurnedSitesAccidental + TotalBurnedSitesLightning, 
         NumberFires_notRx = (NumberFiresAccidental + NumberFiresLightning)) %>%
  mutate(AvgFireSizes = ((TotalBurnedSites_notRx* ha_per_cell) / NumberFires_notRx),
         TotalFireSizes = (TotalBurnedSites_notRx * ha_per_cell),
         avg_fires_yr_Human = mean(NumberFiresAccidental),
         avg_fires_yr_Natural = mean(NumberFiresLightning))

breaks <- seq(from= 0, to = 202500, by = 25)

firedat_histogram_summ <- read.csv(paste0(model_dir,"/scrapple-events-log.csv")) %>%
  filter(SimulationYear > 1 & IgnitionType != " Rx") %>%
  mutate(ID = as.character(EventID), Hectares = TotalSitesBurned * ha_per_cell, 
         Name = "Simulated",  Bins = cut(Hectares, breaks=breaks, lables=TRUE)) %>%
  dplyr::select(ID, Hectares, Name, Bins)

Tags <- str_split(firedat_histogram_summ$Bins, ",") %>% 
  unlist() %>% 
  matrix(ncol=2, byrow=TRUE) %>% 
  as.data.frame() %>%
  dplyr::select(V1) %>%
  mutate(V1 = str_replace_all(V1, "\\(", ""))

firedat_histogram_grouped <- firedat_histogram_summ %>%
  mutate(Bins = Tags$V1) %>%
  group_by(Bins) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = (Count/(sum(Count)))*100,Bins = as.numeric(Bins), Name = "Simulated") 

# Manually adjust to years the sim ran and the resolution of your cell size.
sim_years <- max(all.firedat$SimulationYear)

# summary of all fire data
fire.stat <- read.csv(paste0(model_dir,"/scrapple-events-log.csv")) %>%
  group_by(SimulationYear) %>%
  summarise(avg_ha_per_fire = (mean(TotalSitesBurned) * ha_per_cell),
            max_ha_burned = (max(TotalSitesBurned) * ha_per_cell),
            avg_ha_burned_per_yr = (mean(sum(TotalSitesBurned) * ha_per_cell))) %>%
  ungroup() %>%
  summarise(Name = "Simulated",
            avg_ha_per_fire = mean(avg_ha_per_fire), 
            max_ha_burned = max(max_ha_burned),
            avg_ha_burned_per_yr = mean(avg_ha_burned_per_yr),
            avg_fires_yr_Human = all.firedat$avg_fires_yr_Human[1],
            avg_fires_yr_Natural = all.firedat$avg_fires_yr_Natural[1]) %>%
  rbind(fire.stat.hist)
            
#-----------------------------------
## SEVERITY DATA
# read in file with summarized severities
severity_92to17 <- read.csv(paste0(data_dir, "mtbs_severity_1992_2017.csv"))
severity_92to17 <- severity_92to17[,-1]
head(severity_92to17)

## Severities from sim
# severity_reclass <- read.csv("/Volumes/Alison.Deak_440-223-4897/SCRPPLE/calibration/data/severity_reclass_matrix.csv")[,-1] %>% as.matrix()

sim_severity <- read.csv(paste0(model_dir,"/scrapple-events-log.csv")) %>%
  filter(IgnitionType != " Rx" & SimulationYear > 1) %>%
  dplyr::select(EventID, SimulationYear, MeanDNBR) %>%
  mutate(severity = case_when(MeanDNBR >= 0 & MeanDNBR <= 200 ~ 1, 
                              MeanDNBR > 200 & MeanDNBR <= 439 ~ 2, 
                              MeanDNBR > 439 & MeanDNBR <= 2000 ~ 3),
         Name = "Simulated")  %>%
  dplyr::rename(year=SimulationYear) %>%
  dplyr::select(severity, year, Name)

# fire severity distribution
compare_severity_92to17 <- rbind(sim_severity, severity_92to17)

severity_summ <- data.frame(with(compare_severity_92to17, table(severity, Name)))
sev_summ_hist <- severity_summ %>% filter(Name=="Historical") %>% mutate(Percent = Freq/sum(Freq)*100)
sev_summ_sim <- severity_summ %>% filter(Name=="Simulated") %>% mutate(Percent = Freq/sum(Freq)*100)
severity_summ <- rbind(sev_summ_hist, sev_summ_sim)
severity_summ

### Plot comparisons ### -------------------------------------------------------------------------------------------
compare_histogram <- rbind(hist_size_grouped, firedat_histogram_grouped) %>% mutate(Perc_Rnd = ceiling(Percent))

# rebuild frequency df based on percentages
compare_histogram_new <- NULL
for (i in 1:nrow(compare_histogram)) {
  compare_histogram_new <- rbind(compare_histogram_new, compare_histogram[rep(i, compare_histogram[i,5]),])
}
compare_histogram_new <- compare_histogram_new[,c(1,4)]
compare_histogram_92to17 <- subset(compare_histogram_new, compare_histogram_new$Name=="Historic Karent Short 1992-2017")

compare_AccidentalIgnPerYr <- ggplot(data=fire.stat, aes(Name, avg_fires_yr_Human, group=Name)) +
  geom_bar(stat = "identity", aes(fill = Name)) +
  # geom_errorbar(aes(x=Name, y=FiresPerYr, ymin=FiresPerYr, ymax=FiresPerYr + FiresPerYr_sd), width=0, alpha=0.5, size=.8) +
  ggtitle("Average # Accidental Ignitions / Year") +
  scale_fill_manual(values=c("#568956", "#67D567")) +
  theme_classic() +
  theme(legend.position = "none")
compare_AccidentalIgnPerYr
# ggsave(paste0(output_dir, "firesperyr.pdf"))

compare_NaturalIgnPerYr <- ggplot(data=fire.stat, aes(Name, avg_fires_yr_Natural, group=Name)) +
  geom_bar(stat = "identity", aes(fill = Name)) +
  # geom_errorbar(aes(x=Name, y=FiresPerYr, ymin=FiresPerYr, ymax=FiresPerYr + FiresPerYr_sd), width=0, alpha=0.5, size=.8) +
  ggtitle("Average # Natural Ignitions / Year") +
  scale_fill_manual(values=c("#568956", "#67D567")) +
  theme_classic() +
  theme(legend.position = "none")
compare_NaturalIgnPerYr
# ggsave(paste0(output_dir, "firesperyr.pdf"))

compare_HAburnedPerYr <- ggplot(data=fire.stat, aes(Name, avg_ha_burned_per_yr)) +
  geom_bar(stat = "identity", aes(fill = Name)) +
  # geom_errorbar(aes(x=Name, y=HAburnedPerYr, ymin=HAburnedPerYr, ymax=HAburnedPerYr + HAburnedPerYr_sd), width=0, alpha=0.5, size=.8) +
  ggtitle("Average Hectares Burned / Year") +
  scale_fill_manual(values=c("#568956", "#67D567")) +
  theme_classic() +
  theme(legend.position = "none")
compare_HAburnedPerYr
# ggsave(paste0(output_dir, "ha_burned_per_yr.pdf"))

compare_MaxHABurned <- ggplot(data=fire.stat, aes(Name, max_ha_burned)) +
  geom_bar(stat = "identity", aes(fill = Name)) +
  ggtitle("Maximum Fire Size (Ha)") +
  scale_fill_manual(values=c("#568956", "#67D567")) +
  theme_classic() +
  theme(legend.position = "none")
compare_MaxHABurned
# ggsave(paste0(output_dir, "maxHA_burned.pdf"))

compare_AvgHAPerFire <- ggplot(data=fire.stat, aes(Name, avg_ha_per_fire)) +
  geom_bar(stat = "identity", aes(fill = Name)) +
  # geom_errorbar(aes(x=Name, y=AvgHAPerFire, ymin=AvgHAPerFire, ymax=AvgHAPerFire + SDHAPerFire), width=0.0, alpha=0.5, size=.8) +
  ggtitle("Average Fire Size (Ha)") +
  scale_fill_manual(values=c("#568956", "#67D567", "#F7E925")) +
  theme_classic()
compare_AvgHAPerFire
# ggsave(paste0(output_dir, "average_fire_size.pdf"))

compare_firesizedistr <- ggplot(compare_histogram, aes(x=Bins, fill=Name)) +
  geom_histogram(color="#e9ecef", alpha=0.8, position = 'dodge') +
  ggtitle("Fire Size Distribution") +
  scale_fill_manual(values=c("#568956", "#67D567", "#F7E925")) +
  ylab("Percentage of Fires") +
  theme_classic() +
  theme(legend.position = "none")
compare_firesizedistr
# ggsave(paste0(output_dir, "fire_size_disribution.pdf"))

compare_severitydistr_92to17 <- ggplot(severity_summ, aes(x=Name, y=Percent, fill=severity)) +
  geom_bar(color="#e9ecef", position='fill', stat='identity') +
  ggtitle("Fire Severity Distribution") +
  scale_fill_manual(values=c("cornsilk", "orange", "maroon")) +
  ylab("Severity Proportions") +
  theme_classic() +
  theme(legend.position = "none")
compare_severitydistr_92to17
ggsave(paste0(model_dir, "severity_comparison.pdf"))

# plot altogether
all_plots <- ((compare_AccidentalIgnPerYr|compare_HAburnedPerYr) / (compare_NaturalIgnPerYr|compare_AvgHAPerFire) / compare_severitydistr_92to17 / compare_firesizedistr) +
  plot_annotation(title = "Simulation results compared to historic stats 1992-2018",
                  caption = SCENARIO)
all_plots

ggsave(paste0(model_dir, "all_fire_plots.pdf"), plot = last_plot())

