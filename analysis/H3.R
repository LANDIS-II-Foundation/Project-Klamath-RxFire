library(tidyverse)
library(terra)
library(ggridges)
library(MetBrewer)
library(FSA)
library(scales)

##-----------------------------
# H3. The average size of wildland fires on the study landscape will decrease with increasing 
# prescribed fire frequency and extent of prescribed fires in all climate scenarios. 
# This will be most pronounced under a warmer and drier climate scenario.
##-----------------------------

wd <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/"

options(scipen = 33333)
ha_per_cell <- 0.09
number_yrs <- 51
n_cells <- 8828190

##-----------------------------
## AREA BURNED BY 50% LARGEST FIRE EVENTS
##-----------------------------
# h=1; i=1; j=1
all_events <- NULL
all_scen <- list.files(wd)
for (h in 1:length(all_scen)) {
  clim <- all_scen[[h]]
  clim_scenario <- list.files(paste0(wd, clim))
  for (i in 1: length(clim_scenario)) {
    rep <- clim_scenario[[i]]
    mgmt_scenario <- list.files(paste0(wd, clim, "/", rep))
    for (j in 1:length(mgmt_scenario)) {
      management <- mgmt_scenario[[j]]
      events <- read.csv(paste0(wd, clim, "/",rep,"/", management,"/scrapple-events-log.csv")) %>%
        filter(IgnitionType == " Accidental" | IgnitionType == " Lightning") %>%
        select(SimulationYear, TotalSitesBurned) %>%
        mutate(log_ha_burned = (log(TotalSitesBurned * ha_per_cell)), 
               ha_burned = TotalSitesBurned * ha_per_cell,
               clim_scenario = clim, rep = i, mgmt_scenario = management) %>%
        filter(log_ha_burned > 0)

      all_events <- rbind(all_events, events)
    }
  }
}

for_analysis <- all_events %>%
  slice_max(log_ha_burned, prop = 0.5) %>%
  filter(clim_scenario == "Warmer&drier") %>%
  mutate(median_ha_burned = median(log_ha_burned), 
         scenario = paste0(clim_scenario, "." , mgmt_scenario),
         clim_scenario = as.factor(clim_scenario),
         mgmt_scenario = as.factor(mgmt_scenario))



## Statistical test
bartlett.test(for_analysis$log_ha_burned, for_analysis$scenario) 
# variances are NOT equal unless p-value > 0.05

one_way <- aov(log_ha_burned ~ scenario, data = for_analysis)
summary(one_way)
TukeyHSD(one_way)

two_way <- aov(log_ha_burned ~ mgmt_scenario + clim_scenario, data = for_analysis)
summary(two_way)
TukeyHSD(two_way)

## if variances are not equal: Kruskal-wallis test followed by Dunns test
# kruskal.test(for_analysis$ha_burned, for_analysis$scenario)
# dunnTest(ha_burned ~ scenario,data=for_analysis, method="bonferroni")

##-----------------------------
## DISTRIBUTIONS OF ANNUAL AREA BURNED
##-----------------------------
for_plot <- all_events %>%
  mutate(SimulationYear = SimulationYear - 1) %>%
  group_by(clim_scenario, mgmt_scenario) %>%
  slice_max(ha_burned, prop=0.5) %>%
  mutate(mean_ha_burned = mean(ha_burned), median_ha_burned=median(ha_burned), 
         mgmt_scenario = factor(mgmt_scenario, levels = c("No-Rx", "1x-Rx", "3x-Rx", "10x-Rx")))

# medians <- all_events %>%
#   mutate(SimulationYear = SimulationYear - 1) %>%
#   filter(SimulationYear > 0) %>%
#   mutate(group=cut(SimulationYear, breaks= seq(0, 50, by = 25))) %>%
#   group_by(group, clim_scenario, mgmt_scenario) %>%
#   slice_max(ha_burned, prop = 0.25) %>%
#   mutate(median_ha_burned=median(ha_burned)) %>%
#   select(group, clim_scenario, median_ha_burned) %>%
#   distinct
# write.csv(medians, paste0(out_dir, "median_area_burned.csv"))

for_plot %>%
  mutate(mgmt_scenario = factor(mgmt_scenario, levels = c("10x-Rx", "3x-Rx", "1x-Rx", "No-Rx")),
         clim_scenario = factor(clim_scenario, levels = c("Historical", "Warmer&drier", "Warmer&wetter"))) %>%
  ggplot(aes(x=ha_burned, y = mgmt_scenario, fill = mgmt_scenario), color = "black") +
  geom_density_ridges_gradient(aes(fill= mgmt_scenario), quantiles = 2, quantile_lines = TRUE, size = 0.25) +
  # scale_fill_gradientn(colors = met.brewer("OKeeffe2")) +
  scale_fill_manual(values = met.brewer("Navajo", 4)) +
  facet_grid(cols = vars(clim_scenario)) +
  xlab("Area burned (hectares)") +
  ylab("Density") +
  theme_minimal() +
  theme(legend.position = "none") 
ggsave(paste0(out_dir,"area_burned_density_plot_by_decade.pdf"), height = 3, width = 7.5)

for_plot %>%
  group_by(clim_scenario, mgmt_scenario) %>%
  summarize(median_ha= median(ha_burned))
           