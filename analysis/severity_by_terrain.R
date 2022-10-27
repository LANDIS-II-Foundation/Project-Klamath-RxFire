library(terra)
library(tidyverse)
library(ggpubr)
library(patchwork)

wd <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/"

number_yrs = 51
options(scipen = 33333)
number_yrs <- 51

clim_region <- rast("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/ecoregions/spatial_data/ecoregions.tif") %>%
  as.data.frame() %>%
  rename("clim_region" = ecoregions)
mask <- rast("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/landscape/landscape_mask.tif")
oneyr_dnbr <- rast("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/Historical/rep-1/10x-Rx/files-for-use/fire-dnbr-2.img")
dem <- rast("/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/landscape/merged_dem.tif")
slope <- terrain(dem, v="slope", neighbors=8, unit="degrees") %>%
  project(mask) %>%
  crop(mask) %>%
  as.data.frame %>%
  mutate(slope = case_when(slope <= 20 ~ "< 20°", slope > 20 ~ "> 20°"))
terrain <- terrain(dem, v="aspect", neighbors=8, unit="degrees") %>%
  project(mask) %>%
  crop(mask) %>%
  as.data.frame %>%
  mutate(aspect = case_when(aspect >  45 & aspect <= 135 ~ "north",
                            aspect > 135 & aspect <= 225 ~ "west",
                            aspect > 225 & aspect <= 315 ~ "south",
                            aspect > 315 | aspect <=  45 ~ "east")) %>%
  cbind(slope)
head(terrain)

# i=1; j=1; k=2
all_cells_severity <- NULL
all_scen <- list.files(wd)
for (h in 1:length(all_scen)) {
  clim <- all_scen[[h]]
  clim_scenario <- list.files(paste0(wd, clim))
  for (i in 1: length(clim_scenario)) {
    rep <- clim_scenario[[i]]
    mgmt_scenario <- list.files(paste0(wd, clim, "/", rep))
    for (j in 1:length(mgmt_scenario)) {
      management <- mgmt_scenario[[j]]
      for(k in seq(2,number_yrs)) {
        oneyr_events <- rast(paste0(wd, clim, "/",rep,"/", management,"/files-for-use/ignition-type-", k,".img")) 
        oneyr_events[oneyr_events == 0 | oneyr_events == 1 | oneyr_events == 4] <- NA
        oneyr_dnbr <- rast(paste0(wd, clim, "/", rep, "/", management, "/files-for-use/fire-dnbr-", k, ".img"))
        oneyr_severity <- mask(oneyr_dnbr, oneyr_events) %>% 
          as.data.frame(na.rm = F) %>% 
          cbind(terrain) %>% 
          cbind(clim_region) %>%
          na.omit %>%
          mutate(clim_scenario = clim, rep = i, mgmt_scenario = management, year = k) %>%
          rename("severity" = Layer_1) %>%
          group_by(year, slope, aspect, clim_region, clim_scenario, mgmt_scenario) %>%
          summarize(mean_severity = mean(severity))
        all_cells_severity <- rbind(all_cells_severity, oneyr_severity)
      }
    }
  }
}

write.csv(all_cells_severity, "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/large_data_for_figs/severity_by_terrain.csv")
all_cells_severity <- read.csv("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/large_data_for_figs/severity_by_terrain.csv")

all_cells_severity <- read.csv("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/large_data_for_figs/severity_by_terrain.csv")
for_analysis <- all_cells_severity %>%
  mutate(year = year - 1) %>%
  mutate(group=ntile(year, 2)) %>%
  filter(clim_region != 2) %>%
  pivot_longer(cols = c(slope, aspect), names_to = "terrain", values_to = "value") %>%
  na.omit() %>%
  group_by(group, clim_scenario, mgmt_scenario, clim_region, value) %>%
  mutate(sd_severity = sd(mean_severity))
  summarise(mean_severity = mean(mean_severity)) %>%
  pivot_wider(names_from = group, names_prefix = "timeperiod_", values_from = mean_severity) %>%
  group_by(clim_scenario, mgmt_scenario, clim_region, value) %>%
  summarise(change_dnbr = timeperiod_2 - timeperiod_1) %>%
  filter(clim_region > 0 ) %>%
  mutate(mgmt_scenario = factor(mgmt_scenario, levels = rev(c("No-Rx", "1x-Rx", "3x-Rx", "10x-Rx"))),
         value = factor(value, level = c("< 20°", "> 20°", "north", "south")),
         clim_region = factor(clim_region)) %>%
  na.omit


for_analysis %>%
  ggplot(aes(x= change_dnbr, y = mgmt_scenario)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(aes(color = clim_region), size = 4, alpha = 0.7) +
  facet_grid(cols = vars(value), row = vars(clim_scenario)) +
  scale_color_manual(values = c("#e59a52", "#094568")) +
  xlim(c(-120,120)) +
  labs(x = "Change in dNBR", y = "Treatment scenario", color = "Climate region") +
  theme_minimal() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed")) 
  
ggsave(paste0(out_dir, "cleveland_dot_plot_terrain.pdf"), height = 6, width = 9)


