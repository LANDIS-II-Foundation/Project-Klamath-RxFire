library(tidyverse)
setwd("/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/")


DRY <- read.csv("Warmer&drier/rep-2/10x-Rx/Climate-future-input-log.csv")
head(DRY)

summary <- DRY %>%
  group_by(Year, EcoregionName) %>%
  summarise(sum_precip = sum(ppt), 
            mean_min_temp = mean(min_airtemp), 
            mean_max_temp = mean(max_airtemp))

mean_DRY_20_24 <- summary %>%
  group_by(Year, EcoregionName) %>%
  filter(Year %in% 2020:2024) %>%
  group_by(EcoregionName) %>%
  summarise(mean_precip_start = mean(sum_precip), 
            mean_min_temp_start = mean(mean_min_temp), 
            mean_max_temp_start = mean(mean_max_temp))
mean_DRY_20_24

mean_DRY <- summary %>%
  group_by(Year, EcoregionName) %>%
  filter(Year %in% 2066:2070) %>%
  group_by(EcoregionName) %>%
  summarise(mean_precip_end = mean(sum_precip),
            mean_min_temp_end = mean(mean_min_temp), 
            mean_max_temp_end = mean(mean_max_temp)) %>%
  mutate(clim_scenario = "dry") %>%
  left_join(mean_DRY_20_24)
mean_DRY



WET <- read.csv("Warmer&wetter/rep-2/3x-Rx/Climate-future-input-log.csv")
head(WET)

summary <- WET %>%
  group_by(Year, EcoregionName) %>%
  summarise(sum_precip = sum(ppt), 
            mean_min_temp = mean(min_airtemp), 
            mean_max_temp = mean(max_airtemp))

mean_WET_20_24 <- summary %>%
  group_by(Year, EcoregionName) %>%
  filter(Year %in% 2020:2024) %>%
  group_by(EcoregionName) %>%
  summarise(mean_precip_start = mean(sum_precip), 
            mean_min_temp_start = mean(mean_min_temp), 
            mean_max_temp_start = mean(mean_max_temp)) %>%
  mutate(clim_scenario = "wet")
mean_WET_20_24

mean_WET <- summary %>%
  group_by(Year, EcoregionName) %>%
  filter(Year %in% 2066:2070) %>%
  group_by(EcoregionName) %>%
  summarise(mean_precip_end = mean(sum_precip),
            mean_min_temp_end = mean(mean_min_temp), 
            mean_max_temp_end = mean(mean_max_temp)) %>%
  left_join(mean_WET_20_24)
mean_WET


historical <- read.csv("Historical/rep-1/1x-Rx/Climate-future-input-log.csv")
summary <- historical %>%
  group_by(Year, EcoregionName) %>%
  summarise(sum_precip = sum(ppt), 
            mean_min_temp = mean(min_airtemp), 
            mean_max_temp = mean(max_airtemp)) %>%
  group_by(EcoregionName) %>%
  summarise(mean_precip = mean(sum_precip), 
            mean_min_temp = mean(mean_min_temp), 
            mean_max_temp = mean(mean_max_temp)) %>%
  mutate(clim_scenario = "historical", years = "all")

all_clim_avgs <- rbind(mean_WET, mean_DRY) %>%
  group_by(EcoregionName, clim_scenario) %>%
  mutate(change_precip = mean_precip_end - mean_precip_start,
         change_max_temp = mean_max_temp_end - mean_max_temp_start,
         change_min_temp = mean_min_temp_end - mean_min_temp_start)
all_clim_avgs

write.csv(all_clim_avgs, "/Users/alisondeak/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Landis-Siskiyous/thesis_writing/climate_change_averages.csv")


