library(tidyverse)

necn <- read.csv("C://Users/adeak/Desktop/LANDIS/single_cell_USE/NECN-succession-log.csv")
max_agb <- read.csv("C://Users/adeak/Dropbox (University of Oregon)/Research/agb_by_cohort.csv") %>%
  filter(LANDIS_NAME == "PseuMenz" & STDAGE > 10) %>%
  group_by(STDAGE) %>%
  summarise(max_agb = max(biomass))
plot(max_agb$max_agb~max_agb$STDAGE)
lm(max_agb$max_agb ~ max_agb$STDAGE)

analysis <- necn %>%
  select(Time, AGB) %>%
  left_join(max_agb, by = c(Time = "STDAGE")) %>%
  filter(LANDIS_NAME == "PseuMenz") %>%
  select(Time, AGB, max_agb)
analysis

plot(analysis$Time, analysis$AGB, ylim = c(0,30000))
point(analysis$Time, analysis$max_agb, col = "red")
abline(6320, -2.457)
