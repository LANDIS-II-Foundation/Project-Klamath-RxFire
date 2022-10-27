library(tidyverse)
library(terra)
library(ggpubr)
##-----------------------------
## H2. Severity of wildland fires occurring on the study landscape, as measured by differenced 
## Normalized Burn Ratio (dNBR), will decrease with increased prescribed fire frequency and 
## extent in all climate scenarios. 
##-----------------------------

wd <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/MODEL_OUTPUT/"
out_dir <- "/Users/alisondeak/Dropbox (University of Oregon)/Research/THESIS_ANALYSIS/FIGURES/"

cols <- c("#660d20", "#e59a52", "#edce79")

options(scipen = 33333)
ha_per_cell <- 0.09
number_yrs <- 51
n_cells <- 8828190

##-----------------------------
## DOT PLOT OF SEVERITY BY MEAN SEVERITY
##-----------------------------
all_mean_severities <- NULL
all_scen <- list.files(wd)
# h=1; i=1; j=1
for (h in 1:length(all_scen)) {
  clim <- all_scen[[h]]
  clim_scenario <- list.files(paste0(wd, clim))
  for (i in 1: length(clim_scenario)) {
    rep <- clim_scenario[[i]]
    mgmt_scenario <- list.files(paste0(wd, clim, "/", rep))
    for (j in 1:length(mgmt_scenario)) {
      management <- mgmt_scenario[[j]]
      severities <- read.csv(paste0(wd, clim, "/",rep,"/", management,"/scrapple-events-log.csv")) %>%
        filter(SimulationYear > 1 & IgnitionType == " Accidental" | IgnitionType == " Lightning") %>%
        select(SimulationYear, MeanDNBR) %>%
        mutate(clim_scenario = clim, mgmt_scenario = management, year = j)
      all_mean_severities <- rbind(all_mean_severities, severities)
    }
  }
}

mean_severity <- all_mean_severities %>%
  mutate(SimulationYear = SimulationYear - 1) %>%
  filter(SimulationYear > 0) %>%
  group_by(clim_scenario, mgmt_scenario) %>%
  # mutate(group=cut(SimulationYear, breaks= seq(0, 50, by = 10))) %>%
  group_by(clim_scenario, mgmt_scenario, SimulationYear) %>%
  summarize(mean_severity = mean(MeanDNBR), 
            median_severity = median(MeanDNBR), 
            sd_severity = sd(MeanDNBR)) %>%
  mutate(severity = case_when(mean_severity >= 0 & mean_severity <= 200 ~ "Low_severity", 
                              mean_severity > 200 & mean_severity <= 439 ~ "Moderate_severity", 
                              mean_severity > 439 & mean_severity <= 2000 ~ "High_severity"))

mean_severity %>%
  mutate(mgmt_scenario = factor(mgmt_scenario, levels = c("No-Rx", "1x-Rx", "3x-Rx", "10x-Rx"))) %>%
  ggscatter(x ="SimulationYear", y = "mean_severity", merge = T, shape = 21, fill = "severity",
            palette = c(High_severity = "#660d20", Moderate_severity = "#e59a52"), 
            add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.1 = 3, label.y = 175, size = 3.5) +
  stat_regline_equation(label.x = 1, label.y = 100, size = 3.5) +
  # annotate("rect", ymin=-Inf,ymax=200, xmin=-Inf,xmax=Inf,fill=cols[[3]], alpha = .5) +
  # annotate("rect", ymin=200,ymax=439, xmin=-Inf,xmax=Inf,fill=cols[[2]], alpha = .5) +
  # annotate("rect", ymin=439,ymax=Inf, xmin=-Inf,xmax=Inf,fill=cols[[1]], alpha = .5) +
  facet_grid(cols = vars(mgmt_scenario), rows = vars(clim_scenario)) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(paste0(out_dir, "severity_regression_plot.pdf"), width = 7.5, height = 5)

##-----------------------------
## STACKED BAR CHART OF SEVERITY BY PROPORTION OF TOTAL CELLS BURNED
##-----------------------------
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
        oneyr_mask <- mask(oneyr_dnbr, oneyr_events) %>% as.data.frame() %>% na.omit
        names(oneyr_mask) <- "oneyr_mask"
        oneyr_severity <- oneyr_mask %>%
          mutate(severity = case_when(oneyr_mask >= 0 & oneyr_mask <= 200 ~ "Low severity", 
                                      oneyr_mask > 200 & oneyr_mask <= 439 ~ "Moderate severity", 
                                      oneyr_mask > 439 & oneyr_mask <= 2000 ~ "High severity")) %>%        
          group_by(severity) %>%
          summarise(n = n()) %>%
          mutate(clim_scenario = clim, rep = i, mgmt_scenario = management, year = k, ha_burned = n*ha_per_cell)
        all_cells_severity <- rbind(all_cells_severity, oneyr_severity)
      }
    }
  }
}

all_sims_severity <- all_cells_severity %>%
  mutate(year = year - 1) %>%
  group_by(clim_scenario, mgmt_scenario, severity, group=cut(year, breaks= seq(0, 50, by = 25))) %>%
  summarise(sum_ha_burned = sum(ha_burned)) %>%
  na.omit %>%
  group_by(clim_scenario, mgmt_scenario, group) %>%
  mutate(percent = ceiling(sum_ha_burned/sum(sum_ha_burned) * 100))
all_sims_severity

all_sims_severity %>%
  mutate(severity = factor(severity, levels = c("High severity", "Moderate severity", "Low severity")),
         mgmt_scenario = factor(mgmt_scenario, levels = c("No-Rx", "1x-Rx", "3x-Rx", "10x-Rx"))) %>%
  ggplot(aes(x=group, y=sum_ha_burned, fill=severity)) + 
  geom_bar(position = "fill", stat = "identity", alpha = 70, color = "white") +
  geom_text(aes(label = paste0(percent, "%")), position=position_fill(vjust=0.5), color = "white") +
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
  guides(fill=guide_legend(title="Severity")) +
  theme_light() +
  labs(title="Area of wildland fires by severity and management scenario", 
       y="Proportion of area burned", x = "Simulation period") +
  facet_grid(cols = vars(mgmt_scenario), rows = vars(clim_scenario))
ggsave(paste0(out_dir,"severity_by_clim_scenario_stackedbarplot.pdf"))

##-----------------------------
## AREA BURNED BY SEVERITY 
##-----------------------------
all_sims_severity <- all_cells_severity %>%
  mutate(year = year - 1) %>%
  group_by(clim_scenario, mgmt_scenario, year, severity) %>%
  summarise(mean_ha_burned = mean(ha_burned)) %>%
  group_by(clim_scenario, mgmt_scenario, severity, group=cut(year, breaks= seq(0, 50, by = 10))) %>% 
  summarise(ha_burned = sum(mean_ha_burned)) %>%
  na.omit %>%
  group_by(clim_scenario, mgmt_scenario, group) %>%
  mutate(percent = ceiling(ha_burned/sum(ha_burned) * 100))

all_sims_severity %>%
  mutate(severity = factor(severity, levels = c("High severity", "Moderate severity", "Low severity")),
         mgmt_scenario = factor(mgmt_scenario, levels = c("No-Rx", "1x-Rx", "3x-Rx", "10x-Rx"))) %>%
  ggplot(aes(x=group, y=ha_burned, fill=severity)) + 
  geom_col(position = position_stack(), alpha = 70) +
  geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = .5)) +
  theme(plot.title = element_text(face="bold")) + 
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
  guides(fill=guide_legend(title="severity")) +
  theme_bw() +
  labs(title="Area of wildland fires by severity and management scenario\n(with annual burned area averaged across reps)", 
       y="Area burned (hectares)", x = "Simulation period") +
  facet_grid(cols = vars(mgmt_scenario), rows = vars(clim_scenario))
ggsave(paste0(out_dir,"severity_by_clim_scenario_stackedbarplot.pdf"))

##-----------------------------
## CUMULATIVE AREA BURNED
##-----------------------------
# ## LINE PLOTS OF AREA BURNED THROUGH TIME
all_cells_severity %>%
  group_by(mgmt_scenario, clim_scenario, year) %>%
  summarise(annual_ha_burned = mean(ha_burned)) %>%
  select(clim_scenario, mgmt_scenario, year, annual_ha_burned) %>%
  group_by(mgmt_scenario, clim_scenario) %>%
  arrange(year, .by_group = T) %>%
  mutate(annual_ha_burned = cumsum(annual_ha_burned)) %>%
  ggplot(aes(x=year, y=annual_ha_burned, color=mgmt_scenario)) +
  geom_line() +
  facet_grid(cols = vars(clim_scenario)) +
  theme_light()

