library(tidyverse)
library(raster)

severity_reclass <- read.csv("/Volumes/Alison.Deak_440-223-4897/MS Research/SCRPPLE/calibration/data/severity_reclass_matrix.csv")[,-1] %>% as.matrix()
severity_92to17 <- read.csv("/Volumes/Alison.Deak_440-223-4897/MS Research/SCRPPLE/calibration/output/mtbs_severity_1992_2017.csv")[,-1]
events <- read.csv("/Volumes/Alison.Deak_440-223-4897/model_analysis/2022-05-21/d/scrapple-events-log.csv") %>%
  dplyr::filter(SimulationYear > 1 & IgnitionType != " Rx")

## classify severity of simulated events
severity_simulated <- events %>%
  dplyr::select(EventID, SimulationYear, MeanDNBR) %>%
  mutate(severity = case_when(MeanDNBR >= 0 & MeanDNBR <= 200 ~ 1,
                              MeanDNBR > 200 & MeanDNBR <= 439 ~ 2,
                              MeanDNBR > 439 & MeanDNBR <= 2000 ~ 3),
         Name = "Simulated") %>%
  dplyr::rename("year" = SimulationYear) %>%
  dplyr::select(severity, year, Name)


## calculate and classify severity based on events file
eqn <- function(b0, b2, b3, b4, Clay, ET, EFW, CWD, FineFuels, LadderFuels) {
  (1/(b0 + (0.0002638*Clay) + (b2*ET) + (b3*EFW) + (b4*CWD) + (0.00358100*FineFuels) + (0.00000064*LadderFuels)))
}

b_0 = -0.011   # -0.0095
b_2 = 0.00000
b_3 = 0.00105 # 0.001
b_4 = 0.0000066  # 0.0000055; up = lowered severity

events$severity <- (eqn(b0 = b_0, b2 = b_2, b3 = b_3, b4 = b_4, Clay = events$MeanClay,
                        ET = events$MeanPET, EFW = events$MeanEffectiveWindSpeed,
                        CWD = events$MeanWD, FineFuels = events$MeanFineFuels,
                        LadderFuels = events$MeanLadderFuels))

# events$severity <- (16.70 + 1.84 * eqn(b0 = b_0, b2 = b_2, b3 = b_3, b4 = b_4, Clay = events$MeanClay,
#                                           ET = events$MeanPET, EFW = events$MeanEffectiveWindSpeed,
#                                           CWD = events$MeanWD, FineFuels = events$MeanFineFuels,
#                                           LadderFuels = events$MeanLadderFuels))
events <- events %>% filter(severity > 0 & severity <=2000)
# range(events$severity)
# range(events$MeanDNBR)
# cor(events$severity, events$MeanDNBR) # correlation coefficient = 0.98
# lm(MeanDNBR ~ severity, events) # equation: severity = 44.1624 + (0.4279 * severity)
# events <- events %>%
#   mutate(severity = (-80.512 + 2.354 * severity))
# plot(events$severity ~ events$MeanDNBR)
# abline(1,1)

severity_calced <- events %>%
  dplyr::select(severity, SimulationYear) %>%
  mutate(severity = case_when(severity >= 0 & severity <= severity_reclass[1,2] ~ severity_reclass[1,3],
                              severity > severity_reclass[1,2] & severity <= severity_reclass[2,2] ~ severity_reclass[2,3],
                              severity >= severity_reclass[2,2] & severity <= 2000 ~ severity_reclass[3,3]),
         Name = "Calculated") %>%
  dplyr::rename("year" = SimulationYear) %>%
  dplyr::select(severity, year, Name)

# fire severity distribution
compare_severity_92to17 <- rbind(severity_simulated, severity_92to17, severity_calced)
severity_summ <- data.frame(with(compare_severity_92to17, table(severity, Name)))
sev_summ_hist <- severity_summ %>% filter(Name=="Historical") %>% mutate(Percent = Freq/sum(Freq)*100)
sev_summ_sim <- severity_summ %>% filter(Name=="Simulated") %>% mutate(Percent = Freq/sum(Freq)*100)
sev_summ_calc <- severity_summ %>% filter(Name=="Calculated") %>% mutate(Percent = Freq/sum(Freq)*100)
severity_summ <- rbind(sev_summ_hist, sev_summ_sim, sev_summ_calc)
severity_summ

## PLOT
ggplot(severity_summ, aes(x=Name, y=Percent, fill=severity)) +
  geom_bar(color="#e9ecef", position='fill', stat='identity') +
  ggtitle("Fire Severity Distribution") +
  scale_fill_manual(values=c("cornsilk", "orange", "maroon")) +
  ylab("Severity Proportions") +
  theme_classic()
ggsave("/Volumes/Alison.Deak_440-223-4897/model_analysis/2022-05-21/b/severity-plot.png")

