
# Title:      Targets Run File
# Created by: Pia Benaud
# Created on: 05-05-2023


# Load packages -----------------------------------------------------------

library(targets)

tar_manifest()

tar_visnetwork()

targets::tar_make()


# Pull out the stuff for the paper ----------------------------------------

library(patchwork)
library(tidyverse)

tar_load_everything()

((GAM_Plots) | (GLM_Plot + plot_layout(guides = "collect"))) +
  plot_annotation(tag_levels = "a")+
  plot_layout(ncol = 2)&
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0, unit = "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(color="gray20", ))


 # hydro plot
# tiff("/Users/piabenaud/Library/CloudStorage/OneDrive-UniversityofExeter/NfCPGS/Papers/FTP - Short-term WQ and Hydro/Figures/Figure_2_Flow.tiff", width = 18, height = 12, units = 'cm', res = 300) 
# Hydro_Plot
# dev.off() 

summary(Emmeans)
summary(The_GLM)
GAM_Summary
Flow_Stats

tiff("/Users/piabenaud/Library/CloudStorage/OneDrive-UniversityofExeter/NfCPGS/Papers/FTP - Short-term WQ and Hydro/Figures/Figure_7_WQ_Boxplot.tiff", width = 18, height = 6, units = 'cm', res = 300)
WQ_Box_Plot
dev.off()

WQ_Q_event_stats %>% 
  group_by(resto, metric) %>% 
  summarise()


