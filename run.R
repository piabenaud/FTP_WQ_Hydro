
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
tiff("figures/Figure_2_Flow.tiff", width = 18, height = 12, units = 'cm', res = 300)
Hydro_Plot
dev.off()

summary(Emmeans)
summary(The_GLM)
GAM_Summary
Flow_Stats

tiff("figures/Figure_7_WQ_Boxplot.tiff", width = 18, height = 7, units = 'cm', res = 300)
WQ_Box_Plot
dev.off()


WQ_Q_event_stats %>% 
  filter(metric == "DOC_Load_ha") %>% 
  rename(result = mean) %>% 
  group_by(resto) %>% 
  summarise(mean = mean(result, na.rm = T),
            median = median(result, na.rm = T),
            IQR = IQR(result, na.rm = T),
            sd = sd(result, na.rm = T),
            se = sd/sqrt(n()),
            min = min(result, na.rm = T),
            max = max(result, na.rm = T))


