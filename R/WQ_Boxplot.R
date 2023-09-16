

# Title:      WQ Boxplots
# Objective:  Plot Water Quality metrics
# Created by: Pia Benaud
# Created on: 19-05-2023


# Load packages -----------------------------------------------------------

 # library(tidyverse) # wranglings
# 
 # summary_stats <- WQ_Q_event_stats

# The function ------------------------------------------------------------

WQ_Boxplots <- function(summary_stats){
  
  metric_order <- c("FWMC_DOC", "DOC_Load_ha", "FWMC_Abs400", "FWMC_C_C", "FWMC_SUVA")
  
  metric_labels <- c(expression(atop(a,DOC~(mg~L^{-1}))), 
                     expression(atop(b,DOC~Load~(kg~ha^{-1}))),
                     expression(atop(c,Colour~(Au~m^{-1}))), 
                     expression(atop(d,paste("Colour"~paste("/Carbon")))), 
                     expression(atop(e,SUVA)))
  
  the_plot <- summary_stats %>% 
    filter(metric %in% c("FWMC_DOC", "DOC_Load_ha", "FWMC_Abs400", "FWMC_C_C", "FWMC_SUVA")) %>% 
    mutate(resto = fct_relevel(resto, "Before", "After")) %>% 
    mutate(metric = factor(metric, levels = metric_order, labels = metric_labels)) %>% 
    # arrange(metric)
    ggplot() +
    geom_boxplot(aes(x = resto, y = mean, fill = resto))+
    geom_jitter(aes(x = resto, y = mean, fill = resto), alpha = 0.5) +
    facet_wrap(vars(metric), scales = "free", nrow = 1, labeller = label_parsed) +
    scale_fill_manual(values = c("Before" = "#ca0020", "After" = "#0571b0"))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(legend.position = "none")+
    theme(axis.title = element_blank())
  
}




  