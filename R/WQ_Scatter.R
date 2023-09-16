

# Title:      WQ Scatter plots
# Objective:  Plot Water DOC vs Flow data
# Created by: Pia Benaud
# Created on: 19-05-2023


# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# 
# summary_stats <- WQ_Q_event_stats
# all_wq_data <- WQ_Q_Data_All

# The function ------------------------------------------------------------

WQ_Scatter <- function(summary_stats, all_wq_data){
  
  total_q <- summary_stats %>% 
    ungroup() %>% 
    filter(metric == "q_m3_s") %>% 
    select(eventID, total_q, resto)
  
  total_r <- summary_stats %>% 
    ungroup() %>% 
    filter(metric == "rainfall_mm_h") %>% 
    select(eventID, total_rain, resto)
  
  conc_c <- summary_stats %>% 
    ungroup() %>% 
    filter(metric == "FWMC_DOC") %>% 
    select(eventID, mean, resto) %>% 
    rename(DOC = mean)
  
  total_load <- summary_stats %>% 
    ungroup() %>% 
    filter(metric == "DOC_Load_kg") %>% 
    select(eventID, mean, resto) %>% 
    rename(load = mean)
  
  join <- left_join(total_load, total_r) %>% 
    left_join(., total_q) %>% 
    left_join(., conc_c) %>% 
    select(eventID, resto, total_rain, total_q, DOC, load) %>% 
    mutate(norm_load = load/total_rain) %>% 
    mutate(norm_discharge = total_q/total_rain) %>% 
    mutate(resto = fct_relevel(resto, "Before", "After"))
  

  
 q_only <-  ggplot(join) +
    geom_point(aes(x = total_q, y = DOC, colour = resto)) +
    scale_colour_manual(values = c("Before" = "#ca0020", "After" = "#0571b0")) +
   labs(x = expression(Total~flow~(m^{3})), y = expression(DOC~(mg~L^{-1})), colour = "Restoration Status") +
   theme_bw()+
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
     theme(legend.position = "none")
  
  Q_R <- ggplot(join) +
    geom_point(aes(x = norm_discharge, y = DOC, colour = resto)) +
    scale_colour_manual(values = c("Before" = "#ca0020", "After" = "#0571b0")) +
    labs(x = expression(Total~flow~(m^{3})~"/"~Total~rainfall~(mm)), y = expression(DOC~(mg~L^{-1})), colour = "Restoration Status") +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  

   
  
  out <- q_only + Q_R +
  plot_annotation(tag_levels = "a")+
    plot_layout(guides = "collect")&
    theme(legend.position = "bottom",
    legend.margin = margin(t = 0, unit = "cm"),
  legend.key.size = unit(0.5, "cm"),
  legend.text = element_text(color="gray20", ))
  
  tiff("figures/Figure_8_WQ_Flow.tiff", width = 18, height = 7, units = 'cm', res = 300)
  out
  dev.off()
  
  return(out)
  
}




