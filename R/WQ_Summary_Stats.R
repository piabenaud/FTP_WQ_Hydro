# Title:      Summary Stats
# Objective:  Derive all the summary stats needed for the analysis
# Created by: Pia Benaud
# Created on: 17-05-2023


# # Load packages -----------------------------------------------------------
# 
# library(tidyverse) # wranglings
# 
#targets::tar_load(WQ_Q_Data_All)
#wq_flow_data <- WQ_Q_Data_All


WQ_Summary_Stats <- function(wq_flow_data){
  
  WQ_out <- wq_flow_data %>% 
    filter(sample == TRUE) %>% 
    select(datetime, eventID, resto,
           DOC_calibrated, DOC_Load_inst, DOC_Load_kg, FWMC_DOC,
           Abs400, FWMC_Abs400, C_C, FWMC_C_C, SUVA, Abs254, FWMC_SUVA) %>% 
    pivot_longer(., cols = 4:14, names_to = "metric", values_to = "result") %>% 
    group_by(metric, resto) %>%  
    summarise(mean = mean(result, na.rm = T),
              median = median(result, na.rm = T),
              IQR = IQR(result, na.rm = T),
              sd = sd(result, na.rm = T),
              se = sd/sqrt(n()),
              min = min(result, na.rm = T),
              max = max(result, na.rm = T))
  
}