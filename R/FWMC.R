
# Title:      FWMC
# Objective:  Calculate the Flow Weighted Mean Concentrations for colour and DOC
# Created by: Pia Benaud
# Created on: 16-05-2023

# FWMC based on equation in Grand-Clement et al (2014), which is based on Dinsmore et al. (2013) and is calculated for each event.
# 
# FWMC = Sum (Ci * ti * qi) / Sum (ti * Qi)  
# 
# Where ci is the instantaneous concentration, 
# qi is the instantaneous discharge and 
# ti is the time step between subsequent concentration measurements.

# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# library(lubridate) # working with dates and time
# 
# 
# # The Function ------------------------------------------------------------
# targets::tar_load(WQ_Q_Data)
# wq_flow_data <- WQ_Q_Data

FWMC <- function(wq_flow_data){
  
  split_data <- wq_flow_data %>% 
    group_by(eventID) %>% 
    group_split()

  indv_FWMC <- function(the_data){
    
    out <- the_data %>% 
      filter(sample == TRUE) %>% 
      mutate("event_end" = last(datetime)) %>% # needed for timestep calculation
      mutate("timestep" = case_when( !is.na((lead(datetime) - datetime)/dminutes(1))# calculate time between each sample (final -> sample and end of event)
                                     ~ (lead(datetime) - datetime)/dminutes(1),
                                     TRUE ~ (event_end - datetime)/dminutes(1))) %>% ## important to use lubridate::dminutes(1) here, otherwise defaults to sec, min, hour depending on duration 
      mutate(FWMC_DOC = (sum(DOC_calibrated * timestep * q_m3_s))/(sum(timestep*q_m3_s)),  # i.e.  Sum (Ci * ti * qi) / Sum (ti * Qi) ))
             FWMC_Abs400 = (sum(Abs400 * timestep * q_m3_s))/(sum(timestep*q_m3_s)),
             FWMC_C_C = (sum(C_C * timestep * q_m3_s))/(sum(timestep*q_m3_s)),
             FWMC_SUVA = (sum(SUVA * timestep * q_m3_s))/(sum(timestep*q_m3_s))) %>% 
      select(datetime, FWMC_DOC, FWMC_Abs400, FWMC_C_C, FWMC_SUVA)
    
  }
 
  # map through function
    all_FWMC <- map(split_data, ~indv_FWMC(.)) %>% 
  bind_rows()
  
    # bind with original data
  out <- left_join(wq_flow_data, all_FWMC, by = "datetime")
  
}