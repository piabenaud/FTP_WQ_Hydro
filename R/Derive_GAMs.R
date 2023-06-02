
# Title:      Derive GAMs
# Objective:  Generate GAM for GAM hydrograph plotting
# Created by: Pia Benaud
# Created on: 05-05-2023

# Based on functions in: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/8_event_overlay/Event_overlay.R
# (DOI: 10.5281/zenodo.6034308)

# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# library(broom) # for summarising modelling outputs
# library(gam) # self explanatory.. 


# The functions -----------------------------------------------------------

Derive_GAMs <- function(events_combined, perc_cutoff = 0.95) {
  
  # function to fit gam and return full dataframe
  gam_func <- function(.data) {
    
    gamA <- mgcv::gam(q_m3_s ~ s(event_step, by=resto, bs='cs', k=5) + resto,
                      method = "REML", data = .data, family=Gamma(link='log')) 
    gam1 <- gamA %>%
      broom::augment(type.predict='response') %>%
      rename_with(., starts_with("."), .fn = ~(paste0("gam", .)))
    
    gamB <- mgcv::gam(rainfall_mm_h ~ s(event_step, by=resto, bs='cs', k=-1) + resto,
                      method = "REML", data = .data,family=gaussian(link='identity')) 
    gam2 <- gamB %>%
      broom::augment(type.predict='response') %>%
      select(-c(rainfall_mm_h, resto, event_step)) %>%
      rename_all(., ~paste0(., "_rain"))
    
    df <- gam1 %>%
      bind_cols(select(.data, event_id, tot_rain, s_flow, rainfall_mm_h), ., gam2) 
    
    return(list(data=df, flowGAM = gamA, rainGAM=gamB))
    
  }
  
  
  fit_gams <- function(events_combined, perc_cutoff = 0.95){
    
    maxhrs <- quantile(events_combined$event_step[events_combined$resto == 'After'], probs = c(perc_cutoff))
    
    events_combined %>%
      filter(event_step < maxhrs) %>%
      gam_func(.) 
    
  }
  
  out <- fit_gams(events_combined, perc_cutoff = 0.95)
  
}




