
# Title:      Derive GAM summary stats
# Objective:  Use GAM outputs to derive summary stats
# Created by: Pia Benaud
# Created on: 05-05-2023

# Based on functions in: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/8_event_overlay/Event_overlay.R
# and https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/8_event_overlay/Run_overlay.R

# (DOI: 10.5281/zenodo.6034308)


# Load packages -----------------------------------------------------------

#library(tidyverse)


# The Function ------------------------------------------------------------

Summarise_GAM <- function(the_data){
  
  PeakQ.df <- the_data$data %>%
    mutate(resto = fct_relevel(resto, "Before", "After")) %>%  # order logically
    mutate(gam.LowCI = gam.fitted-(gam.se.fit*1.96),
           gam.HighCI = gam.fitted+(gam.se.fit*1.96)) %>%
    group_by(resto) %>%
    summarise(PredQMax = max(gam.fitted),
              PredQMaxTime = event_step[which.max(gam.fitted)],
              PredPrecMax = max(.fitted_rain ),
              PredPrecMaxTime = event_step[which.max(.fitted_rain)],
              lagTime = PredQMaxTime - PredPrecMaxTime,
              RL_gradientAvg = (max(gam.fitted) - gam.fitted[which.max(.fitted_rain)])/lagTime,
              RL_gradientLow = (gam.LowCI[which.max(gam.fitted)] - gam.HighCI[which.max(.fitted_rain)])/lagTime,
              RL_gradientHigh = (gam.HighCI[which.max(gam.fitted)] - gam.LowCI[which.max(.fitted_rain)])/lagTime) %>%
    
    mutate(height = case_when(resto == "After" ~ 0.04,
                              TRUE ~ 0.1),
           .label = "Lag time (Peak rain to Peak Q)")
  
  
  csv_out <- PeakQ.df %>%
    summarise(lagChange = (lagTime[resto=='After']-lagTime[resto=='Before'])/
                lagTime[resto=='Before'] *100,
              lagGradientChangeAvg = (RL_gradientAvg[resto=='After']-RL_gradientAvg[resto=='Before'])/
                RL_gradientAvg[resto=='Before'] *100,
              lagGradientChangeLow = (RL_gradientLow[resto=='After']-RL_gradientLow[resto=='Before'])/
                RL_gradientLow[resto=='Before'] *100,
              lagGradientChangeHigh = (RL_gradientHigh[resto=='After']-RL_gradientHigh[resto=='Before'])/
                RL_gradientHigh[resto=='Before'] *100)
  
  write_csv(csv_out, "exports/GAM_Lag_Summary.csv")
  
  return(PeakQ.df)
  
}


