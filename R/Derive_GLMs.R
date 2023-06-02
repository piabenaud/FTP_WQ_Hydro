
# Title:      Derive GLMs
# Objective:  Generate GLMs for the before and after period to allow us to account for changes in rainfall before after
# Created by: Pia Benaud
# Created on: 10-05-2023

# Based on functions in: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/6_Event_Stats/Graham_etal_InReview_BudBrook_Mech.R
# and here: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/6_Event_Stats/mechanisms_paper_functions.R 
# (DOI: 10.5281/zenodo.6034308)

# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# library(lubridate)
# library(broom)
# library(glm2)


# The function ------------------------------------------------------------

Derive_GLMs <- function(event_metrics){
  
  # Original Additive regression model # like Puttock et al 2021, we're just going to run the one model
  BA_m1.init <- glm2(Q.peak.m3.s ~ rain.tot.mm + resto , 
                       data= event_metrics, family = Gamma(link='identity')) # prelim run to get starting vals
  
  BA_m1 <- glm2(Q.peak.m3.s ~  rain.tot.mm + resto , 
                  data= event_metrics, family = Gamma(link='identity'), 
                  start = coef(BA_m1.init)) # final model
  
  #summary(BA_m1)
  
  return(BA_m1)

}



