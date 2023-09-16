
# Title:      Derive Additional WQ Parameters
# Objective:  Derive additional WQ parameters using the existing water quality data
# Created by: Pia Benaud
# Created on: 11-05-2023


# Load packages -----------------------------------------------------------

#library(tidyverse) # wranglings


# The function ------------------------------------------------------------


Additional_WQ <- function(.data) {

    .data %>% 
      mutate(Abs254 = Au254nm / 4 * 100,    # convert Abs nm to standard absorbance units of Abs m-1 (we used a 4cm cuvette)
             Abs400 = Au400nm / 4 * 100,
             Abs465 = Au465nm / 4 * 100,
             Abs665 = Au665nm / 4 * 100) %>% 
      select(-c(Au254nm, Au400nm, Au465nm, Au665nm)) %>% 
      mutate(SUVA = Abs254 / DOC_calibrated,
             C_C = Abs400 / DOC_calibrated) 
  }

  

