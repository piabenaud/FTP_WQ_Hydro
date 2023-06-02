
# Title:      Calibrate DOC
# Objective:  Use the measured DOC observations to calibrate the spectroscopy data
# Created by: Pia Benaud
# Created on: 11-05-2023

# need to calibrate the DOC values from the Trios against the measured checks from SWW and our NPOC
# a different LSA was used from 2017, so need to use calibration data from that time
# assumption is the relationship remains consistent over time.

# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# library(lubridate) # working with dates and time

# Quick look at the data --------------------------------------------------
# flow_wq_data <- WQ_Q_Data
# 
# # quick look at the data
# ggplot() + # is there a consistent relationship between the trios and the check
#   geom_point(data = flow_wq_data, aes(DOC_sww, DOC_trios, colour = as.factor(year(datetime))), alpha = 0.8) +
#   geom_point(data = flow_wq_data, aes(DOC_NPOC, DOC_trios, colour = as.factor(year(datetime))), alpha = 0.8) +
#   geom_smooth(data = flow_wq_data, aes(DOC_sww, DOC_trios), method ="lm", colour = "blue", linetype = "dashed", size = 0.5) +
#   geom_smooth(data = flow_wq_data, aes(DOC_NPOC, DOC_trios), method ="lm", colour = "green", linetype = "dashed", size = 0.5) +
#   geom_abline(colour = "#6B8B8D", size = 0.5) + #1to1 line
#   theme_classic() +
#   theme(legend.position = "bottom") 
# 
# ggplot() +  # should be a fairly consistent relationship between DOC and Colour (Abs400)
#   geom_point(data = flow_wq_data, aes(Au400nm, DOC_trios, colour = as.factor(year(datetime)))) +
#   theme_classic() +
#   theme(legend.position = "bottom") 


# The function ------------------------------------------------------------

Calibrate_DOC <- function(.data){
  
  lm_out1 <- lm(DOC_sww ~ DOC_trios, data = .data) # determine linear model
  a <- lm_out1$coefficients[1] # y intercept
  b <- lm_out1$coefficients[2] # slope
  
  lm_out2 <- lm(DOC_NPOC ~ DOC_trios, data = .data) 
  c <- lm_out2$coefficients[1] # y intercept
  d <- lm_out2$coefficients[2] # slope
  
  out <- .data %>% 
    mutate(year = year(datetime)) %>% # temp addition
    mutate(DOC_calibrated = if_else(year < 2017,
                                    a + (b * DOC_trios),
                                    c + (d * DOC_trios)), .after = "DOC_NPOC") %>% 
    select(-year)
  
  sum1 <- tidy(lm_out1) %>% 
    filter(term != "(Intercept)") %>% 
    select(std.error) %>% 
    bind_cols(., glance(lm_out1)) %>% 
    mutate(method = "DOC_sww", .before = 1)
  
  sum2 <- tidy(lm_out2) %>% 
    filter(term != "(Intercept)") %>% 
    select(std.error) %>% 
    bind_cols(., glance(lm_out2)) %>% 
    mutate(method = "DOC_NPOC", .before = 1)
  
  write_csv(bind_rows(sum1, sum2), "exports/DOC_calibration_stats.csv")
  
  return(out)
  
}


# Another quick look ------------------------------------------------------

# test <- Calibrate_DOC(flow_wq_data)
# 
# ggplot() +  # should be a fairly consistent relationship between DOC and Colour (Abs400)
#   geom_point(data = test, aes(Au400nm, DOC_calibrated, colour = as.factor(year(datetime)))) +
#   theme_classic() +
#   theme(legend.position = "bottom") 

