
# Title:      Data Setup For H1 + H2
# Objective:  Import and tidy the flow, water quality and event separation data
# Created by: Pia Benaud
# Created on: 2022-03-16


# Load packages -----------------------------------------------------------

library(tidyverse) # readr, dplyr, ggplot2, purrr, stringr, tidyr, tibble
library(lubridate) # working with dates and time

#rm(list = ls()) # if you need a quick clean of the environment - DOES NOT UNLOAD PACKAGES!


# Bring over the Event Extraction data ------------------------------------
 # Only need to run once, should change to an if exists...

# read_csv("00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/eventEx_OUTPUTS_timeseries.csv") %>% 
#   write_csv(., "data/04_Extracted_Events.csv")
# 
# read_csv("00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/eventEx_EVENTS_metrics.csv") %>% 
#   write_csv(., "data/05_Extracted_Events_Metrics.csv")


# Import the data ---------------------------------------------------------

flow_data <- read_csv("data/04_Extracted_Events.csv")

wq_data <- read_csv("data/01_WQ_Flume_PrePost.csv")


# Initial Tidy ------------------------------------------------------------

tidy_flow_1 <- function(.data){
  .data %>% 
    select(datetime,
           q_m3_s,
           rainfall_mm_h,
           baseflow_m3_s,
           stormflow_m3_s,
           stormquickflow_m3_s,
           event,
           eventID) %>% 
    mutate(event = if_else(event == 0, FALSE, TRUE))
   
}

flow_data <- tidy_flow_1(flow_data)


tidy_wq_1 <- function(.data){
  .data %>% 
    mutate(datetime = dmy_hm(sample_ts_UTC), .before = 1) %>% 
    mutate(datetime = round_date(datetime, "15 mins"),
           pre_post = if_else(pre_post == 1, "Pre", "Post")) %>% 
    mutate(sample = TRUE, .before = 2) %>% 
    select(-sample_ts_UTC)
}

wq_data <- tidy_wq_1(wq_data)
  
  
# Join the two datasets ---------------------------------------------------

flow_wq_data <- full_join(flow_data,
                          wq_data,
                          by = "datetime")


# Standardise colour measurements -----------------------------------------

 # convert Abs nm to standard absorbance units of Abs m-1 (we used a 4cm cuvette)
convert_abs <- function(.data){
  .data %>% 
    mutate("Abs254" = Au254nm / 4 * 100,  
           "Abs400" = Au400nm / 4 * 100,
           "Abs465" = Au465nm / 4 * 100,
           "Abs665" = Au665nm / 4 * 100) %>% 
    select(-c(Au254nm, Au400nm, Au465nm, Au665nm))
}

flow_wq_data <- convert_abs(flow_wq_data)
  

# Recalculate DOC ---------------------------------------------------------

 # we need to calibrate the DOC values from the Trios against the measured checks from SWW and our NPOC
 # a different LSA was used in 2017, so need to use calibration data from that time


ggplot() + 
  geom_point(data = wq_data, aes(DOC_sww, DOC_trios, colour = pre_post), alpha = 0.8) +
  geom_point(data = wq_data, aes(DOC_NPOC, DOC_trios, colour = pre_post), alpha = 0.8) +
  geom_smooth(data = wq_data, aes(DOC_sww, DOC_trios), method ="lm", colour = "#324448", linetype = "dashed", size = 0.5) +
  geom_abline(colour = "#6B8B8D", size = 0.5) + #1to1 line
  theme_classic() +
  theme(legend.position = "none") 

ggplot() + 
  geom_point(data = wq_data, aes(Au400nm, DOC_trios, colour = pre_post), alpha = 0.8) +
  geom_smooth(data = wq_data, aes(Au400nm, DOC_trios), method ="lm", colour = "#324448", linetype = "dashed", size = 0.5) +
  geom_abline(colour = "#6B8B8D", size = 0.5) + #1to1 line
  theme_classic() +
  theme(legend.position = "none") 





