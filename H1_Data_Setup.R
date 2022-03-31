
# Title:      Data Setup For  H1
# Objective:  Import and tidy the flow / event separation data
# Created by: Pia Benaud
# Created on: 2022-03-31


# Load packages -----------------------------------------------------------

library(tidyverse) # readr, dplyr, ggplot2, purrr, stringr, tidyr, tibble
library(lubridate) # working with dates and time
library(broom) # snazzy stats stuff

#rm(list = ls()) # if you need a quick clean of the environment - DOES NOT UNLOAD PACKAGES!


# Bring over the Event Extraction data ------------------------------------
# Only need to run once, should change to an if exists...

# read_rds("00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/eventEx_OUTPUTS_timeseries.rds") %>% 
#   write_csv(., "data/04_Extracted_Events.csv")
# 
# read_csv("00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/eventEx_EVENTS_metrics.csv") %>% 
#   write_csv(., "data/05_Extracted_Events_Metrics.csv")


# Import the data ---------------------------------------------------------

flow_data <- read.csv("data/04_Extracted_Events.csv") # have to use read.csv here as the eventID col is saved as logical


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
    mutate(datetime = ymd_hms(datetime)) %>% 
    mutate(event = if_else(event == 0, FALSE, TRUE)) %>% 
    mutate(eventID = as.numeric(eventID)) %>% 
    as_tibble()
}

flow_data <- tidy_flow_1(flow_data)


