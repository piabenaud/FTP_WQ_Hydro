
# Title:      Import Water Quality data and bind with Event data
# Objective:  Import and tidy event Q data and water quality data
# Created by: Pia Benaud
# Created on: 11-05-2023


# Load packages -----------------------------------------------------------

# library(tidyverse) # readr, dplyr, ggplot2, purrr, stringr, tidyr, tibble
# library(lubridate) # working with dates and time
# # 
# flow_file <- Flow_Raw
# wq_file <- WQ_Raw

# The function ------------------------------------------------------------

Bind_WQ_Q <- function(flow_file, wq_file){
  
  wq_data <- read_csv(wq_file, show_col_types = FALSE)
  flow_data <- read_rds(flow_file)
  
  wq_data <- wq_data %>% 
    mutate(datetime = dmy_hm(sample_ts_UTC), .before = 1) %>% 
    mutate(datetime = round_date(datetime, "15 mins")) %>% 
    mutate(sample = TRUE, .before = 2) %>% 
    select(-sample_ts_UTC)
  
  flow_data <- flow_data %>% 
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
    mutate(resto = as.factor(ifelse(datetime > ymd_hms("2014-10-01 00:00:00", tz = "UTC"), "After",
                                    ifelse(datetime > ymd_hms("2014-08-01 00:00:00", tz = "UTC") &
                                             datetime < ymd_hms("2014-10-01 00:00:00", tz = "UTC"), "During", "Before")))) %>%
    as_tibble()
  
  flow_wq_data <- full_join(flow_data,
                            wq_data,
                            by = "datetime")
  
}


