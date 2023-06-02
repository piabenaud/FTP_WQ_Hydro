
# Title:      Import_Tidy_Events
# Objective:  Import and tidy event Q data
# Created by: Pia Benaud
# Created on: 05-05-2023

# Based on functions in: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/8_event_overlay/Event_overlay.R
# (DOI: 10.5281/zenodo.6034308)

# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# library(lubridate) # working with dates and time


# The functions -----------------------------------------------------------

#events_folder <- "00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/final_event_windows"

Import_Events <- function(events_folder){
  
  # read files
  read_event <- function(events_folder, event, id){
    
    evFile <- file.path(events_folder, event) 
    
    evDf <- read_csv(evFile, col_types = cols()) %>%
      filter(q_m3_s > 0) %>%  # handfull of zero Q values that need to be removed
      mutate(event_step = row_number()) %>%
      mutate(resto = as.factor(ifelse(datetime > ymd_hms("2014-10-01 00:00:00", tz = "UTC"), "After",
                                      ifelse(datetime > ymd_hms("2014-08-01 00:00:00", tz = "UTC") &
                                               datetime < ymd_hms("2014-10-01 00:00:00", tz = "UTC"), "During", "Before")))) %>%
      filter(resto != "During") %>%
      mutate(tot_rain = sum(rainfall_mm_h)/4) %>%
      mutate(s_flow = pluck(q_m3_s, 1)) %>%
      # mutate(q_m3_s = q_m3_s**(1/(tot_rain))) %>%
      select(datetime, q_m3_s, tot_rain, event_step, resto, s_flow, rainfall_mm_h) %>%
      mutate(event_id = as.factor(id))  
    
  }
  
  # implementing above but safely..
  safe_read <- function(evf, x, id, pb){
    f  = purrr::safely(function() read_event(evf, x, id), otherwise = NA)
    pb$tick()
    f()
  }
  
  # main function to map through file list and join into single df
  df_overlay <- function(events_folder){
    
    event_path_list = list.files(events_folder)
    
    pb <- progress::progress_bar$new(total = length(event_path_list), clear = FALSE)
    
    events_combined <- event_path_list %>%
      purrr::imap(., ~ safe_read(events_folder, .x, .y, pb)) %>%
      purrr::map(., purrr::pluck, "result") %>%
      Filter(Negate(anyNA), .) %>%
      bind_rows() %>%
      mutate(event_step = event_step/4) 
    
  }
  
  out <- df_overlay(events_folder)
  
}

