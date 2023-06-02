
# Title:      ID adequate sampling
# Objective:  Pull out the events that have been sampled adequately for analysis
# Created by: Pia Benaud
# Created on: 10-05-2023

# Criteria based on Grand-Clement et al 2014/2023??:
# 1. Sample must be taken within a separated event.
# 2. There must be >3 samples taken within the event.
# 3. The sampling must cover >70% of the event discharge.


# Load packages -----------------------------------------------------------

#library(tidyverse) # wranglings

#tar_load(All_WQ_Q_Data)

# The function ------------------------------------------------------------

#flow_wq_data <- All_WQ_Q_Data

ID_Adequate <- function(flow_wq_data){
  
  # ID events where sampling covers >70% of the event discharge
  ok_events <- flow_wq_data %>% 
    group_by(eventID) %>% 
    mutate("storm_15" = stormflow_m3_s * 900,  # total quickflow for each 15 mins
           "cum_storm_15" = cumsum(storm_15),  # cumulative discharge during each event 
           "cum_event" = max(cum_storm_15)) %>%  # total event discharge
    filter(!is.na(sample)) %>%  # filter to sample to aid next step
    mutate("cum_sample" = max(cum_storm_15) - min(cum_storm_15), # total discharge between first and last sample
           "over_70" = (cum_sample / cum_event) > 0.7) %>%   # >70% flag
    ungroup(eventID) %>%   
    distinct(eventID, .keep_all = TRUE) %>%  # list of individual events
    transmute("x" = case_when(over_70 == TRUE ~ eventID)) %>%  # list of events with >70% sampled 
    filter(!is.na(x)) %>% # remove NAs
    pull(x) # converts into vector
  
  out <- flow_wq_data %>% 
    filter(event == TRUE) %>% # must be an event
    group_by(eventID) %>% 
    filter(sum(sample, na.rm = TRUE) > 3) %>%  # must have >3 samples in the event
    filter(eventID %in% ok_events) %>%  # must cover >70% sampling
    filter(eventID != "147") # based on the below
  

  return(out)
  
}


# 
# # Have a quick look at the remaining events
# flow_wq_data %>% 
#   mutate("sampletime" = if_else(sample != "NA",
#                                 paste(datetime),
#                                 "NA")) %>% 
#   mutate(sampletime = ymd_hms(sampletime)) %>% 
#   ggplot() +
#   geom_line(aes(datetime, stormflow_m3_s), colour = "#1f78b4") + 
#   geom_vline( aes(xintercept = sampletime), colour = "gray20", alpha = 0.6) +
#   scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M", date_breaks = "12 hours") +
#   facet_wrap(~eventID, scales = "free")
# 
# 
# # Event 147 doesn't look great - let's remove

