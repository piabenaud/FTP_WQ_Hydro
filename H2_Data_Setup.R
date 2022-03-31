
# Title:      Data Setup For  H2
# Objective:  Import and tidy the flow, water quality and event separation data
# Created by: Pia Benaud
# Created on: 2022-03-16


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

wq_data <- read_csv("data/01_WQ_Flume_PrePost.csv")


# Initial Tidy ------------------------------------------------------------

tidy_flow <- function(.data){
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

flow_data <- tidy_flow(flow_data)


tidy_wq <- function(.data){
  .data %>% 
    mutate(datetime = dmy_hm(sample_ts_UTC), .before = 1) %>% 
    mutate(datetime = round_date(datetime, "15 mins"),
           pre_post = if_else(pre_post == 1, "Pre", "Post")) %>% 
    mutate(sample = TRUE, .before = 2) %>% 
    select(-sample_ts_UTC)
}

wq_data <- tidy_wq(wq_data)
  
  
# Join the two datasets ---------------------------------------------------

flow_wq_data <- full_join(flow_data,
                          wq_data,
                          by = "datetime")

rm(flow_data, wq_data)


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

# need to calibrate the DOC values from the Trios against the measured checks from SWW and our NPOC
# a different LSA was used from 2017, so need to use calibration data from that time
# assumption is the relationship remains consistent over time.

# quick look at the data
ggplot() + # is there a consistent relationship between the trios and the check
  geom_point(data = flow_wq_data, aes(DOC_sww, DOC_trios, colour = as.factor(year(datetime))), alpha = 0.8) +
  geom_point(data = flow_wq_data, aes(DOC_NPOC, DOC_trios, colour = as.factor(year(datetime))), alpha = 0.8) +
  geom_smooth(data = flow_wq_data, aes(DOC_sww, DOC_trios), method ="lm", colour = "blue", linetype = "dashed", size = 0.5) +
  geom_smooth(data = flow_wq_data, aes(DOC_NPOC, DOC_trios), method ="lm", colour = "green", linetype = "dashed", size = 0.5) +
  geom_abline(colour = "#6B8B8D", size = 0.5) + #1to1 line
  theme_classic() +
  theme(legend.position = "bottom") 

ggplot() +  # should be a fairly consistent relationship between DOC and Colour (Abs400)
  geom_point(data = flow_wq_data, aes(Abs400, DOC_trios, colour = as.factor(year(datetime)))) +
  theme_classic() +
  theme(legend.position = "bottom") 


recalc_doc <- function(.data){
  
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


flow_wq_data <-recalc_doc(flow_wq_data)

# have another look at the data
ggplot() +  # should be a fairly consistent relationship between DOC and Colour (Abs400)
  geom_point(data = flow_wq_data, aes(Abs400, DOC_calibrated, colour = as.factor(year(datetime)))) +
  theme_classic() +
  theme(legend.position = "bottom") 



# Identify adequately sampled events --------------------------------------

# Criteria:
# 1. Sample must be taken within a separated event.
# 2. There must be >3 samples taken within the event.
# 3. The sampling must cover >70% of the event discharge.

adequate_sampling <- function(.data){
  
  # ID events where sampling covers >70% of the event discharge
  ok_events <- .data %>% 
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
  
  out <- .data %>% 
    filter(event == TRUE) %>% # must be an event
    group_by(eventID) %>% 
    filter(sum(sample, na.rm = TRUE) > 3) %>%  # must have >3 samples in the event
    filter(eventID %in% ok_events) # must cover >70% sampling
  
 return(out)

}


flow_wq_data <- adequate_sampling(flow_wq_data)

 # Have a quick look
flow_wq_data %>% 
  mutate("sampletime" = if_else(sample != "NA",
                                paste(datetime),
                                "NA")) %>% 
  mutate(sampletime = ymd_hms(sampletime)) %>% 
  ggplot() +
  geom_line(aes(datetime, stormflow_m3_s), colour = "#1f78b4") + 
  geom_vline( aes(xintercept = sampletime), colour = "gray20", alpha = 0.6)+
  facet_wrap(~eventID, scales = "free")

 # Event 147 doesn't look great - let's remove
flow_wq_data <- flow_wq_data %>% 
  filter(eventID != "147")


# Calculate Flow Weighted Mean Concentrations -----------------------------





# Calculate DOC loads -----------------------------------------------------













# Tidy --------------------------------------------------------------------

# export all the data sets




rm(flow_data, wq_data)

