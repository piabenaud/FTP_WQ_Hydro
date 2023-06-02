
# Title:      GLMs Data
# Objective:  Generate the dataframes needed for the GLM
# Created by: Pia Benaud
# Created on: 10-05-2023

# Based on functions in: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/6_Event_Stats/Graham_etal_InReview_BudBrook_Mech.R
# and here: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/6_Event_Stats/mechanisms_paper_functions.R 
# (DOI: 10.5281/zenodo.6034308)

# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# library(lubridate)

Data_for_GLM <- function(event_metrics, all_rain_q){
  
  # import and tidy event metric data
  all_event_metrics <- read_rds(event_metrics) %>% 
    mutate(resto = as.factor(ifelse(event.start.ts > ymd_hms("2014-10-01 00:00:00", tz = "UTC"), "After",
                                    ifelse(event.start.ts > ymd_hms("2014-08-01 00:00:00", tz = "UTC") &
                                             event.start.ts < ymd_hms("2014-10-01 00:00:00", tz = "UTC"), "During", "Before")))) %>%
    filter(resto != "During" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>% # removes period of restoration.
    mutate(resto = fct_relevel(resto, "Before", "After")) %>%
    mutate(rain.rate = rain.tot.mm/(rain.dur@.Data/3600)) %>%
    mutate(flow.rate = Q.response.tot.m3/quickflow.dur@.Data) %>%
    mutate(init.rain.rate = (init.rain.tot.mm/init.rain.dur@.Data)*60*60) %>%
    mutate(Month = (month(event.start.ts))) %>%
    mutate(Year = as.factor(year(event.start.ts))) %>%
    mutate(rlimb.dur.hrs = (rlimb.dur@.Data/60)/60)%>%
    mutate(flimb.dur.hrs = (flimb.dur@.Data/60)/60)%>%
    mutate(Season = (ifelse(Month >= 3 & Month < 6, "Spring", # define seasons - need to decide if we include this analysis
                            (ifelse(Month >=6 & Month <9, "Summer",
                                    (ifelse(Month >=9 & Month <12, "Autumn",
                                            (ifelse(Month >=12 | Month <3, "Winter", ""))))))))) %>%
    mutate(Hydro.Seas = ifelse(Month >= 10 | Month < 4, "Wet", "Dry")) %>% # uk hydro year starts in October
    mutate(Season = fct_relevel(Season, "Winter","Spring", "Summer", "Autumn"))%>%
    mutate(Hydro.Seas = fct_relevel(Hydro.Seas, "Dry", "Wet")) %>%
    select(-c(response.eventID,check.outlier1)) %>%
    drop_na()
  
  
  # calculate new excedence limits
  all_q <- read_rds(all_rain_q) %>%  # need to reimport all flow data for calculating correct excedence
    select(q) %>% 
    drop_na()
  
  perc_flow <- ecdf(all_q$q)
  
  # calculate and tidy
  event_metrics <- all_event_metrics %>% 
    mutate(per_q = (1 - perc_flow(Q.peak.m3.s)) * 100) %>% 
    select(rain.tot.mm, Q.peak.m3.s, rain.peak.ts, Q.peak.ts, anti.rain.mm5d, 
           per_q, resto, Hydro.Seas, Season, rain.mean, event.Q.tot.m3, 
           event.quickflow.tot.m3,anti.rain.mm5d)
  
}