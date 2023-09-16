
# Title:      DOC Loads 
# Objective:  Calculate Ev
# Created by: Pia Benaud
# Created on: 


# Load packages -----------------------------------------------------------

# library(tidyverse) # wranglings
# library(sf)
# # 
# targets::tar_load(WQ_Q_Data)
# wq_flow_data <- WQ_Q_Data
# source("R/Method5.R")

# The Function ------------------------------------------------------------

DOC_Load <- function(wq_flow_data, shp_file){
  
  area_m2 <- st_read(shp_file) %>% # for calculating load per unit area (ha)
    st_area(.)%>% 
    as.numeric(.) 
  
  area_ha <- area_m2/10000 
  
  split_data <- wq_flow_data %>% 
    group_by(eventID) %>% 
    group_split() %>% 
    setNames(unique(wq_flow_data$eventID))
  
  # method5 <- function(db){ # this is modified from the RiverLoad package github as it is no longer available on CRAN (https://github.com/VeronicaNava/RiverLoad/tree/master)
  #   ncomp <- ncol(db) - 2
  #   notNA <- db %>% na.omit()
  #   
  #   matCQ <- matrix(nrow = nrow(notNA), ncol = ncomp)
  #   for (i in 3:(2 + ncomp)) {
  #     matCQ[, i - 2] <- notNA$flow * notNA[, i]
  #   }
  #   sum.CQ <- colSums(matCQ)
  #   
  #   sum.flow <- sum(notNA$flow)
  #   fluxM5gsec <- (sum.CQ / sum.flow) * mean(db$flow)
  #   
  #   difference <- as.numeric(db[nrow(db), 1] - db[1, 1])
  #   method5 <- fluxM5gsec * (difference) * 86400
  #   
  #   mat.met <- matrix(method5, nrow = 1, ncol = ncomp)
  #   colnames(mat.met) <- names(db)[3:(ncomp + 2)]
  #   
  #   method5N <- as.numeric(mat.met)
  #   names(method5N) <- names(db)[3:(ncomp + 2)]
  #   
  #   return(method5N)
  # }
  
  indv_load <- function(the_data){
    the_data %>% 
      transmute(datetime = datetime,
                flow = q_m3_s,
                DOC = DOC_calibrated) %>%  # note: DOC in mg/l == g/m3.
      as.data.frame() %>%   # RiverLoad needs a DF
      method5(., 1)
  }
  
  event_loads <- map(split_data, ~indv_load(.))
  
  out <- map2_df(split_data, event_loads, ~ .x %>% mutate("DOC_Load_g" = .y)) %>%  # use event_loads to make new DOC_Load col
    mutate(DOC_Load_kg = DOC_Load_g/1000) %>% # convert to kg, which is a slightly more sensible unit
    mutate(DOC_Load_inst = DOC_calibrated * q_m3_s) %>%  # in g
    mutate(DOC_Load_ha = DOC_Load_kg/area_ha) # calculate load per ha
  
}
