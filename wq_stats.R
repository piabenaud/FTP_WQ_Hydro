

# Title:      WQ Stats
# Objective:  
# Created by: Pia Benaud
# Created on: 


# Load packages -----------------------------------------------------------


#library(broom)


# The function ------------------------------------------------------------

WQ_Stats <- function(summary_stats){
  
  test <- summary_stats %>% 
    ungroup() %>% 
    filter(metric %in% c("FWMC_DOC", "DOC_Load_kg", "FWMC_Abs400", "FWMC_C_C", "FWMC_SUVA")) %>% 
    select(metric, resto, mean) %>% 
    group_by(resto, metric) %>% 
    nest() %>% 
    mutate(sw.norm = map(data, ~glance(shapiro.test(.$mean)))) %>% 
    unnest(sw.norm) %>% 
    select(resto, metric, data, p.value) %>% 
    unnest(cols = c(data)) %>% 
    mutate(is_norm = if_else(p.value > 0.05, "norm", "not_norm"))
  
  
  test_2 <- test %>% 
    mutate(result_log10 = log10(mean)) %>% 
    select(metric, resto, mean, result_log10) %>% 
    group_by(resto, metric) %>% 
    nest() %>% 
    mutate(sw.norm = map(data, ~glance(shapiro.test(.$result_log10)))) %>% 
    unnest(sw.norm) %>% 
    select(resto, metric, data, p.value) %>% 
    unnest(cols = c(data)) %>% 
    mutate(is_norm = if_else(p.value > 0.05, "norm", "not_norm"))
  
  wilcox_is_significant <- test_2 %>% 
    select(metric, resto, mean, result_log10, is_norm) %>% 
    group_by(metric) %>% 
    nest() %>% 
    mutate(sig_test = map(data, ~glance(wilcox.test(.$result_log10 ~ .$resto)))) %>% 
    unnest(sig_test) %>% 
    select(metric, data, p.value) %>% 
    unnest(cols = c(data)) %>% 
    mutate(is_diff = if_else(p.value > 0.05, "no_sig_diff", "sig_diff")) %>%
    distinct(metric, .keep_all = TRUE) %>% 
    select(metric, p.value, is_diff)
  
}


