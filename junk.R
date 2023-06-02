

targets::tar_load(WQ_Q_Data_All)
targets::tar_load(WQ_Q_event_stats)

library(tidyverse)


temp <- WQ_Q_event_stats %>% 
  arrange(metric)

total_q <- WQ_Q_event_stats %>% 
  ungroup() %>% 
  filter(metric == "q_m3_s") %>% 
  select(eventID, total_q, resto)

total_r <- WQ_Q_event_stats %>% 
  ungroup() %>% 
  filter(metric == "rainfall_mm_h") %>% 
  select(eventID, total_rain, resto)

total_load <- WQ_Q_event_stats %>% 
  ungroup() %>% 
  filter(metric == "DOC_Load_kg") %>% 
  select(eventID, mean, resto) %>% 
  rename(load = mean)

total_c <- WQ_Q_event_stats %>% 
  ungroup() %>% 
  filter(metric == "FWMC_DOC") %>% 
  select(eventID, mean, resto) %>% 
  rename(DOC = mean)

join <- left_join(total_load, total_r) %>% 
  left_join(., total_q) %>% 
  left_join(., total_c) %>% 
  select(eventID, resto, total_rain, total_q, DOC, load) %>% 
  mutate(norm_load = load/total_rain) %>% 
  pivot_longer(., 3:6, names_to = "metric", values_to = "result")

ggplot(join) +
  geom_point(aes(x = total_q, y = DOC, colour = resto)) 
  

join %>% 
  mutate(resto = fct_relevel(resto, "Before", "After")) %>% 
  ggplot() +
  geom_boxplot(aes(x = resto, y = result, fill = resto))+
  geom_jitter(aes(x = resto, y = result, fill = resto), alpha = 0.5) +
  facet_wrap(vars(metric), scales = "free") +
  scale_fill_manual(values = c("Before" = "#ca0020", "After" = "#0571b0"))+
  theme_bw()


ggplot(data = WQ_Q_Data_All) +
  geom_boxplot(aes(x = resto, y = DOC_calibrated))+
  geom_jitter(aes(x = resto, y = DOC_calibrated))


ggplot(data = WQ_Q_event_stats) +
  geom_boxplot(aes(x = resto, y = DOC_calibrated))+
  geom_jitter(aes(x = resto, y = DOC_calibrated))



WQ_Q_event_stats %>% 
  filter(metric %in% c("FWMC_DOC", "DOC_Load_kg", "FWMC_Abs400", "FWMC_C_C", "FWMC_SUVA", "DOC_Load_inst")) %>% 
  mutate(resto = fct_relevel(resto, "Before", "After")) %>% 
  ggplot() +
  geom_boxplot(aes(x = resto, y = mean, fill = resto))+
  geom_jitter(aes(x = resto, y = mean, fill = resto), alpha = 0.5) +
  facet_wrap(vars(metric), scales = "free") +
  scale_fill_manual(values = c("Before" = "#ca0020", "After" = "#0571b0"))+
  theme_bw()

 WQ_Q_event_stats %>% 
mutate(resto = fct_relevel(resto, "Before", "After")) %>% 
ggplot() +
  geom_boxplot(aes(x = resto, y = median))+
  geom_jitter(aes(x = resto, y = median)) +
  facet_wrap(vars(metric), scales = "free")



ggplot(WQ_Q_Data_All)+
  geom_point(aes(colour = resto, x = q_m3_s, y = DOC_calibrated))

ggplot(WQ_Q_Data_All)+
  geom_point(aes(colour = resto, x = q_m3_s/rainfall_mm_h, y = DOC_calibrated))
