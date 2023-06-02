
# edited from: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/6_Event_Stats/Puttock_etal_2021_BudBrook.R


Flow.Sum.Tab <- function(.data){
  .data %>%
    group_by(resto) %>% 
    summarize(Mean = mean(q_m3_s), Median = median(q_m3_s), R2FDC = ((log10(quantile(q_m3_s, 0.66)) - log10(quantile(q_m3_s, 0.33)))/(0.66-0.33))*-1,
              Q5 = quantile(q_m3_s, 0.95), Q95 = quantile(q_m3_s, 0.05)) %>%
    mutate(resto = c('Before','After'), `Q5:Q95 ratio` = Q5/Q95) %>%
    rename(" " = resto) %>%
    bind_rows(summarise(.," " = '% Change',
                        Mean = (Mean[2]-Mean[1])/Mean[1]*100,
                        Median = (Median[2]-Median[1])/Median[1]*100,
                        R2FDC= (R2FDC[2]-R2FDC[1])/R2FDC[1]*100,
                        Q5 = (Q5[2]-Q5[1])/Q5[1]*100,
                        Q95 = (Q95[2]-Q95[1])/Q95[1]*100,
                        `Q5:Q95 ratio` = (`Q5:Q95 ratio`[2]-`Q5:Q95 ratio`[1])/`Q5:Q95 ratio`[1]*100,))%>%
    mutate_at(vars(Mean, Median, R2FDC, Q5, Q95, `Q5:Q95 ratio`), round,3) 
}


