
# Title:      Plot GLMs
# Objective:  Plot the GLM predictions against observations
# Created by: Pia Benaud
# Created on: 10-05-2023

# Based on functions in: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/6_Event_Stats/Graham_etal_InReview_BudBrook_Mech.R
# and here: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/6_Event_Stats/mechanisms_paper_functions.R 
# (DOI: 10.5281/zenodo.6034308)

# Load packages -----------------------------------------------------------

#library(tidyverse) # wranglings and plots


# The function ------------------------------------------------------------

glm.plot <- function(.data, model.data, line=TRUE) {
  
  p <- ggplot(model.data, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=resto, fill=resto))+
    geom_point(data=.data, alpha = 0.4, size=0.9, stroke=0)
  
  if (isTRUE(lines)) {
    p <- p + geom_line(aes(x=rain.tot.mm, y = .fitted))
  }
  p +
    geom_ribbon(aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), 
                    ymax = .fitted + (1.96 *.se.fit)), 
                alpha=0.7, lwd=0.7) +
    scale_color_manual(values = c("Before" = "#ca0020", "After" = "#0571b0")) +
    scale_fill_manual(values = c("Before" = "#ca0020", "After" = "#0571b0")) +
    # scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1),
    #               labels = c(0.001, 0.01, 0.1, 1),
    #               limits = c(0.005, 0.5)) +
    # annotation_logticks(sides='l') +
    labs(y= (expression("Event Maximum Flow " (m^{3}~s^{-1}))),
         x=expression("Total Event Rainfall " (mm)),
         colour = "Restoration", 
         fill = "Restoration") +
    theme_bw() +
    theme(legend.position = 'none')
}

#test <- glm.plot(event_metrics, model.data = glm_predictions)
