
# Title:      Plot GAM hydrographs
# Objective:  Use GAM outputs to plot an averaged/GAM hydrograph for pre and post restoration
# Created by: Pia Benaud
# Created on: 05-05-2023

# Based on functions in: https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/8_event_overlay/Event_overlay.R
# and https://github.com/h-a-graham/Budleigh_Brook_Beaver_Hydro/blob/master/8_event_overlay/Run_overlay.R

# (DOI: 10.5281/zenodo.6034308)

# Load packages -----------------------------------------------------------
# 
# library(tidyverse) # wranglings
# library(ggforce)
# library(patchwork)
# # 
# the_data <- The_GAMs
# summary_data <- GAM_Summary

plot_GAM_hydro <- function(the_data, summary_data){ 
  
  .data <- the_data$data %>% 
    mutate(resto = fct_relevel(resto, "Before", "After")) # order logically
  
  #
  PeakQ.df <- summary_data
  
  
  p1 <- ggplot(.data, aes(x=event_step, y=q_m3_s , group=event_id, colour=resto, fill=resto)) +
    geom_line(alpha=0.05, lwd=0.4) +
    geom_ribbon(aes(x=event_step, ymin = gam.fitted - (gam.se.fit*1.96), 
                               ymax = gam.fitted + (gam.se.fit*1.96), colour=resto,
                               fill=resto),lwd=0.7, alpha=0.7, inherit.aes = F) +
    geom_point(data=PeakQ.df, aes(x=PredQMaxTime, y=PredQMax, group=NULL, 
                                  pch='GAM Hydrograph Peak Q and Rainfall'), 
               colour='gray20', size=3)+
    geom_segment(data=PeakQ.df, aes(x=PredPrecMaxTime,xend=PredQMaxTime, y=height, yend=height,
                                    colour=resto, group=NULL),
                 arrow = arrow(length = unit(0.03, "npc")), show.legend=FALSE)+
    geom_linerange(data=PeakQ.df, aes(x=PredPrecMaxTime, ymin=height, ymax=5,
                                      colour=resto, group=NULL, y=NULL), 
                   linetype=2, alpha=0.6, show.legend=FALSE) +
    geom_linerange(data=PeakQ.df, aes(x=PredQMaxTime, ymin=PredQMax, ymax=height,
                                      colour=resto, group=NULL, y=NULL), 
                   linetype=2, alpha=0.6, show.legend=FALSE) +
    geom_text(data=PeakQ.df, mapping = aes(x=32, y=0.1, label= .label, group=NULL), 
              size=3, colour='gray20') + 
    scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1),
                  labels = c(0.001, 0.01, 0.1, 1)) +
    labs(x = 'Time since event start (hrs)', y= (expression('Flow ' (m^{3}~s^{-1}))), color='Restoration Status', fill='Restoration Status') +
    scale_color_manual(values = c("Before" = "#ca0020", "After" = "#0571b0")) +
    scale_fill_manual(values = c("Before" = "#ca0020", "After" = "#0571b0"))+
    theme_bw() + 
    annotation_logticks(sides='l') +
    coord_trans(ylim=c(0.0005, 0.5)) +
    scale_shape_manual(values=13, name=NULL)+
    guides(fill = guide_legend(override.aes = list(shape = NA),
                               order=1),
           colour = guide_legend(order=1))+
    theme(legend.position = "none")
  
  p2 <- .data %>% 
    ggplot(aes(x=event_step, y=.fitted_rain , colour=resto, fill=resto)) +
    geom_point(aes(y=rainfall_mm_h),alpha=0.06, lwd=0.4, size = 0.5) +
    # geom_line(lwd=1.1, alpha=0.4) +
    geom_ribbon(aes(x=event_step, ymin = .fitted_rain - (.se.fit_rain*1.96),
                    ymax = .fitted_rain + (.se.fit_rain*1.96), colour=resto,
                    fill=resto),lwd=0.5, alpha=0.7, inherit.aes = F) +
    geom_point(data=PeakQ.df, aes(x=PredPrecMaxTime, y=PredPrecMax, group=NULL),pch=13, size=3, colour='grey20')+
    geom_linerange(data=PeakQ.df, aes(x=PredPrecMaxTime, ymin=PredPrecMax, ymax=1.1,
                                      colour=resto, group=NULL, y=NULL),
                   linetype=2, alpha=0.6) +
    labs(x = 'time since event start (hrs)', y= (expression('Rainfall ' (mm~hr^{-1})))) +
    scale_color_manual(values = c("Before" = "#ca0020", "After" = "#0571b0")) +
    scale_fill_manual(values = c("Before" = "#ca0020", "After" = "#0571b0"))+
    theme_bw() +
    guides(fill='none', colour='none')+
    scale_y_reverse(limits = c(1.1, 0),labels = scales::number_format(accuracy = 0.1), breaks = c(1, 0.5, 0)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
 out <- p2/p1 + plot_layout(heights = c(1, 2)) +
   plot_annotation(tag_levels = "a")
 
 return(out)
  
}

