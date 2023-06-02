
# Title:      Flow plots
# Objective:  Use GAM outputs to plot an averaged/GAM hydrograph for pre and post restoration
# Created by: Pia Benaud
# Created on: 10-05-2023


# Load packages -----------------------------------------------------------

# library(tidyverse)
# library(cowplot)
# library(patchwork)

# 
# gam_plot <- GAM_Plots
# glm_plot <- GLM_Plot

# The Function ------------------------------------------------------------

Plot_Hydro <- function(gam_plot, glm_plot){
  
  legend_fun <- function(.plot) { 
    the_legend <- cowplot::get_legend(
      .plot +
        theme(legend.direction = "horizontal",
              legend.position = "bottom",
              legend.margin = margin(t = 0, unit = "cm"),
              legend.key.size = unit(0.5, "cm"),
              legend.text = element_text(color="gray20", )))
    return(the_legend)
  }
  
  the_legend <- legend_fun(gam_plot)
  
  hydro_plots <- plot_grid(gam_plot, glm_plot, nrow = 1)
  
  out <- plot_grid(the_legend, hydro_plots, nrow = 2, rel_heights = c(0.1, 1))

  return(out)
  
  
}
