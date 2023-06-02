
# Title:      FTP Hydrological and Water Quality Targets Pipeline
# Objective:  Work through the processing needed for the analysis of the hydrological and water quality data from FTP
# Created by: Pia Benaud
# Created on: 05-05-2023




# Load Targets ------------------------------------------------------------

library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint


# Define Targets options --------------------------------------------------

# Set target options:
tar_option_set(
  packages = c("tidyverse", "lubridate", "broom", "gam", "ggforce", "patchwork", "glm2", "emmeans", "cowplot"), 
  format = "rds" # default storage format
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")


# Import project functions ------------------------------------------------

purrr::map(list.files("R/", full.names = TRUE), ~source(.)) # source all functions in R folder


# Define the pipeline -----------------------------------------------------

list(
  
  ## Hydrological analysis ----
  
  # All Q and Rainfall data
  tar_target(All_Rain_Q, "data/04_Rainfall_Q_15min.rds", format = "file"),
  
  # Separated Events - derived using "00_Event_Extraction" processing
  tar_target(Event_Windows, "00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/final_event_windows", format = "file"),
  
  # Event Metrics - derived using "00_Event_Extraction" processing
  tar_target(Event_Metrics, "00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/eventEx_EVENTS_metrics.rds", format = "file"),
  

  # import and tidy hydro events
  tar_target(All_Event_Data, Import_Events(events_folder = Event_Windows)),
  
  # derive gams
  tar_target(The_GAMs, Derive_GAMs(All_Event_Data, perc_cutoff = 0.95)),
  
  # summarise gam outputs
  tar_target(GAM_Summary, Summarise_GAM(The_GAMs)),
  
  # plot the gam hydrographs
  tar_target(GAM_Plots, plot_GAM_hydro(the_data = The_GAMs, 
                                       summary_data = GAM_Summary)),
  
  # make df for GLM analysis
  tar_target(GLM_Data, Data_for_GLM(event_metrics = Event_Metrics,
                                     all_rain_q = All_Rain_Q)),
  
  # derive glms
  tar_target(The_GLM, Derive_GLMs(GLM_Data)),
  
  # summarise glm outputs
  
  # use glm to model predictions
  tar_target(GLM_Predictions, broom::augment(The_GLM, type.predict = "response",
                                             type.residuals = "deviance",
                                             se_fit = T)),
  
  # plot glms
  tar_target(GLM_Plot, glm.plot(GLM_Data, 
                                model.data = GLM_Predictions)),

  # emmeans
  tar_target(Emmeans, emmeans::emmeans(The_GLM, ~resto)),
  
  # combine plots
  tar_target(Hydro_Plot, Plot_Hydro(GAM_Plots, GLM_Plot)),
  
  
  ## Water Quality analysis ----
  
  # WQ data file
  tar_target(WQ_Raw, "data/01_WQ_Flume_PrePost.csv", format = "file"),
  
  # Flow data file
  tar_target(Flow_Raw, "00_Event_Extraction/FTP/run_20220315_1708_value__padding2880_alpha0.98_passes3_BFI0.619/eventEx_OUTPUTS_timeseries.rds", format = "file"),
  
  # Import WQ and bind with event q data
  tar_target(WQ_Q_Data_Raw, Bind_WQ_Q(flow_file = Flow_Raw,
                                     wq_file = WQ_Raw)),
  
  #calibrate spectral DOC measurements
  tar_target(DOC_WQ_Q_Data, Calibrate_DOC(WQ_Q_Data_Raw)),
  
  #calculate additional base WQ metrics i.e. SUVA, C_C and standardised absorbance units
  tar_target(All_WQ_Q_Data, Additional_WQ(DOC_WQ_Q_Data)),
  
  # check for adequate sampling
  tar_target(WQ_Q_Data, ID_Adequate(All_WQ_Q_Data)),
  
  # calculate loads
  tar_target(WQ_Q_Data_Loads, DOC_Load(WQ_Q_Data)),
  
  # calculate FWMC
  tar_target(WQ_Q_Data_FWMC, FWMC(WQ_Q_Data)),
  
  # join two datasets
  tar_target(WQ_Q_Data_All, left_join(WQ_Q_Data_Loads, WQ_Q_Data_FWMC)),
  
  # run event by event summary stats
  tar_target(WQ_Q_event_stats, Event_Summary_Stats(WQ_Q_Data_All)),
  
  # plot summary stats
  tar_target(WQ_Box_Plot, WQ_Boxplots(WQ_Q_event_stats)),
  
  # wq stats
  tar_target(WQ_Stats_Results, WQ_Stats(WQ_Q_event_stats)),
  
  # flow stats
  tar_target(Flow_Stats, Flow.Sum.Tab(All_Event_Data))
  
)







