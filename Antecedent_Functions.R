
# Title:      Antecedent Functions
# Objective:  This script writes the functions needed to summarise the antecedent environmental conditions before a point in time (i.e. a water sample).
# Created by: Pia Benaud
# Created on: 18-11-2020


# The params we're using --------------------------------------------------

# df1 is the water sample dataframe(or list of dataframes)
# df2 is the dataframe holding the environmental condition of interest(or list of dataframes)
# x is the name of the datetime col for df1 (sample time) provided in quotations
# y is the name of the datetime col for df2 provided in quotations
# z is the name of the variable within df2 that is of interest provided in quotations


# Time interval function --------------------------------------------------

 # now the function "interval_calcs" where mean, max, min and total is calculated for each time step
interval_calcs <- function(.time_step, df2, x, y, z){
  
  y <- sym(y) # change to symbol
  z <- sym(z)
  
  tab <- df2 %>% 
    filter(!!y >= x - .time_step, !!y <= x) %>% # filter data to interval of interest
    summarise("mean_{z}" := round(mean(!!z, na.rm=TRUE), 2), # calculate summary stats of interest
              "max_{z}" := round(max(!!z, na.rm=TRUE), 2),
              "min_{z}" := round(min(!!z, na.rm=TRUE), 2),
              "total_{z}" := round(sum(!!z, na.rm=TRUE), 2)) %>% 
    mutate(datetime = x, .before = 1,  
           antecedent_interval = .time_step)
  
  return(tab)
}


# Function to run through list of intervals -------------------------------

interval_run <- function (df2, x, y, z) {
  
  time_intervals <- list(hours(1), #lubridate times     Ooo should change this to an input param??
                         hours(2), 
                         hours(6), 
                         hours(12), 
                         days(1), 
                         days(2), 
                         days(7), 
                         days(14), 
                         days(21), 
                         days(30),
                         days(60))
  
  time_intervals %>% 
    map(., ~interval_calcs(., df2, x, y, z)) %>% # run through all the time intervals
    bind_rows()
    
}




# Now the main function ---------------------------------------------------

antecedent_conditions <- function(df1, df2, x, y, z){
  
  x <- sym(x) # change to symbol
  
  x <- df1 %>%  # make list of all sample times
    distinct(!! x) %>%  
    pull() 
  
  ant_output <- map(x, ~interval_run(df2, ., y, z)) %>% # apply function to all timesteps and bind output together
    bind_rows()
  
  join_up <- tibble(merge(df1, #join antecedent data with the original data         
                   ant_output))
  
  return(join_up)
  
}


# Now let's test it -------------------------------------------------------

#  # Bring in some test data
# df1 <- EA_spot[[1]]
# df2 <- Rain_hour[[1]]
# x <- 'datetime'
# y <- 'datetime'
# z <- 'rain_intensity_mmhr'
# 
# 
# start_time <- Sys.time()
# df1 <- antecedent_conditions(df1, df2, x, y, z)
# end_time <- Sys.time()
# end_time - start_time
# 


