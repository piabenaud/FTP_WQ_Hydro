read_hydro_csv <- function(dateTime_col, rain_col, response_col,lubridate_time, file_path){
  if (missing(file_path)){
    
    read.csv(file.choose()) %>%
      rename('datetime' = !!dateTime_col)%>%
      mutate(datetime = lubridate_time(datetime, tz='UTC')) %>%
      rename('rainfall' = !!rain_col)%>%
      rename('q' = !!response_col) %>%
      select(datetime, rainfall, q)
  } else {
    read.csv(file_path) %>%
      rename('datetime' = !!dateTime_col)%>%
      mutate(datetime = lubridate_time(datetime, tz='UTC')) %>%
      rename('rainfall' = !!rain_col)%>%
      rename('q' = !!response_col) %>%
      select(datetime, rainfall, q)
  }}

read_hydro_rds <- function(dateTime_col, rain_col, response_col,lubridate_time, file_path){
  if (missing(file_path)){
    
    readRDS(file.choose()) %>%
      rename('datetime' = !!dateTime_col)%>%
      mutate(datetime = lubridate_time(datetime, tz='UTC')) %>%
      rename('rainfall' = !!rain_col)%>%
      rename('q' = !!response_col) %>%
      select(datetime, rainfall, q)
  } else {
    readRDS(file_path) %>%
      rename('datetime' = !!dateTime_col)%>%
      mutate(datetime = lubridate_time(datetime, tz='UTC')) %>%
      rename('rainfall' = !!rain_col)%>%
      rename('q' = !!response_col) %>%
      select(datetime, rainfall, q)
  }}
