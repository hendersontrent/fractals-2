#---------------------------------------
# This script sets out to define a set 
# of helper functions that can process 
# the raw HRV data into a tidy format
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 5 August 2021
#---------------------------------------

#' Function to clean some regex
#' 
#' @param data the dataframe to clean
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

time_cleaner <- function(data){
  
  tmp <- data %>%
    gather(key = minute, value = value, 1:15) %>% # Pull into 2 columns
    mutate(minute = gsub("_.*", "", minute)) %>% # Remove nuisance symbols
    mutate(minute = gsub("[A-z]", "\\1", minute)) %>% # Extract just minute number
    mutate(minute = as.numeric(minute))
  
  return(tmp)
}

#' Function to wrangle HRV data into correct format
#' 
#' @param data the file path of the HRV data to load
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

process_hrv_data <- function(data){
  
  # Load and prep data
  
  d <- read_csv(data) %>%
    clean_names()
  
  #---------------
  # Data reshaping
  #---------------
  
  # T1 Rest
  
  df_t1_base <- d %>%
    dplyr::select(c(1:15)) %>%
    mutate(id = row_number())
  
  df_t1_base <- time_cleaner(df_t1_base) %>%
    mutate(condition = "T1") %>%
    mutate(state = "Rest")
  
  # T1 Meditation
  
  df_t1_med <- d %>%
    dplyr::select(c(16:30)) %>%
    mutate(id = row_number())
  
  df_t1_med <- time_cleaner(df_t1_med) %>%
    mutate(condition = "T1") %>%
    mutate(state = "Meditation") %>%
    mutate(minute = minute-15)
  
  # T2 Rest
  
  df_t2_base <- d %>%
    dplyr::select(c(31:45)) %>%
    mutate(id = row_number())
  
  df_t2_base <- time_cleaner(df_t2_base) %>%
    mutate(condition = "T2") %>%
    mutate(state = "Rest") %>%
    mutate(minute = minute-(2*15))
  
  # T2 Meditation
  
  df_t2_med <- d %>%
    dplyr::select(c(46:60)) %>%
    mutate(id = row_number())
  
  df_t2_med <- time_cleaner(df_t2_med) %>%
    mutate(condition = "T2") %>%
    mutate(state = "Meditation") %>%
    mutate(minute = minute-(3*15))
  
  #----------
  # MERGING
  #----------
  
  # Bind files together
  
  tmp <- bind_rows(df_t1_base, df_t1_med, df_t2_base, df_t2_med) %>%
    mutate(state = ifelse(state == "Rest", 1, 2),
           condition = ifelse(condition == "T1", 1, 2))
  
  return(tmp)
}
