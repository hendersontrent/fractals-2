#------------------------------------------------
# This script sets out to run models on the
# Fractals 1.0 paper data to extract and save
# priors for use in models for the current paper
#------------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 4 March 2021
#--------------------------------------

#----------------------- Read in and pre process data ------------------

# Load and prep data

d <- read.csv("data/fractals_1_1minutebins.csv") %>%
  clean_names()
  
# Define general regex cleaning function

time_cleaner <- function(data){
  data <- data %>%
    gather(key = minute, value = value, 1:15) %>% # Pull into 2 columns
    mutate(minute = gsub("_.*", "", minute)) %>% # Remove nuisance symbols
    mutate(minute = gsub("[A-z]", "\\1", minute)) %>% # Extract just minute number
    mutate(minute = as.numeric(minute))
}

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
  mutate(state = as.factor(state)) %>%
  mutate(condition = as.factor(condition))

#----------------------- Specify a Bayesian model ----------------------

# Build model with vague and uninformative priors

m1 <- brm(a1 ~ condition*state*minute,
          data = tmp, iter = 2000, chains = 3, seed = 123)

# Visualise distribution of coefficient posteriors to get correct prior shape for Fractals 2.0

as.data.frame(m1) %>%
  clean_names() %>%
  dplyr::select(c(b_condition, b_state, b_minute, b_intx)) %>%
  gather(key = parameter, value = value, 1:4) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.001, fill = "steelblue2") +
  labs(title = "Posterior distributions for Fractals 1.0",
       x = "Coefficient Value",
       y = "Frequency") +
  theme_bw() +
  facet_wrap(~parameter)
  
#----------------------- Save priors for future use -------------------
  
# Save as .Rda to avoid needing to re-run model which can take time

priors <- as.data.frame(m1) %>%
  clean_names() %>%
  summarise(beta_condition_mean = mean(b_condition),
            beta_condition_sd = sd(b_condition),
            beta_state_mean = mean(b_state),
            beta_state_sd = sd(b_state),
            beta_minute_mean = mean(b_minute),
            beta_minute_sd = sd(b_minute),
            beta_intx_mean = mean(b_intx),
            beta_intx_sd = sd(b_intx))
            
save(priors, file = "data/priors.Rda")
