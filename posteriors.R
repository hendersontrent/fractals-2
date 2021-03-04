#------------------------------------------------
# This script sets out to run informed Bayesian
# models on Fractals 2.0 data using priors from
# Fractals 1.0
#------------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 4 March 2021
#--------------------------------------

#----------------------- Read in and pre process data ------------------

# Load and prep data

d <- read.csv("data/fractals_2_1minutebins.csv") %>%
  clean_names()
  
# Load priors
  
load("data/priors.Rda")
  
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

# Specify priors

condition_prior <- paste0("normal(",priors$beta_condition_mean,",",priors$beta_condition_sd,")")
state_prior <- paste0("cauchy(",priors$beta_state_mean,",",priors$beta_state_sd,")")
minute_prior <- paste0("normal(",priors$beta_minute_mean,",",priors$beta_minute_sd,")")
intx_prior <- paste0("normal(",priors$beta_intx_mean,",",priors$beta_intx_sd,")")

# Fit model

m1 <- brm(a1 ~ condition*state*minute,
          prior = c(set_prior(prior = condition_prior, class = "b", coef = "b_condition"),
                    set_prior(prior = state_prior, class = "b", coef = "b_state"),
                    set_prior(prior = minute_prior, class = "b", coef = "b_minute"),
                    set_prior(prior = intx, class = "b", coef = "b_intx")),
          data = tmp, iter = 2000, chains = 3, seed = 123)
          
#--------------------- Compute outputs & data vis ----------------------

# Diagnostic 1: Chain convergence

mcmc_trace(m1)

# Diagnostic 2: LOO

loo1 <- loo(m1, save_psis = TRUE)
plot(loo1)

# Diagnostic 3: Posterior predictive checks

pp_check(m1, nsamples = 100) +
  labs(title = "Posterior predictive check",
       x = "HRV",
       y = "Count")

# Summative data visualisation

mcmc_intervals(m1) +
  labs(title = "Coefficient posterior distributions")
          
