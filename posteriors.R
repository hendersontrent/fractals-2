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

options(mc.cores = parallel::detectCores())

# Specify priors

b_condition_t2 <- paste0("normal(",priors$b_condition_t2_mean,",",priors$b_condition_t2_sd,")")
b_state_rest <- paste0("normal(",priors$b_state_rest_mean,",",priors$b_state_rest_sd,")")
b_minute <- paste0("normal(",priors$b_minute_mean,",",priors$b_minute_sd,")")
b_condition_t2_state_rest <- paste0("normal(",priors$b_condition_t2_state_rest_mean,",",priors$b_condition_t2_state_rest_sd,")")
b_condition_t2_minute <- paste0("normal(",priors$b_condition_t2_minute_mean,",",priors$b_condition_t2_minute_sd,")")
b_state_rest_minute <- paste0("normal(",priors$b_state_rest_minute_mean,",",priors$b_state_rest_minute_sd,")")
b_condition_t2_state_rest_minute <- paste0("normal(",priors$b_condition_t2_state_rest_minute_mean,",",priors$b_condition_t2_state_rest_minute_sd,")")

# Fit model

m1 <- brm(value ~ condition*state*minute,
          prior = c(set_prior(prior = b_condition_t2, class = "b", coef = "conditionT2"),
                    set_prior(prior = b_state_rest, class = "b", coef = "stateRest"),
                    set_prior(prior = b_minute, class = "b", coef = "minute"),
                    set_prior(prior = b_condition_t2_state_rest, class = "b", coef = "conditionT2:stateRest"),
                    set_prior(prior = b_condition_t2_minute, class = "b", coef = "conditionT2:minute"),
                    set_prior(prior = b_state_rest_minute, class = "b", coef = "stateRest:minute"),
                    set_prior(prior = b_condition_t2_state_rest_minute, class = "b", coef = "conditionT2:stateRest:minute")),
          data = tmp, iter = 2000, chains = 3, seed = 123)
          
#--------------------- Compute outputs & data vis ----------------------

# Diagnostic 1: Chain convergence

mcmc_trace(m1)

# Diagnostic 2: LOO

loo1 <- loo(m1, save_psis = TRUE)
plot(loo1)

# Diagnostic 3: Posterior predictive checks

CairoPNG("output/fractals_2_PPC.png",800,600)
pp_check(m1, nsamples = 100) +
  labs(title = "Posterior predictive check",
       x = "HRV",
       y = "Count")
dev.off()

# Summative data visualisation

CairoPNG("output/fractals_2_posterior.png",800,600)
mcmc_areas(m1, regex_pars = c("conditionT2", "stateRest", "minute",
                                  "conditionT2:stateRest", "conditionT2:minute", "stateRest:minute",
                                  "conditionT2:stateRest:minute"),
           area_method = "scaled height") +
  labs(title = "Coefficient posterior distributions")
dev.off()
