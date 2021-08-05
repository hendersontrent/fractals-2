#---------------------------------------
# This script sets out to run a Bayesian
# mixed effects model to analyse HRV
# data
#
# NOTE: This script requires setup.R to
# have been run first
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 5 August 2021
#---------------------------------------

# Process data file into appropriate format

tmp <- process_hrv_data(data = "data/fractals_2_1minutebins.csv")

#---------------- Model specification ----------------

# Fit model

options(mc.cores = parallel::detectCores())

m1 <- brm(value ~ 1 + state + condition + minute + state*condition*minute + (1 + minute | id),
          data = tmp,
          family = gaussian,
          prior = c(set_prior(prior = "normal(5, 2)", class = "Intercept"),
                    set_prior(prior = "normal(0, 2.5)", class = "b", coef = "state"),
                    set_prior(prior = "normal(0, 2.5)", class = "b", coef = "condition"),
                    set_prior(prior = "normal(0, 2.5)", class = "b", coef = "minute"),
                    set_prior(prior = "normal(0, 2.5)", class = "b", coef = "condition:minute"),
                    set_prior(prior = "normal(0, 2.5)", class = "b", coef = "state:condition"),
                    set_prior(prior = "normal(0, 2.5)", class = "b", coef = "state:minute"),
                    set_prior(prior = "normal(0, 2.5)", class = "b", coef = "state:condition:minute")),
          iter = 3000,
          chains = 3,
          control = list(adapt_delta = 0.95, max_treedepth = 15),
          seed = 123)

#---------------- Data visualisation -----------------

# Posterior predictive checks

pp_check(m1, nsamples = 100)

# Chain convergence

mcmc_trace(m1, regex_pars = "b_")

# Coefficients

mcmc_areas(m1, pars = c("b_state", "b_condition", "b_minute",
                        "b_state:condition", "b_state:minute", "b_condition:minute",
                        "b_state:condition:minute"), area_method = "scaled height",
           prob = 0.8)
