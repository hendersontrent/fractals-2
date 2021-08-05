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

m1 <- brm(value ~ 1 + state + condition + minute + state*condition*minute + (1 + minute | id),
          data = tmp2,
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


