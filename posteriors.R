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
  mutate(state = ifelse(state == "Rest", 1, 2),
         condition = ifelse(condition == "T1", 1, 2))

#----------------------- Specify a Bayesian model ----------------------

options(mc.cores = parallel::detectCores())

# Fit model

stan_data <- list(N = nrow(tmp),
                  state = tmp$state,
                  condition = tmp$condition,
                  minute = tmp$minute,
                  y = tmp$value,
                  alpha_mean = priors$alpha_mean, alpha_sd = priors$alpha_sd,
                  beta_1_mean = priors$beta_1_mean, beta_1_sd = priors$beta_1_sd,
                  beta_2_mean = priors$beta_2_mean, beta_2_sd = priors$beta_2_sd,
                  beta_3_mean = priors$beta_3_mean, beta_3_sd = priors$beta_3_sd,
                  beta_4_mean = priors$beta_4_mean, beta_4_sd = priors$beta_4_sd,
                  beta_5_mean = priors$beta_5_mean, beta_5_sd = priors$beta_5_sd,
                  beta_6_mean = priors$beta_6_mean, beta_6_sd = priors$beta_6_sd,
                  beta_7_mean = priors$beta_7_mean, beta_7_sd = priors$beta_7_sd)

m2 <- stan(file = "stan/fractal2.stan",
           data = stan_data, iter = 2000, chains = 3, seed = 123)
          
#--------------------- Compute outputs & data vis ----------------------

# Diagnostic 1: Chain convergence

CairoPNG("output/fractals_2_traceplot.png",800,600)
mcmc_trace(m2, regex_pars = c("beta_"))
dev.off()

# Diagnostic 2: LOO

CairoPNG("output/fractals_2_loo.png",800,600)
loo1 <- loo(m2, save_psis = TRUE)
plot(loo1)
dev.off()

# Diagnostic 3: Posterior predictive checks

CairoPNG("output/fractals_2_PPC.png",800,600)
set.seed(123)
y <- tmp$value
yrep <- extract(m2)[["y_rep"]]
samp100 <- sample(nrow(yrep), 100)
ppc_dens_overlay(y, yrep[samp100, ]) +
  labs(title = "Posterior predictive check",
       x = "HRV",
       y = "Count")
dev.off()

# Summative data visualisations

CairoPNG("output/fractals_2_posterior.png",800,600)
stan_hist(m2, pars = c("beta_1", "beta_2", "beta_3", 
                       "beta_4", "beta_5", "beta_6", "beta_7")) +
  labs(title = "Coefficient posterior distributions") +
  geom_vline(xintercept = 0, lty = "dashed", colour = "grey50", size = 1)
dev.off()

# Posterior table

posts <- as.data.frame(m2) %>%
  dplyr::select(c(3:9)) %>%
  gather(key = coefficient, value = value, 1:7) %>%
  group_by(coefficient) %>%
  summarise(estimate = mean(value),
            lower = quantile(value, probs = 0.05),
            upper = quantile(value, probs = 0.95)) %>%
  ungroup()
