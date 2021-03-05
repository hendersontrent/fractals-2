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

d <- read_csv("data/fractals_1_1minutebins.csv") %>%
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
  mutate(state = ifelse(state == "Rest", 1, 2),
         condition = ifelse(condition == "T1", 1, 2))

#----------------------- Specify a Bayesian model ----------------------

options(mc.cores = parallel::detectCores())

# Build model with vague and uninformative priors

stan_data <- list(N = nrow(tmp),
                  state = tmp$state,
                  condition = tmp$condition,
                  minute = tmp$minute,
                  y = tmp$value)

m1 <- stan(file = "stan/prior.stan",
           data = stan_data, iter = 2000, chains = 3, seed = 123)

# Visualise distribution of coefficient posteriors to get correct prior shape for Fractals 2.0

CairoPNG("output/fractals_1_posterior.png",800,600)
as.data.frame(m1) %>%
  clean_names() %>%
  dplyr::select(c(2:8)) %>%
  gather(key = parameter, value = value, 1:7) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue2") +
  labs(title = "Posterior distributions for Fractals 1.0",
       x = "Coefficient Value",
       y = "Frequency") +
  theme_bw() +
  facet_wrap(~parameter)
dev.off()

#----------------------- Save priors for future use -------------------

options(mc.cores = parallel::detectCores())

# Save as .Rda to avoid needing to re-run model which can take a long time

priors <- as.data.frame(m1) %>%
  clean_names() %>%
  dplyr::select(c(1:8)) %>%
  summarise(alpha_mean = mean(alpha), alpha_sd = sd(alpha),
            beta_2_mean = mean(beta_2), beta_2_sd = sd(beta_2),
            beta_1_mean = mean(beta_1), beta_1_sd = sd(beta_1),
            beta_3_mean = mean(beta_3), beta_3_sd = sd(beta_3),
            beta_4_mean = mean(beta_4), beta_4_sd = sd(beta_4),
            beta_6_mean = mean(beta_6), beta_6_sd = sd(beta_6),
            beta_5_mean = mean(beta_5), beta_5_sd = sd(beta_5),
            beta_7_mean = mean(beta_7), beta_7_sd = sd(beta_7))

save(priors, file = "data/priors.Rda")
