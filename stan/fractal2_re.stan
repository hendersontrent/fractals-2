//
// This Stan program aims to build an informed
// Bayesian linear regression of HRV data for Fractals 2.0
// using priors from Fractals 1.0 data but adding a random effects
// intercept to enable modelling of individual participants
//

//
// Author: Trent Henderson, 4 March 2021
//

data {
  
  // Variables
  
  int<lower=1> N; // number of observations
  vector[N] state; // State of person (Rest vs Meditation)
  vector[N] condition; // Condition of person (T1 vs T2)
  vector[N] minute; // Minute of the observed period
  vector[N] y; // The observed HRV measurements
  vector[N] g; // The random intercept identifer for participants
  
  // Priors
  
  real alpha_mean;
  real alpha_sd;
  real beta_1_mean;
  real beta_1_sd;
  real beta_2_mean;
  real beta_2_sd;
  real beta_3_mean;
  real beta_3_sd;
  real beta_4_mean;
  real beta_4_sd;
  real beta_5_mean;
  real beta_5_sd;
  real beta_6_mean;
  real beta_6_sd;
  real beta_7_mean;
  real beta_7_sd;
}

transformed data {
  
  // Interaction terms
  
  vector[N] inter_1;
  vector[N] inter_2;
  vector[N] inter_3;
  vector[N] inter_4;
  
  inter_1 = state .* condition;
  inter_2 = state .* minute;
  inter_3 = condition .* minute;
  inter_4 = state .* condition .* minute;
}

parameters {
  real alpha; // Intercept term
  real<lower=0> sigma; // Error SD for Gaussian likelihood
  real alpha_random[g]; // Random effects intercept
  
  // Coefficients
  
  real beta_1;
  real beta_2;
  real beta_3;
  real beta_4;
  real beta_5;
  real beta_6;
  real beta_7;
}

model {
  
  // Priors
  
  alpha ~ normal(alpha_mean,alpha_sd);
  beta_1 ~ normal(beta_1_mean,beta_1_sd);
  beta_2 ~ normal(beta_2_mean,beta_2_sd);
  beta_3 ~ normal(beta_3_mean,beta_3_sd);
  beta_4 ~ normal(beta_4_mean,beta_4_sd);
  beta_5 ~ normal(beta_5_mean,beta_5_sd);
  beta_6 ~ normal(beta_6_mean,beta_6_sd);
  beta_7 ~ normal(beta_7_mean,beta_7_sd);
  alpha_random ~ cauchy(3,2)
  
  // Log-likelihood
  
   y ~ normal(alpha + alpha_random[g] + beta_1*state + beta_2*condition + beta_3*minute + beta_4*inter_1 + beta_5*inter_2 + beta_6*inter_3 + beta_7*inter_4, sigma);
}

generated quantities {
  
  // Compute estimates of the likelihood for LOO-CV
  
  vector[N] log_lik; // Log-likelihood for LOO
  vector[N] y_rep; // PPC replications for model diagnostics

  for (n in 1:N) {
    real y_hat = alpha + alpha_random[g[n]] + beta_1*state[n] + beta_2*condition[n] + beta_3*minute[n] + beta_4*inter_1[n] + beta_5*inter_2[n] + beta_6*inter_3[n] + beta_7*inter_4[n];
    log_lik[n] = normal_lpdf(y[n] | y_hat, sigma);
    y_rep[n] = normal_rng(y_hat, sigma);
  }
}
