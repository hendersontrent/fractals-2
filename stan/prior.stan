//
// This Stan program aims to build a basic uninformed
// Bayesian linear regression of HRV data for Fractals 2.0
//

//
// Author: Trent Henderson, 4 March 2021
//

data {
  int<lower=1> N; // number of observations
  vector[N] state; // State of person (Rest vs Meditation)
  vector[N] condition; // Condition of person (T1 vs T2)
  vector[N] minute; // Minute of the observed period
  vector[N] y; // The observed HRV measurements
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
  vector[7] beta; // Regression coefficients
  real<lower=0> sigma; // error sd for Gaussian likelihood
}

model {
  
  // Priors
  
  alpha ~ normal(3,2);
  beta ~ normal(0,5);
  
  // Log-likelihood
  
   y ~ normal(alpha + beta[1]*state + beta[2]*condition + beta[3]*minute + beta[4]*inter_1 + beta[5]*inter_2 + beta[6]*inter_3 + beta[7]*inter_4, sigma);
}

generated quantities {
  
  // Compute estimates of the likelihood for LOO-CV
  
  vector[N] log_lik; // Log-likelihood for LOO
  vector[N] y_rep; // PPC replications for model diagnostics

  for (n in 1:N) {
    real y_hat = alpha + beta[1]*state[n] + beta[2]*condition[n] + beta[3]*minute[n] + beta[4]*inter_1[n] + beta[5]*inter_2[n] + beta[6]*inter_3[n] + beta[7]*inter_4[n];
    log_lik[n] = normal_lpdf(y[n] | y_hat, sigma);
    y_rep[n] = normal_rng(y_hat, sigma);
  }
}
