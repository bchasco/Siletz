data {
  int<lower=0> n; // Number of years
  vector[n] y; // Year
}
parameters {
  real<lower=-20, upper=20> theta_log;
  real<lower=-20, upper=20> sigma_log;
}

model {
  // Implicit uniform priors are used.
  // Likelihood
  real theta = exp(theta_log);
  real sigma = exp(sigma_log);
  
  y ~ normal( theta , sigma );
  
}
