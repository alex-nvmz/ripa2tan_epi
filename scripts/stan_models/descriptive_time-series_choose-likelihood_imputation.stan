// time series model for disease incidence
data {
  int<lower=0> n;           // N obs.
  
  // imputation setup
  int n_obs;              // N observed values   
  int n_miss;             // N missing values
  array[n_obs] int obs;   // indices of observed Y
  array[n_miss] int miss; // indices of missing Y
  array[n_obs] real Y_obs;    // observed outcome
  
  vector[n] offset;         // offset (population for disease count models)
  
  array[n] int<lower=1> T;  // time index
  int<lower=1> n_T;
  
  array[n] int<lower=1> L;  // location index
  int<lower=1> n_L;
  
  // penalized spline of season (month)
  int<lower=1> n_sp_s;     
  matrix[n, n_sp_s] sp_s; // spline basis
  int<lower=1> n_Omega_s;
  array[n_Omega_s] matrix[n_sp_s, n_sp_s] Omega_s; // penalty matrix
  
  // likelihood options
  int<lower=1, upper=3> family; // 1 = Normal, 2 = Gamma, 3 = Negative Binomial
}

transformed data {
  // transform Y to int for count likelihoods
  array[n_obs] int Y_obs_int = to_int(Y_obs);
}

parameters {
  
  real<lower=0> phi; // dispersion (global), for any likelihood
  
  vector[n_L] alpha; // intercept (by location)
  
  // temporal trend (by location; non-centered)
  matrix[n_T, n_L] z_x;
  real<lower=0> sigma_x;  // sd of random walk (global)
  
  // spline basis coefficients (by location)
  matrix[n_sp_s, n_L] theta_s;
  
  // smoothing parameters for penalized splines (by location)
  matrix<lower=0>[n_Omega_s, n_L] lambda_s;
}

transformed parameters {
  // temporal trend as random walk (1)
  matrix[n_T, n_L] x;
  
  x[1, 1:n_L] = z_x[1, 1:n_L] .* sigma_x;  // element-wise product by location
  for (t in 2:n_T) {
    x[t, 1:n_L] = x[t-1, 1:n_L] + z_x[t, 1:n_L] .* sigma_x;
  }
  
  // ----------------------------------------
  
  // define latent parameters (for all observations, observed and missing)
  vector[n] eta; // linear predictor
  vector[n] mu; // expected value
  
  // linear predictor
  for (i in 1:n) {
    eta[i] = alpha[L[i]] + x[T[i], L[i]] + sp_s[i, :]*theta_s[:, L[i]];
  }
  
  // Normal
  if (family == 1) {
    mu = offset + eta;
  
  // Gamma
  } else if (family == 2) {
    mu = exp(offset + eta);
  
  // Negative Binomial
  } else if (family == 3) {
    mu = offset + eta; // actually log_mu
  }

}

model {
  // likelihood --------------------------------------------------------------------------
  
  // different intercept priors depending if link function is identity or log
  // define likelihood for observed Y (the rest are just sampled from the prior)
  
  // Normal
  if (family == 1) {
    Y_obs ~ normal(mu[obs], phi);
    
    alpha ~ normal(0, 1);  // intercept (standardized Y)
    
  // Gamma
  } else if (family == 2) {
    
    real alpha_gamma = 1 / phi;
    vector[n] beta = 1 / (phi * mu);
    
    Y_obs ~ gamma(alpha_gamma, beta[obs]);
    
    alpha ~ normal(5, 1);  // intercept
    
  // Negative Binomial
  } else if (family == 3) {
    Y_obs_int ~ neg_binomial_2_log(mu[obs], 1 / phi); // precision (1 / phi)
    
    alpha ~ normal(5, 1);  // intercept
  }
  
  // priors ------------------------------------------------------------------------------
  
  // penalty matrices for splines (by location)
  array[n_L] matrix[n_sp_s, n_sp_s] Omega_s_sum;  // season
    
  phi ~ exponential(1);  // dispersion
  sigma_x ~ exponential(2);  // temporal trend sd
  
  // location-dependent parameters
  for (l in 1:n_L) {
    
    z_x[:,l] ~ normal(0, 1); // temporal trend
    
    // penalized splines
    
    // season
    Omega_s_sum[l] = rep_matrix(0, n_sp_s, n_sp_s);  // penalty matrix
    for (o in 1:n_Omega_s) {
      Omega_s_sum[l] = Omega_s_sum[l] + Omega_s[o].*lambda_s[o,l];
    };
    theta_s[:,l] ~ multi_normal_prec(               // coefficients
      rep_vector(0, n_sp_s), Omega_s_sum[l]
    );
    lambda_s[:,l] ~ gamma(10*0.005, 0.005);         // smoothing parameter
    
  }
}

generated quantities {
  // get imputations
  array[n_miss] real Y_miss_pred;
  
  if (family == 1) {
    Y_miss_pred = normal_rng(mu[miss], phi);
    
  // Gamma
  } else if (family == 2) {
    
    real alpha_gamma = 1 / phi;
    vector[n] beta = 1 / (phi * mu);
    
    Y_miss_pred = gamma_rng(alpha_gamma, beta[miss]);
    
  // Negative Binomial
  } else if (family == 3) {
    
    Y_miss_pred = neg_binomial_2_log_rng(mu[miss], 1 / phi);
    
  }
  
}
