// time series model for disease incidence
data {
  int<lower=0> n;           // N obs.
  array[n] int<lower=0> D;  // disease counts
  vector[n] P;              // population
  
  array[n] int<lower=1> T;  // time index
  int<lower=1> n_T;
  
  array[n] int<lower=1> L;  // location index
  int<lower=1> n_L;
  
  // penalized spline of season (month)
  int<lower=1> n_sp_s;     
  matrix[n, n_sp_s] sp_s; // spline basis
  int<lower=1> n_Omega_s;
  array[n_Omega_s] matrix[n_sp_s, n_sp_s] Omega_s; // penalty matrix
  
  // penalized spline of exposure
  int<lower=1> n_sp_E;
  matrix[n, n_sp_E] sp_E; // spline basis
  int<lower=1> n_Omega_E;
  array[n_Omega_E] matrix[n_sp_E, n_sp_E] Omega_E; // penalty matrix
}

parameters {
  real<lower=0> inv_phi; // overdispersion (global)
  vector[n_L] alpha;     // intercept (by location)
  
  // temporal trend (by location; non-centered)
  matrix[n_T, n_L] z_x;
  real<lower=0> sigma_x;  // sd of random walk (global)
  
  // spline basis coefficients (by location)
  matrix[n_sp_s, n_L] theta_s;
  matrix[n_sp_E, n_L] theta_E;
  
  // smoothing parameters for penalized splines (by location)
  matrix<lower=0>[n_Omega_E, n_L] lambda_E;
  matrix<lower=0>[n_Omega_s, n_L] lambda_s;
}

transformed parameters {
  // temporal trend as random walk (1)
  matrix[n_T, n_L] x;
  
  x[1, 1:n_L] = z_x[1, 1:n_L] .* sigma_x;  // element-wise product by location
  for (t in 2:n_T) {
    x[t, 1:n_L] = x[t-1, 1:n_L] + z_x[t, 1:n_L] .* sigma_x;
  }
}

model {
  vector[n] log_mu;  // rate for population P
  vector[n] log_eta; // rate per 1e5 people
  
  // likelihood --------------------------------------------------------------------------
  for (i in 1:n) {
    log_eta[i] = alpha[L[i]] + x[T[i], L[i]] + sp_s[i, :]*theta_s[:, L[i]]  +
                 sp_E[i, :]*theta_E[:, L[i]];
  }
  log_mu = log(P/1e5) + log_eta;
  D ~ neg_binomial_2_log(log_mu, inv(inv_phi));
  
  // priors ------------------------------------------------------------------------------
  
  // penalty matrices for splines (by location)
  array[n_L] matrix[n_sp_s, n_sp_s] Omega_s_sum;  // season
  array[n_L] matrix[n_sp_E, n_sp_E] Omega_E_sum;  // exposure
  
  inv_phi ~ exponential(1);  // overdispersion
  sigma_x ~ exponential(2);  // temporal trend sd
  
  // location-dependent parameters
  for (l in 1:n_L) {
    
    alpha[l] ~ normal(5, 1);  // intercept
    
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
    
    // exposure
    Omega_E_sum[l] = rep_matrix(0, n_sp_E, n_sp_E); // penalty matrix
    for (o in 1:n_Omega_E) {
      Omega_E_sum[l] = Omega_E_sum[l] + Omega_E[o].*lambda_E[o,l];
    };
    theta_E[:,l] ~ multi_normal_prec(               // coefficients
        rep_vector(0, n_sp_E), Omega_E_sum[l]
    );
    lambda_E[:,l] ~ gamma(10*0.005, 0.005);         // smoothing parameter
  }
}

generated quantities {
}
