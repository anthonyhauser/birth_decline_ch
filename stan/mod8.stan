//mod5 with year random effect, heteroscedasticity with GP, NB with variance mu + mu/phi 
functions {
  //see https://github.com/avehtari/casestudies/blob/master/Birthdays
  // basis function (exponentiated quadratic kernel)
  
  vector diagSPD_EQ(real alpha, real rho, real L, int M) {
    return alpha * sqrt(sqrt(2*pi()) * rho) * exp(-0.25*(rho*pi()/2/L)^2 * linspaced_vector(M, 1, M)^2);
  }
  matrix PHI_EQ(int N, int M, real L, vector x) {
    return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
  }
  
  matrix PHI_EQ_sin(int N, int M, real L, vector x) {
    return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
  }
  matrix PHI_EQ_sin2(int N, int M, real L, vector x) {
    return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x), M), linspaced_vector(M, 1, M)));
  }
  matrix PHI_EQ_cos(int N, int M, real L, vector x) {
    return cos(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
  }
  matrix PHI_EQ_cos2(int N, int M, real L, vector x) {
    return cos(diag_post_multiply(rep_matrix(pi()/(2*L) * (x), M), linspaced_vector(M, 1, M)));
  }
}

data {
  int<lower=1> N;
  int<lower=1> N_year1;
  int<lower=1> N_year2;
  int<lower=1> N_age;
  int<lower=1> N_age2;
  int<lower=1> N_month;
  
  //vector[N] n_birth;
  array[N] int n_birth;
  vector[N] n_pop;
  real log_mean_n_birth_n_pop;
  
  array[N] int year_id1;
  array[N] int year_id2;
  array[N] int age_id;
  array[N] int age_id2;
  array[N] int month_id;
  
  // Time points and corresponding locations
  vector[N_year1] x2;
  vector[N_age] x1;
  vector[N_age2] x3;
  
  // GP basis function settings
  real<lower=0> c_year;           // Boundary scaling factor
  int M_year;   
  real<lower=0> c_age;           // Boundary scaling factor
  int M_age;   
 
  
  //prior 
  real p_inv_sigma;
  //array[2] real p_delta0;
  array[2] real p_alpha;
  array[2] real p_alpha_year;
  array[2] real p_alpha_sigma;
  array[2] real p_gamma_month0;
  real rho;
  real lambda_year;
  real lambda_sigma;
  real p_sigma_year;

  int inference;
}

transformed data {
  // normalize data
  real x1_mean = mean(x1);
  real x1_sd = sd(x1);
  vector[N_age] xn1 = (x1 - x1_mean)/x1_sd;
  
  real x_mean = mean(x2);
  real x_sd = sd(x2);
  vector[N_year1] xn = (x2 - x_mean)/x_sd;
  
  real x3_mean = mean(x3);
  real x3_sd = sd(x3);
  vector[N_age2] xn3 = (x3 - x3_mean)/x3_sd;
  
  real s_year = sqrt(N_year2 / (N_year2 - 1.0));
  
  // compute boundary value
  real L_year = c_year*max(xn);
  real L_age = c_age*max(xn1);
  real L_age2 = c_age*max(xn3);
  
  // compute basis functions for f_year
  matrix[N_year1,M_year] PHI_year = PHI_EQ(N_year1, M_year, L_year, xn);
  matrix[N_age2,M_age] PHI_sigma = PHI_EQ(N_age2, M_age, L_age2, xn3);
  //f_age
  matrix[N_age,M_age] PHI_age_sin_x = PHI_EQ_sin(N_age, M_age, L_age, xn1);
  matrix[N_age,M_age] PHI_age_cos_x = PHI_EQ_cos(N_age, M_age, L_age, xn1);
}

parameters {
  real <lower=0> inv_sigma;
  vector[N_month-1] gamma_month0;
  //real delta0;
  
  vector[N_year2] b_year;
  real<lower=0> sigma_year;
  
  // GPs
  real<lower=0> alpha;
  real<lower=0> alpha_sigma;
  vector[M_age] beta_age;
  vector[M_age] beta_sigma;
  real <lower=0> alpha_year; // Yearly GP scale by age
  vector[M_year] beta_year; // Basis coefficients for yearly GP
}
transformed parameters {
  vector[M_year] diagSPD_year;
  vector[M_age] diagSPD_age;
  vector[M_age] diagSPD_sigma;
  vector[N_year1] f_year;
  matrix[N_age,N_year1] f_age;
  vector[N_age2] f_sigma;
  vector[N_month] gamma_month;
  
  gamma_month = to_vector(append_row(0.0, gamma_month0));
  
  // compute spectral densities for f_year
  diagSPD_year = diagSPD_EQ(alpha_year, lambda_year, L_year, M_year);
  f_year = PHI_year * (diagSPD_year .* beta_year);
  
  diagSPD_sigma = diagSPD_EQ(alpha_sigma, lambda_sigma, L_age2, M_age);
  f_sigma = PHI_sigma* (diagSPD_sigma .* beta_sigma);
  
  //f_age
  diagSPD_age = diagSPD_EQ(alpha, rho, L_age, M_age);
  {
    matrix[N_year1,M_age] PHI_age_sin_s = PHI_EQ_sin2(N_year1, M_age, L_age, f_year/x1_sd);
    matrix[N_year1,M_age] PHI_age_cos_s = PHI_EQ_cos2(N_year1, M_age, L_age, f_year/x1_sd);
    
    vector[M_age] vec = diagSPD_age .* beta_age;
    for(i in 1:N_year1){
      f_age[,i] = PHI_age_sin_x * (vec .* to_vector(PHI_age_cos_s[i,])) +
                  PHI_age_cos_x * (vec .* to_vector(PHI_age_sin_s[i,]));
    }
  }
  
  //sigma
  vector[N_age2] sigma = inv(inv_sigma) * exp(f_sigma);
}
model {
  //priors
  inv_sigma ~ exponential(p_inv_sigma);
  b_year ~ normal(0, s_year * sigma_year);//random effect
  sum(b_year) ~ normal(0, 0.001 * N_year2);//soft sum-to-zero constraint
  sigma_year ~ normal(0, p_sigma_year);
  //delta0 ~normal(p_delta0[1], p_delta0[2]);
  
 //GP: variance and lengthscale
  alpha ~ normal(p_alpha[1], p_alpha[2]);
  beta_age ~ std_normal();
  
  alpha_year ~ normal(p_alpha_year[1], p_alpha_year[2]);
  beta_year ~ normal(0, 1);
  
  alpha_sigma ~ normal(p_alpha_sigma[1], p_alpha_sigma[2]);
  beta_sigma ~ normal(0, 1);
  
  gamma_month0 ~ normal(p_gamma_month0[1], p_gamma_month0[2]);
  
  //for likelihood
  if(inference==1){
    vector[N] log_mu0;
    vector[N] sigma0;
    for(i in 1:N){
      log_mu0[i] = f_age[age_id[i],year_id1[i]] + gamma_month[month_id[i]] + log_mean_n_birth_n_pop + 
                   b_year[year_id2[i]] +  log(n_pop[i]);
      sigma0[i] = sigma[age_id2[i]] * exp(log_mu0[i]);
    }
    target += neg_binomial_2_log_lpmf(n_birth | log_mu0, sigma0);
  }
}

generated quantities{
  array[N_year1, N_age] real birth_prob;
  array[N] int n_birth_pred;
  array[N] int n_birth_pois_pred;
  vector[N_age] age_bias;
  
  //fertility curve
  for(i in 1:N_year1){
    for(j in 1:N_age){
         birth_prob[i,j] =  exp(f_age[j,i] +  log_mean_n_birth_n_pop);
    }
  }
  
  //predictions
  vector[N_year2] beta_year_rng;
  for(i in 1:N_year2){
    beta_year_rng[i] = normal_rng(0,s_year * sigma_year);
  }
  {
    vector[N] log_mu;
    vector[N] sigma2;
    for(i in 1:N){
      log_mu[i] = f_age[age_id[i],year_id1[i]] + gamma_month[month_id[i]] + log_mean_n_birth_n_pop + 
                   beta_year_rng[year_id2[i]] +  log(n_pop[i]);
      sigma2[i] = sigma[age_id2[i]] * exp(log_mu[i]);
    }
    n_birth_pred = neg_binomial_2_log_rng(log_mu, sigma2);
    n_birth_pois_pred = poisson_log_rng(log_mu);
  }

  //bias
  for(i in 1:N_age){
    age_bias[i] = 0.0;
  }
  for(i in 1:N){
    age_bias[age_id[i]] = age_bias[age_id[i]] + (n_birth_pred[i]-n_birth[i]);
  }
}
