functions {
  //see https://github.com/avehtari/casestudies/blob/master/Birthdays
  // basis function (exponentiated quadratic kernel)
    matrix PHI_EQ(int N, int M, real L, vector x) {
      matrix[N,M] A = rep_matrix(pi()/(2*L) * (x+L), M);
      vector[M] B = linspaced_vector(M, 1, M);
      matrix[N,M] PHI = sin(diag_post_multiply(A, B))/sqrt(L);
      for (m in 1:M) PHI[,m] = PHI[,m] - mean(PHI[,m]); // scale to have mean 0
      return PHI;
    }
  // spectral density (exponentiated quadratic kernel)
  vector diagSPD_EQ(real alpha, real lambda, real L, int M) {
    vector[M] B = linspaced_vector(M, 1, M);
    return sqrt( alpha^2 * sqrt(2*pi()) * lambda * exp(-0.5*(lambda*pi()/(2*L))^2*B^2) );
  }
}

data {
  int<lower=1> N;
  int<lower=1> N_year1;
  int<lower=1> N_age;
  int<lower=1> N_sigma;
  
  //vector[N] n_birth;
  array[N] int n_birth;
  vector[N] n_pop;
  
  array[N] int year_id1;
  array[N] int age_id;
  array[N] int sigma_id;
  
  // Time points and corresponding locations
  vector[N_age] x1;
  
  // GP basis function settings
  real<lower=0> c_age;           // Boundary scaling factor
  int M_age;
  
  //prior 
  real p_inv_sigma;
  array[2] real p_delta0;
  array[2] real p_alpha;
  array[2] real p_rho;

  int inference;
}

transformed data {
  // normalize data
  real x1_mean = mean(x1);
  real x1_sd = sd(x1);
  vector[N_age] xn1 = (x1 - x1_mean)/x1_sd;
  
  // compute boundary value
  real L_age = c_age*max(xn1);
  
  // compute basis functions for f_year
  matrix[N_age,M_age] PHI_age = PHI_EQ(N_age, M_age, L_age, xn1);
}

parameters {
  vector <lower=0>[N_sigma] inv_sigma;
  real delta0;
  
  // GPs
  real<lower=0> alpha;
  real<lower=0> rho;
  vector[M_age] beta_age;
}
transformed parameters {
  vector[M_age] diagSPD_age;
  vector[N_age] f_age;
  
  // compute basis functions and spectral density for f_age
  diagSPD_age = diagSPD_EQ(alpha, rho, L_age, M_age);
  f_age = PHI_age * (diagSPD_age .* beta_age);

  //sigma
  vector[N_sigma] sigma = inv(inv_sigma);
}
model {
  //priors
  inv_sigma ~ exponential(p_inv_sigma);
  delta0 ~normal(p_delta0[1], p_delta0[2]);
  
 //GP: variance and lengthscale
  alpha ~ normal(p_alpha[1], p_alpha[2]);
  rho ~ normal(p_rho[1], p_rho[2]);
  beta_age ~ std_normal();
  
  if(inference==1){
    target += neg_binomial_2_log_lpmf(n_birth | delta0 + f_age[age_id] + log(n_pop), sigma[sigma_id]);
    
  }
}

generated quantities{
  array[N_year1, N_age] real logit_birth_prob;
  array[N_year1, N_age] real birth_prob;
  array[N] int n_birth_pred;
  array[N] int n_birth_pois_pred;
  {
    for(i in 1:N_year1){
      for(j in 1:N_age){
         logit_birth_prob[i,j] =  f_age[j] + delta0;
         birth_prob[i,j] =  exp(f_age[j] + delta0);
      }
    }
    vector[N] log_mu;
    for(i in 1:N){
      log_mu[i] = f_age[age_id[i]] + delta0 + log(n_pop[i]);
    }
    n_birth_pred = neg_binomial_2_log_rng(log_mu, sigma[sigma_id]);
    n_birth_pois_pred = poisson_log_rng(log_mu);
  }

  vector[N_age] age_bias;
  for(i in 1:N_age){
    age_bias[i] = 0.0;
  }
  for(i in 1:N){
    age_bias[age_id[i]] = age_bias[age_id[i]] + (n_birth_pred[i]-n_birth[i]);
  }
}
