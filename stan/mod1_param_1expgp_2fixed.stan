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
  vector diagSPD_periodic(real alpha, real lambda, int M) {
    real a = 1/lambda^2;
    vector[M] q = exp(log(alpha) + 0.5 * (log(2) - a + to_vector(log_modified_bessel_first_kind(linspaced_int_array(M, 1, M), a))));
    return append_row(q,q);
  }
  
  matrix PHI_periodic(int N, int M, real w0, vector x) {
    matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector(M, 1, M));
    return append_col(cos(mw0x), sin(mw0x));
  }
  
  
  vector log_birth_prob(vector age_id, vector a_peak, real log_h_peak, real sigma) {
    return log_h_peak - (age_id - a_peak)^2 / (2*sigma^2);
  }
  
  real log_birth_prob2(real age_id, real a_peak,real log_h_peak, real sigma) {
    return log_h_peak - (age_id - a_peak)^2 / (2*sigma^2);
  }
  

}

data {
  int<lower=1> N;
  int<lower=1> N_year;
  int<lower=1> N_age;
  
  //vector[N] n_birth;
  array[N] int n_birth;
  vector[N] n_pop;
  
  array[N] int year_id;
  array[N] int age_id;
  
  // Time points and corresponding locations
  vector[N_year] x;
  
  // GP basis function settings
  real<lower=0> c_year;           // Boundary scaling factor
  int M_year;                     // Number of EQ basis functions
  
  // Hyperprior parameters
  array[2] real p_age_peak1;
  array[2] real p_log_h_peak1;
  array[2] real p_birth_prob_sigma1;
  
  array[2] real p_alpha_year;
  array[2] real p_lambda_year;
  
  real p_inv_sigma;
  
  int inference;
}

transformed data {
  // normalize data
  real x_mean = mean(x);
  real x_sd = sd(x);
  vector[N_year] xn = (x - x_mean)/x_sd;
  
  // compute boundary value
  real L_year = c_year*max(xn);
  
  // compute basis functions for f
  matrix[N_year,M_year] PHI_year = PHI_EQ(N_year, M_year, L_year, xn);
}

parameters {
  real <lower=0> inv_sigma;
  
  real <lower=0> age_peak1;
  real log_h_peak1;
  real <lower=0> birth_prob_sigma1;
  
  // GPs
  real <lower=0> alpha_year;       // Yearly GP scale by age
  real <lower=0> lambda_year;      // Yearly GP lengthscale by age
  vector[M_year] beta_year; // Basis coefficients for yearly GP
}
transformed parameters {
  vector[M_year] diagSPD_year;
  vector[N_year] f_year;
  
  // compute spectral densities for f
  diagSPD_year = diagSPD_EQ(alpha_year, lambda_year, L_year, M_year);
  // compute f
  f_year = PHI_year * (diagSPD_year .* beta_year);
  
  real sigma = inv(inv_sigma);
}
model {
  inv_sigma ~ exponential(p_inv_sigma);
  
  // weak priors
  age_peak1 ~ normal(p_age_peak1[1],p_age_peak1[2]);
  log_h_peak1 ~ normal(p_log_h_peak1[1],p_log_h_peak1[2]);
  birth_prob_sigma1 ~ normal(p_birth_prob_sigma1[1],p_birth_prob_sigma1[2]);
  
  //GP: variance and lengthscale
  lambda_year ~ lognormal(p_lambda_year[1], p_lambda_year[2]);
  alpha_year ~ normal(p_alpha_year[1], p_alpha_year[2]);
  beta_year ~ normal(0, 1);
  
  if(inference==1){
    target += neg_binomial_2_log_lpmf(n_birth |log_birth_prob(to_vector(age_id), age_peak1 * exp(f_year[year_id]/N_year),//scale by N_age might decrease the risk of divergences
                                                              log_h_peak1,
                                                              birth_prob_sigma1) + log(n_pop), sigma);
  }
}

generated quantities{
  array[N_year, N_age] real birth_prob;
  
  for(i in 1:N_year){
    for(j in 1:N_age){
      birth_prob[i,j] = exp(log_birth_prob2(j, age_peak1 * exp(f_year[i]/N_year),
                                            log_h_peak1,
                                            birth_prob_sigma1));
    }
  }
}
