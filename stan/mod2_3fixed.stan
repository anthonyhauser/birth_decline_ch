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
  
  vector log_birth_prob(vector age_id, real a_peak, real log_h_peak, real sigma) {
    return log_h_peak - (age_id - a_peak)^2 / (2*sigma^2);
  }
  
  real log_birth_prob2(real age_id, real a_peak,real log_h_peak, real sigma) {
    return log_h_peak - (age_id - a_peak)^2 / (2*sigma^2);
  }
}

data {
  int<lower=1> N;
  int<lower=1> N_year;//used for scale GP
  int<lower=1> N_month;
  int<lower=1> N_age;
  
  //vector[N] n_birth;
  array[N] int n_birth;
  vector[N] n_pop;
  
  array[N] int month_id;
  array[N] int age_id;
  
  // Hyperprior parameters
  array[2] real p_age_peak1;
  array[2] real p_log_h_peak1;
  array[2] real p_birth_prob_sigma1;
  
  real p_inv_sigma;
  
  int inference;
}

parameters {
  real <lower=0> inv_sigma;
  
  real <lower=0> age_peak1;
  real log_h_peak1;
  real <lower=0> birth_prob_sigma1;
}
transformed parameters {
  real sigma = 1/(inv_sigma)^2;
}
model {
  inv_sigma ~ normal(0,p_inv_sigma);
  
  // weak priors
  age_peak1 ~ normal(p_age_peak1[1],p_age_peak1[2]);
  log_h_peak1 ~ normal(p_log_h_peak1[1],p_log_h_peak1[2]);
  birth_prob_sigma1 ~ normal(p_birth_prob_sigma1[1],p_birth_prob_sigma1[2]);
  
  if(inference==1){
    vector[N] log_mu;
      log_mu = log_birth_prob(to_vector(age_id), age_peak1,//scale by N_month might decrease the risk of divergences
                                                              log_h_peak1,
                                                              birth_prob_sigma1) + log(n_pop);
    target += neg_binomial_2_log_lpmf(n_birth |log_mu, sigma);
  }
}

generated quantities{
  array[N_month, N_age] real birth_prob;

  for(i in 1:N_month){
    for(j in 1:N_age){
      birth_prob[i,j] = exp(log_birth_prob2(j, age_peak1,
                                            log_h_peak1,
                                            birth_prob_sigma1));
    }
  }

  vector[N] log_mu;
  array[N] int n_birth_pred;
  array[N] int n_birth_pois_pred;

  log_mu = log_birth_prob(to_vector(age_id),
                          age_peak1,
                          log_h_peak1,
                          birth_prob_sigma1) + log(n_pop);
  n_birth_pred = neg_binomial_2_log_rng(log_mu, sigma*exp(log_mu));
  n_birth_pois_pred = poisson_log_rng(log_mu);
}
