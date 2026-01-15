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
  // vector diagSPD_periodic(real alpha, real lambda, int M) {
  //   real a = 1/lambda^2;
  //   array[M] int one_to_M;
  //   for (m in 1:M) one_to_M[m] = m;
  //   vector[M] q = sqrt(alpha^2 * 2 / exp(a) * to_vector(modified_bessel_first_kind(one_to_M, a)));
  //   return append_row(q,q);
  // }
  
  matrix PHI_periodic(int N, int M, real w0, vector x) {
    matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector(M, 1, M));
    return append_col(cos(mw0x), sin(mw0x));
  }
  
  
  vector log_birth_prob(vector age_id, vector a_peak, vector h_peak, real sigma) {
    return log(h_peak) - (age_id - a_peak)^2 / (2*sigma^2);
  }
  
  real log_birth_prob2(real age_id, real a_peak,real h_peak, real sigma) {
    return log(h_peak) - (age_id - a_peak)^2 / (2*sigma^2);
  }
  

}

data {
  int<lower=1> N;
  int<lower=1> N_year;
  int<lower=1> N_age;
  int<lower=1> N_group_year;
  
  //vector[N] n_birth;
  array[N] int n_birth;
  vector[N] n_pop;
  
  array[N] int year_id;
  array[N] int age_id;
  array[N] int group_year_id;
  
  // Time points and corresponding locations
  vector[N_year] x;
  vector[N_age] y;
  
  // GP basis function settings
  real<lower=0> c_year;           // Boundary scaling factor
  int M_year;                     // Number of EQ basis functions
  real<lower=0> c_age;           // Boundary scaling factor
  int M_age;                     // Number of EQ basis functions
  
  // Hyperprior parameters
  array[2] real p_age_peak1;    // Prior for yearly GP lengthscale
  array[2] real p_age_peak2;    // Prior for yearly GP lengthscale
  array[2] real p_h_peak1;     // Prior for yearly GP scale
  array[2] real p_h_peak2;     // Prior for yearly GP scale
  array[2] real p_birth_prob_sigma;    // Prior for yearly GP lengthscale
  array[2] real p_alpha_year;     // Prior for yearly GP scale
  array[2] real p_lambda_year;    // Prior for yearly GP lengthscale
  array[2] real p_alpha_age;     // Prior for yearly GP scale
  array[2] real p_lambda_age;    // Prior for yearly GP lengthscale
  real p_inv_sigma;
  
  int inference;
}

transformed data {

}

parameters {
  real <lower=0> inv_sigma;
  
  real <lower=0> age_peak1;
  real age_peak2;
  real <lower=0> h_peak1;
  real h_peak2;
  real <lower=0> birth_prob_sigma;
}
transformed parameters {
  real sigma = inv(inv_sigma);
  
  vector[N] h_peak;
  for(i in 1:N){
    h_peak[i] = max([0,h_peak1+ h_peak2 * (year_id[i])]);
  }
}
model {
  inv_sigma ~ exponential(p_inv_sigma);
  
  // weak priors
  age_peak1 ~ normal(p_age_peak1[1],p_age_peak1[2]);
  age_peak2 ~ normal(p_age_peak2[1],p_age_peak2[2]);
  h_peak1 ~ normal(p_h_peak1[1],p_h_peak1[2]);
  h_peak2 ~ normal(p_h_peak2[1],p_h_peak2[2]);
  birth_prob_sigma ~ normal(p_birth_prob_sigma[1],p_birth_prob_sigma[2]);

  
  if(inference==1){
    for(g in 1:N_group_year){
      //target += normal_lpdf(log(n_birth + 1) |intercept + f_year[year_id] + f_age[age_id] + log(n_pop),sigma); 
      target += neg_binomial_2_log_lpmf(n_birth |log_birth_prob(to_vector(age_id), age_peak1 + age_peak2 * to_vector(year_id),
                                                                h_peak,
                                                                birth_prob_sigma) + log(n_pop), sigma);
    }
  }
  
}

generated quantities{
  array[N_year, N_age] real birth_prob;
  
  for(i in 1:N_year){
    for(j in 1:N_age){
      birth_prob[i,j] = exp(log_birth_prob2(j, age_peak1 + age_peak2 * i,
                                            h_peak1+ h_peak2 * i,
                                            birth_prob_sigma));
    }
  }
}
