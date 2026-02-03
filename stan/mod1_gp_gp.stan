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
  
  array[] int lower_lim(vector age_shift, int N_year1) {
    //floor of age_shift
    array[N_year1] int lw;
    for(i in 1:N_year1){
       lw[i] = 0;
      if(age_shift[i]>=0){
        while (age_shift[i] >= lw[i] + 1) {
          lw[i] = lw[i] + 1;
        }
      }else{
        while (age_shift[i] < lw[i]) {
          lw[i] = lw[i] - 1;
        } 
      }
    }
    return(lw);
  }
  
  vector logit_birth_prob(int N, int N_age, array[] int age_id, array[] int lw, vector age_shift, vector logit_birth_prob_int) {
    vector[N] frac = age_shift-to_vector(lw);
    vector[N] logit_birth_prob_shift;
    for(i in 1:N){
      int idx = age_id[i] - lw[i];
      
      if (idx <= 1) {
        logit_birth_prob_shift[i] = logit_birth_prob_int[1];
      }
      else if (idx >= N_age) {
        logit_birth_prob_shift[i] = logit_birth_prob_int[N_age];
      }
      else {
        logit_birth_prob_shift[i] =
          logit_birth_prob_int[idx - 1] * frac[i] +
          logit_birth_prob_int[idx] * (1.0 - frac[i]);
      }
    }
    return(logit_birth_prob_shift);
  }
  
  real logit_birth_prob2(int N, int N_age,  int age_id, int lw, real age_shift, vector logit_birth_prob_int) {
   int idx = age_id - lw;
    real frac = age_shift-lw;
    real logit_birth_prob_shift;
    if (idx <= 1) logit_birth_prob_shift = logit_birth_prob_int[1];
    else if (idx >= N_age) logit_birth_prob_shift = logit_birth_prob_int[N_age];
    else logit_birth_prob_shift =  logit_birth_prob_int[age_id - lw-1] *  frac + logit_birth_prob_int[age_id - lw] * (1.0 - frac);
  
    return(logit_birth_prob_shift);
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
  vector[N_year1] x2;
  
  // GP basis function settings
  real<lower=0> c_year;           // Boundary scaling factor
  int M_year;   
  real<lower=0> c_age;           // Boundary scaling factor
  int M_age;   
  
  vector[N_age] x1;
  
  real p_inv_sigma;
  
  array[2] real p_delta0;
  
 
  real rho;
  real lambda_year;      // Yearly GP lengthscale by age
  //array[2] real p_rho;
  
  array[2] real p_alpha;
  array[2] real p_alpha_year;
  
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
  
  // compute boundary value
  real L_year = c_year*max(xn);
  real L_age = c_age*max(xn1);
  
  // compute basis functions for f
  matrix[N_year1,M_year] PHI_year = PHI_EQ(N_year1, M_year, L_year, xn);
  matrix[N_age,M_age] PHI_age = PHI_EQ(N_age, M_age, L_age, xn1);
}

parameters {
  //real<lower=0> rho;
  real<lower=0> alpha;
  vector[M_age] beta_age;
  
  //real beta;
 
  vector <lower=0>[N_sigma] inv_sigma;
  
  real delta0;
  
  // GPs
  real <lower=0> alpha_year;       // Yearly GP scale by age
  vector[M_year] beta_year; // Basis coefficients for yearly GP
}
transformed parameters {
  vector[M_year] diagSPD_year;
  vector[M_age] diagSPD_age;
  vector[N_year1] f_year;
  vector[N_age] f_age;
  
  // compute spectral densities for f
  diagSPD_year = diagSPD_EQ(alpha_year, lambda_year, L_year, M_year);
  diagSPD_age = diagSPD_EQ(alpha, rho, L_age, M_age);
  // compute f
  f_year = PHI_year * (diagSPD_year .* beta_year);
  f_age = PHI_age * (diagSPD_age .* beta_age);
  
  vector[N_sigma] sigma = inv(inv_sigma);
  vector[N_age] logit_birth_prob_int = f_age + delta0;
}
model {
  inv_sigma ~ exponential(p_inv_sigma);
  
  //beta ~ normal(0,1);
  
  //rho ~ inv_gamma(p_rho[1], p_rho[2]);
  alpha ~ normal(p_alpha[1], p_alpha[2]);
  beta_age ~ std_normal();
  
  //priors
  delta0 ~normal(p_delta0[1], p_delta0[2]);
  
  //GP: variance and lengthscale
  alpha_year ~ normal(p_alpha_year[1], p_alpha_year[2]);
  beta_year ~ normal(0, 1);

  
  if(inference==1){
    target += neg_binomial_2_log_lpmf(n_birth | log(inv_logit(logit_birth_prob(N, N_age, age_id, (lower_lim(f_year, N_year1))[year_id1],
                                                              f_year[year_id1], logit_birth_prob_int))) + log(n_pop), sigma[sigma_id]);
    
  }
}

generated quantities{
  
  array[N_year1, N_age] real birth_prob;
  array[N] int n_birth_pred;
  array[N] int n_birth_pois_pred;
  {
    array[N_year1] int lw2 = (lower_lim(f_year, N_year1));
  
    for(i in 1:N_year1){
      for(j in 1:N_age){
        birth_prob[i,j] =  inv_logit(logit_birth_prob2(N, N_age, j, lw2[i], f_year[i], logit_birth_prob_int));
      }
    }
  
    vector[N] log_mu = log(inv_logit(logit_birth_prob(N, N_age, age_id, lw2[year_id1], f_year[year_id1], logit_birth_prob_int))) + log(n_pop);
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
