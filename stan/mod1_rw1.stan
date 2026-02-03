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
  
  array[] int lower_lim(vector age_shift, int N_year) {
    //floor of age_shift
    array[N_year] int lw;
    for(i in 1:N_year){
      if(age_shift[i]>=0){
        lw[i] = 0;
        while (age_shift[i] >= lw[i] + 1) {
          lw[i] = lw[i] + 1;
        }
      }else{
        lw[i] = -1;
        while (age_shift[i] <= lw[i] -1) {
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
      if(age_id[i]<=1 + age_shift[i]) logit_birth_prob_shift[i] = logit_birth_prob_int[1];
      else if(age_id[i]>=N_age + age_shift[i]) logit_birth_prob_shift[i] = logit_birth_prob_int[N_age];
      else logit_birth_prob_shift[i] =  logit_birth_prob_int[age_id[i] - lw[i]-1] * frac[i] + logit_birth_prob_int[age_id[i] - lw[i]] * (1.0 - frac[i]);
    }
    return(logit_birth_prob_shift);
  }
  
  
  real logit_birth_prob2(int N, int N_age,  int age_id, int lw, real age_shift, vector logit_birth_prob_int) {
    real frac = age_shift-lw;
    real logit_birth_prob_shift;
    if(age_id<=1 + age_shift) logit_birth_prob_shift = logit_birth_prob_int[1];
    else if(age_id>=N_age + age_shift) logit_birth_prob_shift = logit_birth_prob_int[N_age];
    else logit_birth_prob_shift =  logit_birth_prob_int[age_id - lw-1] *  frac + logit_birth_prob_int[age_id - lw] * (1.0 - frac);
  
    return(logit_birth_prob_shift);
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
  
  real p_inv_sigma;
  
  array[2] real p_delta0;
  array[2] real p_sigma_rw;
  
  int inference;
}

transformed data {
   // vector[N_year] f_year;
   // for(i in 1:N_year){
   //   f_year[i] = 0;
   // }
  //real beta=0;
}

parameters {
  real beta;
  real <lower=0> inv_sigma;
  
  real delta0;
  vector[N_age-1] delta;       // deviations from the mean
  real<lower=0> sigma_rw;       // smoothness of the random walk
}
transformed parameters {
  
   vector[N_year] f_year;
   for(i in 1:N_year){
     f_year[i] = beta * (i-1.0)/(N_year-1.0);
   }
 
  
  real sigma = inv(inv_sigma);
  
  vector[N_age] logit_birth_prob_int;
  logit_birth_prob_int[1] = delta0;
  for(i in 2:N_age){
    logit_birth_prob_int[i] = logit_birth_prob_int[i-1] + delta[i-1] * sigma_rw;
  }
}
model {
  inv_sigma ~ exponential(p_inv_sigma);
  
  beta ~ normal(0,1);
  
  //priors
  delta0 ~normal(p_delta0[1], p_delta0[2]);
  sigma_rw ~ normal(p_sigma_rw[1],p_sigma_rw[2]);
  //random effect
  delta ~ normal(0, 1);
  
  if(inference==1){
    array[N] int lw =  (lower_lim(f_year, N_year))[year_id];
    target += neg_binomial_2_log_lpmf(n_birth | log(inv_logit(logit_birth_prob(N, N_age, age_id, lw, f_year[year_id], logit_birth_prob_int))) + log(n_pop), sigma);
    
  }
}

generated quantities{
  array[N_year, N_age] real birth_prob;

  array[N_year] int lw2 = (lower_lim(f_year, N_year));
  
  for(i in 1:N_year){
    for(j in 1:N_age){
      birth_prob[i,j] =  inv_logit(logit_birth_prob2(N, N_age, j, lw2[i], f_year[i], logit_birth_prob_int));
    }
  }
  
  vector[N] log_mu = log(inv_logit(logit_birth_prob(N, N_age, age_id, lw2[year_id], f_year[year_id], logit_birth_prob_int))) + log(n_pop);
  array[N] int n_birth_pred = neg_binomial_2_log_rng(log_mu, sigma);
  array[N] int n_birth_pois_pred = poisson_log_rng(log_mu);

  vector[N_age] age_bias;
  for(i in 1:N_age){
    age_bias[i] = 0.0;
  }
  for(i in 1:N){
    age_bias[age_id[i]] = age_bias[age_id[i]] + (n_birth_pred[i]-n_birth[i]);
  }
}
