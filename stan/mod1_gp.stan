functions {
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
  
  //vector[N] n_birth;
  array[N] int n_birth;
  vector[N] n_pop;
  
  array[N] int year_id1;
  array[N] int age_id;
  
  array[N_age] real x1;
  
  real p_inv_sigma;
  
  array[2] real p_delta0;
  
  
  array[2] real p_alpha;
  array[2] real p_rho;
  
  int inference;
}

transformed data {
  real gp_delta = 1e-9;
   

  real rho=5;
}

parameters {
  //real<lower=0> rho;
  real<lower=0> alpha;
  vector[N_age] eta;
  
  real beta;
 
  real <lower=0> inv_sigma;
  
  real delta0;
}
transformed parameters {
  

  
  vector[N_year1] f_year;
   for(i in 1:N_year1){
     f_year[i] = beta * (i-1.0)/(N_year1-1.0);
   }
  
  real sigma = inv(inv_sigma);
  
}
model {
  vector[N_age] logit_birth_prob_int;
  {
    matrix[N_age, N_age] L_K;
    matrix[N_age,N_age] K = gp_exp_quad_cov(x1, alpha, rho);
  
    // diagonal elements
    for (n in 1:N_age) {
      K[n, n] = K[n, n] + gp_delta;
    }
  
    L_K = cholesky_decompose(K);
    logit_birth_prob_int = L_K * eta + delta0;
  }
  
  inv_sigma ~ exponential(p_inv_sigma);
  
  beta ~ normal(0,1);
  
  //rho ~ inv_gamma(p_rho[1], p_rho[2]);
  alpha ~ normal(p_alpha[1], p_alpha[2]);
  eta ~ std_normal();
  
  //priors
  delta0 ~normal(p_delta0[1], p_delta0[2]);

  
  if(inference==1){
    array[N] int lw =  (lower_lim(f_year, N_year1))[year_id1];
    target += neg_binomial_2_log_lpmf(n_birth | log(inv_logit(logit_birth_prob(N, N_age, age_id, lw, f_year[year_id1], logit_birth_prob_int))) + log(n_pop), sigma);
    
  }
}

generated quantities{
    vector[N_age] logit_birth_prob_int;
  {
    matrix[N_age, N_age] L_K;
    matrix[N_age,N_age] K = gp_exp_quad_cov(x1, alpha, rho);
  
    // diagonal elements
    for (n in 1:N_age) {
      K[n, n] = K[n, n] + gp_delta;
    }
  
    L_K = cholesky_decompose(K);
    logit_birth_prob_int = L_K * eta + delta0;
  }
  
  array[N_year1, N_age] real birth_prob;

  array[N_year1] int lw2 = (lower_lim(f_year, N_year1));

  for(i in 1:N_year1){
    for(j in 1:N_age){
      birth_prob[i,j] =  inv_logit(logit_birth_prob2(N, N_age, j, lw2[i], f_year[i], logit_birth_prob_int));
    }
  }

  vector[N] log_mu = log(inv_logit(logit_birth_prob(N, N_age, age_id, lw2[year_id1], f_year[year_id1], logit_birth_prob_int))) + log(n_pop);
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
