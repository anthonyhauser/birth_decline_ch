functions {
  //see https://github.com/avehtari/casestudies/blob/master/Birthdays
  // basis function (exponentiated quadratic kernel)
    matrix PHI_EQ_sin(int N, int M, real L, vector x) {
      matrix[N,M] A = rep_matrix(pi()/(2*L) * (x+L), M);
      vector[M] B = linspaced_vector(M, 1, M);
      matrix[N,M] PHI = sin(diag_post_multiply(A, B))/sqrt(L);
      //for (m in 1:M) PHI[,m] = PHI[,m] - mean(PHI[,m]); // scale to have mean 0
      return PHI;
    }
    matrix PHI_EQ_sin2(int N, int M, real L, vector x) {
      matrix[N,M] A = rep_matrix(pi()/(2*L) * (x), M);
      vector[M] B = linspaced_vector(M, 1, M);
      matrix[N,M] PHI = sin(diag_post_multiply(A, B));
      //for (m in 1:M) PHI[,m] = PHI[,m] - mean(PHI[,m]); // scale to have mean 0
      return PHI;
    }
    matrix PHI_EQ_cos(int N, int M, real L, vector x) {
      matrix[N,M] A = rep_matrix(pi()/(2*L) * (x+L), M);
      vector[M] B = linspaced_vector(M, 1, M);
      matrix[N,M] PHI = cos(diag_post_multiply(A, B))/sqrt(L); // we remove /sqrt(L) because already in sin and to hold the identity sin = sin * cos + cos * sin, we need to divide by sqrt(L) either in sin or cos but not twice
      //for (m in 1:M) PHI[,m] = PHI[,m] - mean(PHI[,m]); // scale to have mean 0
      return PHI;
    }
     matrix PHI_EQ_cos2(int N, int M, real L, vector x) {
      matrix[N,M] A = rep_matrix(pi()/(2*L) * (x), M);
      vector[M] B = linspaced_vector(M, 1, M);
      matrix[N,M] PHI = cos(diag_post_multiply(A, B)); // we remove /sqrt(L) because already in sin and to hold the identity sin = sin * cos + cos * sin, we need to divide by sqrt(L) either in sin or cos but not twice
      //for (m in 1:M) PHI[,m] = PHI[,m] - mean(PHI[,m]); // scale to have mean 0
      return PHI;
    }
    // vector PHI_EQ_sin_s(int N, int M, real L, real s) {
    //   real A = pi()/(2*L) * (s+L);
    //   vector[M] B = linspaced_vector(M, 1, M);
    //   matrix[N,M] PHI = sin(A*B)/sqrt(L);
    //   return PHI;
    // }
    //  vector PHI_EQ_cos_s(int N, int M, real L, real s) {
    //   real A = pi()/(2*L) * (s+L);
    //   vector[M] B = linspaced_vector(M, 1, M);
    //   matrix[N,M] PHI = cos(A*B)/sqrt(L);
    //   return PHI;
    // }
    
    // basis function (exponentiated quadratic kernel)
    matrix PHI_EQ(int N, int M, real L, vector x) {
      matrix[N,M] A = rep_matrix(pi()/(2*L) * (x+L), M);
      vector[M] B = linspaced_vector(M, 1, M);
      matrix[N,M] PHI = sin(diag_post_multiply(A, B))/sqrt(L);
      //for (m in 1:M) PHI[,m] = PHI[,m] - mean(PHI[,m]); // scale to have mean 0
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
  vector[N_year1] x2;
  vector[N_age] x1;
  
  // GP basis function settings
  real<lower=0> c_year;           // Boundary scaling factor
  int M_year;   
  real<lower=0> c_age;           // Boundary scaling factor
  int M_age;   
 
  //prior 
  real p_inv_sigma;
  array[2] real p_delta0;
  array[2] real p_alpha;
  array[2] real p_alpha_year;
   array[2] real  p_rho;
  array[2] real  p_lambda_year;   

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
  
  // compute basis functions for f_year
  matrix[N_year1,M_year] PHI_year = PHI_EQ(N_year1, M_year, L_year, xn);
  //f_age
  matrix[N_age,M_age] PHI_age_sin_x = PHI_EQ_sin(N_age, M_age, L_age, xn1);
  matrix[N_age,M_age] PHI_age_cos_x = PHI_EQ_cos(N_age, M_age, L_age, xn1);
  
  
  row_vector[M_age] mean_PHI_age;
  row_vector[M_year] mean_PHI_year;
  for (m in 1:M_year){
    mean_PHI_year[m] = mean(PHI_year[,m]);// scale to have mean 0
    PHI_year[,m] =  PHI_year[,m] - mean_PHI_year[m];
  }
  for (m in 1:M_age){
    mean_PHI_age[m] = mean(PHI_age_sin_x[,m]);// scale to have mean 0
  }
}

parameters {
  vector <lower=0>[N_sigma] inv_sigma;
  real delta0;
  
  // GPs
  real<lower=0> alpha;
  vector[M_age] beta_age;
  real <lower=0> alpha_year; // Yearly GP scale by age
  vector[M_year] beta_year; // Basis coefficients for yearly GP
  
  real <lower=0> rho; // Yearly GP scale by age
  real <lower=0> lambda_year; // Yearly GP scale by age
}
transformed parameters {
  vector[M_year] diagSPD_year;
  vector[M_age] diagSPD_age;
  vector[N_year1] f_year;
  matrix[N_age,N_year1] f_age;
  
  // compute spectral densities for f_year
  diagSPD_year = diagSPD_EQ(alpha_year, lambda_year, L_year, M_year);
  f_year = PHI_year * (diagSPD_year .* beta_year);
  //f_age
  diagSPD_age = diagSPD_EQ(alpha, rho, L_age, M_age);
  {
    matrix[N_year1,M_age] PHI_age_sin_s = PHI_EQ_sin2(N_year1, M_age, L_age, f_year/x1_sd);
    matrix[N_year1,M_age] PHI_age_cos_s = PHI_EQ_cos2(N_year1, M_age, L_age, f_year/x1_sd);
    
    vector[M_age] vec = diagSPD_age .* beta_age;
    for(i in 1:N_year1){
      f_age[,i] = PHI_age_sin_x * (vec .* to_vector(PHI_age_cos_s[i,])) +
                  PHI_age_cos_x * (vec .* to_vector(PHI_age_sin_s[i,])) - (mean_PHI_age * vec); // scale to have mean 0

    }
  }
    // for(i in 1:N_year1){
    //   PHI_age[i] = PHI_age_sin_x[i] .* rep_matrix(to_row_vector(PHI_age_cos_s[i]), M_age) +
    //                PHI_age_cos_x[i] .* rep_matrix(to_row_vector(PHI_age_sin_s[i]), M_age);
    //   f_age[,i] = PHI_age[i] * vec;
    // }
    
  
    //  for(i in 1:N_year1){
    //    matrix[N_age,M_age] m;
    //    matrix[N_age,M_age] m2;
    //    matrix[N_age,M_age] m3;
    //    matrix[N_age,M_age] m4;
    //    matrix[N_age,M_age] m5;
    //    for(j in 1:N_age){
    //      for(k in 1:M_age){
    //        m[j,k] = PHI_age_sin_x[j,k] * PHI_age_cos_s[i,k] +
    //                 PHI_age_cos_x[j,k] * PHI_age_sin_s[i,k];
    //         m2[j,k] = PHI_age_sin_x[j,k];
    //         m3[j,k] = PHI_age_cos_s[i,k];
    //         m4[j,k] = PHI_age_cos_x[j,k];
    //         m5[j,k] = PHI_age_sin_s[i,k];
    //      }
    //    }
    //    PHI_age[i] = m; 
    //    PHI_age_base[i] = m2; 
    //    PHI_age_base2[i] = m3; 
    //    PHI_age_base3[i] = m4; 
    //    PHI_age_base4[i] = m5; 
    //   f_age[,i] = PHI_age[i] * vec;
    // }
  //}
  //sigma
  vector[N_sigma] sigma = inv(inv_sigma);
}
model {
  //priors
  inv_sigma ~ exponential(p_inv_sigma);
  delta0 ~normal(p_delta0[1], p_delta0[2]);
  
 //GP: variance and lengthscale
  alpha ~ normal(p_alpha[1], p_alpha[2]);
  beta_age ~ std_normal();
  
  alpha_year ~ normal(p_alpha_year[1], p_alpha_year[2]);
  beta_year ~ normal(0, 1);
  
  rho ~ normal(p_rho[1], p_rho[2]);
  lambda_year ~ normal(p_lambda_year[1],p_lambda_year[2]);

  
  if(inference==1){
    vector[N] v; 
    for(i in 1:N){
     v[i] = f_age[age_id[i],year_id1[i]];
    }
    target += neg_binomial_2_log_lpmf(n_birth | log(inv_logit(v + delta0)) + log(n_pop), sigma[sigma_id]);
    
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
         logit_birth_prob[i,j] =  f_age[j,i] + delta0;
         birth_prob[i,j] =  inv_logit(f_age[j,i] + delta0);
      }
    }
    vector[N] log_mu;
    for(i in 1:N){
     log_mu[i] = log(inv_logit(f_age[age_id[i],year_id1[i]] + delta0)) + log(n_pop[i]);
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
