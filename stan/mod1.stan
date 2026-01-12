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
}

data {
  int<lower=1> N;
  int<lower=1> N_year;
  int<lower=1> N_age;
  
  array[N] int n_birth;
  vector[N] n_pop;
  
  array[N] int year_id;
  array[N] int age_id;
  
  // Time points and corresponding locations
  vector[N_year] x;
  vector[N_age] y;
  
  // GP basis function settings
  real<lower=0> c_year;           // Boundary scaling factor
  int M_year;                     // Number of EQ basis functions
  real<lower=0> c_age;           // Boundary scaling factor
  int M_age;                     // Number of EQ basis functions
  
  // Hyperprior parameters
  array[2] real p_intercept;      // Prior for intercept
  array[2] real p_alpha_year;     // Prior for yearly GP scale
  array[2] real p_lambda_year;    // Prior for yearly GP lengthscale
  array[2] real p_alpha_age;     // Prior for yearly GP scale
  array[2] real p_lambda_age;    // Prior for yearly GP lengthscale
}

transformed data {
  // normalize data
  real x_mean = mean(x);
  real x_sd = sd(x);
  vector[N_year] xn = (x - x_mean)/x_sd;
  
  real y_mean = mean(y);
  real y_sd = sd(y);
  vector[N_age] yn = (y - y_mean)/y_sd;
  
  // compute boundary value
  real L_year = c_year*max(xn);
  real L_age = c_year*max(yn);
  
  // compute basis functions for f
  matrix[N_year,M_year] PHI_year = PHI_EQ(N_year, M_year, L_year, xn);
  matrix[N_age,M_age] PHI_age = PHI_EQ(N_age, M_age, L_age, yn);
  
}

parameters {
  real intercept;
  
   // GPs by age group

  real <lower=0>  alpha_year;       // Yearly GP scale by age
  real <lower=0>  lambda_year;      // Yearly GP lengthscale by age
  vector[M_year] beta_year; // Basis coefficients for yearly GP
  
  real <lower=0>  alpha_age;       // Yearly GP scale by age
  real <lower=0>  lambda_age;      // Yearly GP lengthscale by age
  vector[M_age] beta_age; // Basis coefficients for yearly GP
}
transformed parameters {
  vector[M_year] diagSPD_year;
  vector[M_age] diagSPD_age;

  vector[N_year] f_year;
  vector[N_age] f_age;
  // compute spectral densities for f
  diagSPD_year = diagSPD_EQ(alpha_year, lambda_year, L_year, M_year);
  diagSPD_age = diagSPD_EQ(alpha_age, lambda_age, L_age, M_age);
  // compute f
  f_year = PHI_year * (diagSPD_year .* beta_year);
  f_age = PHI_age * (diagSPD_age .* beta_age);
}
model {
  // weak priors
  intercept ~ normal(p_intercept[1], p_intercept[2]);
    //GP: variance and lengthscale
  lambda_year ~ lognormal(p_lambda_year[1], p_lambda_year[2]);
  alpha_year ~ normal(p_alpha_year[1], p_alpha_year[2]);
  lambda_age ~ lognormal(p_lambda_age[1], p_lambda_age[2]);
  alpha_age ~ normal(p_alpha_age[1], p_alpha_age[2]);
  
  beta_year ~ normal(0, 1);
  beta_age ~ normal(0, 1);
  
  
  target += poisson_log_lpmf(n_birth |intercept + f_year[year_id] + f_age[age_id] + log(n_pop));
  
}
