functions {
  vector log_birth_prob(vector age_id, real a_peak,real log_h_peak, real sigma) {
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
  
  // Hyperprior parameters
  array[2] real p_age_peak;    // Prior for yearly GP lengthscale
  array[2] real p_log_h_peak;     // Prior for yearly GP scale
  array[2] real p_birth_prob_sigma;    // Prior for yearly GP lengthscale
  
  int inference;
}

parameters {
  real <lower=0> inv_sigma;
  
  real <lower=0> age_peak;
  real log_h_peak;
  real <lower=0> birth_prob_sigma;
}
transformed parameters {
  real sigma = inv(inv_sigma);
}
model {
  inv_sigma ~ normal(0,1);
  // weak priors
  age_peak ~ normal(p_age_peak[1],p_age_peak[2]);
  log_h_peak ~ normal(p_log_h_peak[1],p_log_h_peak[2]);
  birth_prob_sigma ~ normal(p_birth_prob_sigma[1],p_birth_prob_sigma[2]);

  if(inference==1){
    target += neg_binomial_2_log_lpmf(n_birth |log_birth_prob(to_vector(age_id), age_peak, log_h_peak, birth_prob_sigma) + log(n_pop), sigma);
  }
}

generated quantities{
  array[N_year, N_age] real birth_prob;

  for(i in 1:N_year){
    for(j in 1:N_age){
      birth_prob[i,j] = exp(log_birth_prob2(j, age_peak, log_h_peak, birth_prob_sigma));
    }
  }
}
