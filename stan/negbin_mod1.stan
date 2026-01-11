data {
  int<lower=1> N;
  array[N] int Y;
  array[N] int n_pop;
  
  int<lower=1> n_year;
  int<lower=1> n_month;
  int<lower=1> n_region;
  int<lower=1> n_age;
  
  array[N] int year_id;
  array[N] int month_id;
  array[N] int region_id;
  array[N] int age_id;
}

parameters {
  vector[n_year-1]  alpha_year0;
  vector[n_month-1] alpha_month0;
  vector[n_region-1] alpha_region0;
  vector[n_age-1] alpha_age0;
  
  real intercept;
  
  real<lower = 0> inv_sigma;
}
transformed parameters {
   vector[n_year] alpha_year = append_row(0.0, alpha_year0);
   vector[n_month] alpha_month = append_row(0.0, alpha_month0);
   vector[n_region] alpha_region = append_row(0.0, alpha_region0);
   vector[n_age] alpha_age = append_row(0.0, alpha_age0);
   
   real sigma = inv(inv_sigma);
   
}
model {
  // weak priors
  alpha_year0 ~ normal(0, 0.2);
  alpha_month0 ~ normal(0, 0.2);
  alpha_region0 ~ normal(0, 0.1);
  alpha_age0 ~ normal(0, 0.1);
  intercept ~ normal(0, 1);
  inv_sigma ~ normal(0,1);
  
  {
    vector[N] eta;
    for (i in 1:N) {
      eta[i] =
        intercept +
        alpha_year[year_id[i]] +
        alpha_month[month_id[i]] +
        alpha_region[region_id[i]] +
        alpha_age[age_id[i]] +
        log(n_pop[i]);
    }
    Y ~ neg_binomial_2_log(eta,sigma);
  }
}
