data {
  int<lower=1> N;
  array[N] int Y;
  vector[N] n_pop;
  
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
}
transformed parameters {

}
model {
  // weak priors
  alpha_year0 ~ normal(0, 0.2);
  alpha_month0 ~ normal(0, 0.2);
  alpha_region0 ~ normal(0, 0.1);
  alpha_age0 ~ normal(0, 0.1);
  intercept ~ normal(0, 1);
  
  {
   vector[n_year] alpha_year = append_row(0.0, alpha_year0);
   vector[n_month] alpha_month = append_row(0.0, alpha_month0);
   vector[n_region] alpha_region = append_row(0.0, alpha_region0);
   vector[n_age] alpha_age = append_row(0.0, alpha_age0);
   
    Y ~ poisson_log(intercept +
        alpha_year[year_id] +
        alpha_month[month_id] +
        alpha_region[region_id] +
        alpha_age[age_id] +
        log(n_pop));
  }
}
