functions {
  real partial_poisson_lpmf(array[] int slice_Y,
                            int start,
                            int end,
                            real intercept,
                            vector alpha_year,
                            vector alpha_month,
                            vector alpha_region,
                            vector alpha_age,
                            array[] int year_id, 
                            array[] int month_id,
                            array[] int region_id,
                            array[] int age_id,
                            vector n_pop) {

    // slice predictor
    vector[end - start + 1] eta = intercept +
                                  alpha_year[year_id[start:end]] +
                                  alpha_month[month_id[start:end]] +
                                  alpha_region[region_id[start:end]] +
                                  alpha_age[age_id[start:end]] +
                                  log(n_pop[start:end]);

    return poisson_log_lpmf(slice_Y | eta);
  }
}

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
  vector[n_year]   alpha_year   = append_row(0, alpha_year0);
  vector[n_month]  alpha_month  = append_row(0, alpha_month0);
  vector[n_region] alpha_region = append_row(0, alpha_region0);
  vector[n_age]    alpha_age    = append_row(0, alpha_age0);
}
model {
   // priors
  alpha_year0   ~ normal(0, 0.2);
  alpha_month0  ~ normal(0, 0.2);
  alpha_region0 ~ normal(0, 0.1);
  alpha_age0    ~ normal(0, 0.1);
  intercept     ~ normal(0, 1);

  // linear predictor (size N)
  vector[N] lp_pred = intercept +
                      alpha_year[year_id] +
                      alpha_month[month_id] +
                      alpha_region[region_id] +
                      alpha_age[age_id] +
                      log(n_pop);

  // likelihood via reduce_sum
  target += reduce_sum(
              partial_poisson_lpmf,
              Y,
              9000,              // grain_size, adjust later
              intercept, alpha_year, alpha_month, alpha_region, alpha_age,
              year_id, month_id, region_id, age_id,
              n_pop
           );
}
