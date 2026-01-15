data {
  int N;
  array[N] int y;
}

parameters {
  real<lower=0> lambda;
}

model {
  lambda ~ normal(0, 10);   // weak prior
  y ~ poisson(lambda);
}
