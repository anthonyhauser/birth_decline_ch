source("R/000_setup.R")

N <- 3e6
y <- rpois(N, lambda = 10)

stan_data <- list(N = N,
                  y = y)

mod <- cmdstan_model("stan/poisson_model.stan")

fit <- mod$sample(
  data = stan_data,
  chains = 2,
  parallel_chains = 2,
  iter_sampling = 300,
  iter_warmup = 200
)

fit$summary()

################################################################################

# dimensions
n_year  <- 30
n_month <- 12
n_time  <- n_year * n_month
n_region <- 200
n_age <- 5

# unique IDs
year_tbl   <- tibble(year_id = 1:n_year)
month_tbl  <- tibble(month_id = 1:n_month)
region_tbl <- tibble(region_id = 1:n_region)
age_tbl    <- tibble(age_id = 1:n_age)
intercept = log(2/35)

# true effects
year_effect_true   <- c(0,rnorm(n_year-1,   0, 0.1))
month_effect_true  <- c(0,rnorm(n_month-1,   0, 0.1))
region_effect_true <- c(0,rnorm(n_region-1,   0, 0.1))
age_effect_true    <- c(0,rnorm(n_age-1,   0, 0.1))

# full panel: 30 * 12 * 2000 * 5 = 3,600,000 rows
panel <- expand_grid(year_tbl,
                    month_tbl,
                    region_tbl,
                    age_tbl) %>% 
  dplyr::mutate(n_pop = runif(n(), min = 1, max = 200),
                residuals = rnorm(n(),0,0.2),
                eta = intercept + residuals +
                      year_effect_true[year_id] +
                      month_effect_true[month_id] +
                      region_effect_true[region_id] +
                      age_effect_true[age_id] +
                      log(n_pop),
                lambda = exp(eta),
                p = lambda/n_pop,
                Y = rpois(n(), lambda))

panel %>% 
  ggplot(aes(x=p))+geom_histogram()+geom_vline(xintercept = exp(intercept),color="red")

#stan data
stan_data <- list(
  N = nrow(panel),
  Y = panel$Y,
  n_pop = panel$n_pop,
  year_id = panel$year_id,
  month_id = panel$month_id,
  region_id = panel$region_id,
  age_id = panel$age_id,
  n_year = n_year,
  n_month = n_month,
  n_region = n_region,
  n_age = n_age
)

# compile & run
mod <- cmdstan_model("stan/poisson_model2.stan")
fit_pois <- mod$sample(data = stan_data,
                  chains = 4,
                  parallel_chains = 4,
                  iter_sampling = 200,
                  iter_warmup = 200,
                  refresh = 10)

mod <- cmdstan_model("stan/negbin_mod1.stan")
fit_negbin <- mod$sample(data = stan_data,
                  chains = 4,
                  parallel_chains = 4,
                  iter_sampling = 200,
                  iter_warmup = 200,
                  refresh = 10)


fit_pois$summary() %>% arrange(-rhat)
fit_negbin$summary() %>% arrange(-rhat)


fit_pois$summary(variables = c("intercept"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_negbin$summary(variables = c("intercept","sigma","inv_sigma"), "mean",~quantile(.x, probs = c(0.025, 0.975))) 


chains = fit$time()$chains %>% filter(!is.na(total)) %>% pull(chain_id) %>% .[1:2]
stan_diag =  data.frame(time = fit$time()$chains %>% filter(!is.na(total)) %>% pull(total) %>% max(),
                        num_successful_chains =  fit$diagnostic_summary()$num_divergent %>% length(),
                        num_divergent = fit$diagnostic_summary()$num_divergent %>% sum(),#fit$sampler_diagnostics()
                        num_max_treedepth = fit$diagnostic_summary()$num_max_treedepth %>% sum(),
                        ebfmi = fit$diagnostic_summary()$ebfmi %>% min(),
                        rhat = fit$summary() %>% filter(!is.na(rhat)) %>% pull(rhat) %>% max()) %>% 
  dplyr::mutate(is.stan.ok = num_successful_chains>=4 & num_divergent==0 & ebfmi>=0.3 & rhat<1.1)
print(stan_diag)















mod <- cmdstan_model("stan/poisson_model2_v2.stan", cpp_options = list(stan_threads = TRUE))

mod <- cmdstan_model("stan/poisson_model2_v2.stan",
                      cpp_options = list(stan_threads = TRUE),
                      force_recompile = TRUE)

fit <- mod$sample(
  data = stan_data,
  iter_sampling = 200,
  iter_warmup = 200,
  refresh = 10,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 6   # or 6–8 on your CPU
)
