library(cmdstanr)
library(dplyr)
library(ggplot2)
library(bayesplot)
set_cmdstan_path("C:/TEMP/.cmdstan/cmdstan-2.36.0")
cmdstan_path()

#load data
#load the csv file from the link: "https://drive.google.com/file/d/1f7svqwIgCzqYGZPAhCWvolWw4WCYBUnu/view?usp=sharing"
stan_df = readRDS("data/simulated_stan_df_link.RDS")
stan_df = readRDS("data/stan_df.RDS")

#choose the stratification level:
#by_year: one observation by year, Model M0, working
#!by_year: 12 observations by year (monthly), Model M1, Rhat>1.1 and some bias (see below)
by_year=FALSE

get_stan_data = function(stan_df,by_year=TRUE){
  if(by_year){
    stan_df = stan_df %>% 
      group_by(age_id,year_id1) %>% 
      dplyr::summarise(n_birth = sum(n_birth),
                       n_pop = n_pop[1],.groups="drop")
  }
  stan_df
  
  #Stan list--------------------------------------------------------------------
  stan_data = list(N = dim(stan_df)[1],
                   N_year1 = length(unique(stan_df$year_id1)),
                   N_age = length(unique(stan_df$age_id)),
                   N_sigma =1,
                   
                   year_id1 = stan_df$year_id1,
                   age_id = stan_df$age_id,
                   sigma_id = rep(1,length(stan_df$age_id)),
                   
                   n_pop = stan_df$n_pop,
                   n_birth = stan_df$n_birth,
                   
                   x1 = 1:max(stan_df$age_id),
                   x2 = stan_df$year_id1 %>% unique() %>% sort(),
                   
                   M_age = 25, 
                   c_age = 5,
                   
                   M_year = 10, 
                   c_year = 5,
                   
                   p_rho = c(2,5),
                   p_alpha = c(3,1),
                   p_lambda_year = c(2, 2),
                   p_alpha_year = c(2,2),
                   p_inv_sigma = 100,
                   p_delta0 = c(-6,2),
                   
                   inference = 1)
  return(stan_data)
}


stan_year_data = get_stan_data(stan_df,by_year=TRUE)
stan_month_data = get_stan_data(stan_df,by_year=FALSE)
stan_year_data$N
stan_month_data$N
  
#old model-------------------------------------------------------------------
mod1 <- cmdstan_model("stan/mod_stan_discourse1.stan")
fit1_year <- mod1$sample(data = stan_year_data,
                  init=0, #this needs to be relaxed later on
                  chains = 4,
                  parallel_chains = 4,
                  iter_sampling = 200,
                  iter_warmup = 200, #tried 500 but didn't improve much
                  adapt_delta = 0.8,
                  refresh = 10,
                  seed = 1)
fit1 = fit1_year 
fit1$diagnostic_summary()

# extract only needed parameters as a data.frame
var = c("delta0", "alpha","rho", "beta_age[1]", "beta_age[2]")
df <- fit1$draws(var,
                format = "df")
df$log_alpha <- log(df$alpha)
df$log_rho <- log(df$rho)

# pairs plot
var = c("delta0", "log_alpha","log_rho", "beta_age[1]", "beta_age[2]")
mcmc_pairs(df[, var], diag_fun = "hist")

#new model but still with delta0-------------------------------------------------------------------
mod1_v2 <- cmdstan_model("stan/mod_stan_discourse1_v2.stan")
stan_year_data$c_age=1.5
fit1_v2_year <- mod1_v2$sample(data = stan_year_data,
                    init=0, #this needs to be relaxed later on
                    metric = "dense_e",
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 200,
                    iter_warmup = 400, #tried 500 but didn't improve much
                    adapt_delta = 0.8,
                    refresh = 10,
                    seed = 2)
saveRDS(fit1_v2_year, file = "results/fit1_v2_year.rds")

stan_month_data$c_age=1.5
fit1_v2_month <- mod1_v2$sample(data = stan_month_data,
                               init=0, #this needs to be relaxed later on
                               metric = "dense_e",
                               chains = 4,
                               parallel_chains = 4,
                               iter_sampling = 200,
                               iter_warmup = 400, #tried 500 but didn't improve much
                               adapt_delta = 0.8,
                               refresh = 10,
                               seed = 2)
saveRDS(fit1_v2_month, file = "results/fit1_v2_month.rds")

fit1_v2 = fit1_v2_year
fit1_v2 = fit1_v2_month 
fit1_v2$diagnostic_summary()


# extract only needed parameters as a data.frame
var = c("delta0", "alpha","rho", "beta_age[1]", "beta_age[2]")
fit1_v2$summary(var)
df <- fit1_v2$draws(var,
                format = "df")
df$log_alpha <- log(df$alpha)
df$log_rho <- log(df$rho)

# pairs plot
var = c("delta0", "log_alpha","log_rho", "beta_age[1]", "beta_age[2]")
mcmc_pairs(df[, var], diag_fun = "hist")


#Stan model-------------------------------------------------------------------
stan_year_data$c_age=1.5
stan_year_data$log_mean_n_birth_n_pop = log(mean(stan_year_data$n_birth)/mean(stan_year_data$n_pop))

mod2 <- cmdstan_model("stan/mod_stan_discourse2.stan")
fit2_year <- mod2$sample(data = stan_year_data,
                  init=0, #this needs to be relaxed later on
                  metric = "dense_e",
                  chains = 4,
                  parallel_chains = 4,
                  iter_sampling = 200,
                  iter_warmup = 400, #tried 500 but didn't improve much
                  adapt_delta = 0.8,
                  refresh = 10,
                  seed = 2)
saveRDS(fit2_year, file = "results/fit2_year.rds")

stan_month_data$c_age=1.5
stan_month_data$log_mean_n_birth_n_pop = log(mean(stan_month_data$n_birth)/mean(stan_month_data$n_pop))
fit2_month <- mod2$sample(data = stan_month_data,
                    init=0, #this needs to be relaxed later on
                    metric = "dense_e",
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 200,
                    iter_warmup = 400, #tried 500 but didn't improve much
                    adapt_delta = 0.8,
                    refresh = 10,
                    seed = 2)
saveRDS(fit2_month, file = "results/fit2_month.rds")

fit2 = fit2_year
fit2 = fit2_month
fit2$diagnostic_summary()

# extract only needed parameters as a data.frame
var = c("alpha","rho", "beta_age[1]", "beta_age[2]")
df <- fit2_month$draws(var,
                    format = "df")
diag_df <- fit2_month$sampler_diagnostics(format = "df")
df <- df %>%
  left_join(diag_df, by = c(".chain", ".iteration", ".draw"))
df$log_alpha <- log(df$alpha)
df$log_rho <- log(df$rho)

# pairs plot
var = c("log_alpha","log_rho", "beta_age[1]", "beta_age[2]")
mcmc_pairs(df[, var], diag_fun = "hist")


plot_df <- as.data.frame(df[, var])
plot_df$divergent <- df$divergent__ == 1  # TRUE/FALSE for divergent draws
plot_df = plot_df %>% arrange(divergent)
cols <- ifelse(plot_df$divergent,
               adjustcolor("red", alpha.f = 1),
               adjustcolor("black", alpha.f = 0.2))
pairs(plot_df[var],
  col =cols,
  pch = 16,
  main = "Pairwise scatterplots (red = divergent draws)")

#Stan model-------------------------------------------------------------------
stan_month_data$c_age=1.5
stan_month_data$c_year=1.5
stan_month_data$log_mean_n_birth_n_pop = log(mean(stan_month_data$n_birth)/mean(stan_month_data$n_pop))
mod3 <- cmdstan_model("stan/mod_stan_discourse3.stan")
fit3 <- mod3$sample(data = stan_month_data,
                    init=0, #this needs to be relaxed later on
                    metric = "dense_e",
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 200,
                    iter_warmup = 400, #tried 500 but didn't improve much
                    adapt_delta = 0.99,
                    refresh = 10,
                    seed = 1)
fit3$diagnostic_summary()

# extract only needed parameters as a data.frame
var = c( "alpha","rho","alpha_year","lambda_year", "beta_age[1]", "beta_age[2]","beta_year[1]", "beta_year[2]","inv_sigma[1]")
df <- fit$draws(var,
                format = "df")
df$log_alpha <- log(df$alpha)
df$log_rho <- log(df$rho)
df$log_alpha_year <- log(df$alpha_year)

# pairs plot
var = c( "log_alpha","rho","log_alpha_year","lambda_year", "beta_age[1]", "beta_age[2]","beta_year[1]", "beta_year[2]","inv_sigma[1]")
mcmc_pairs(df[, var], diag_fun = "hist")
           
#high rhat for model M1 (up to 1.18)
fit$summary() %>% arrange(-rhat) %>% .[,1:10]

#Stan model-------------------------------------------------------------------
stan_month_data$c_age=1.5
stan_month_data$c_year=1.5
stan_month_data$log_mean_n_birth_n_pop = log(mean(stan_month_data$n_birth)/mean(stan_month_data$n_pop))
stan_month_data$rho = 0.6
stan_month_data$lambda_year = 1
mod4 <- cmdstan_model("stan/mod_stan_discourse4.stan")
fit4_month <- mod4$sample(data = stan_month_data,
                    init=0, #this needs to be relaxed later on
                    metric = "dense_e",
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 500,
                    iter_warmup = 1000, #tried 500 but didn't improve much
                    adapt_delta = 0.99,
                    refresh = 10,
                    seed = 1)
saveRDS(fit4_month, file = "results/fit4_month.rds")
fit4_month$diagnostic_summary()
fit4_month$summary() %>% arrange(-rhat) %>% .[,1:10]

#Stan model-------------------------------------------------------------------
stan_month_data$c_age=1.5
stan_month_data$c_year=1.5
stan_month_data$log_mean_n_birth_n_pop = log(mean(stan_month_data$n_birth)/mean(stan_month_data$n_pop))
stan_month_data$rho = 0.6
stan_month_data$lambda_year = 1
stan_month_data$N_month = max(stan_df$month)
stan_month_data$month_id = stan_df$month
stan_month_data$p_gamma_month0 = c(0,1)
mod5 <- cmdstan_model("stan/mod_stan_discourse5.stan")
fit5_month <- mod5$sample(data = stan_month_data,
                          init=0, #this needs to be relaxed later on
                          metric = "dense_e",
                          chains = 4,
                          parallel_chains = 4,
                          iter_sampling = 500,
                          iter_warmup = 1000, #tried 500 but didn't improve much
                          adapt_delta = 0.99,
                          refresh = 10,
                          seed = 1)
saveRDS(fit5_month, file = "results/fit5_month.rds")
fit5_month$diagnostic_summary()
fit5_month$summary() %>% arrange(-rhat) %>% .[,1:10]

fit$summary(variables = c("gamma_month"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>%
  tidyr::extract(variable,into=c("variable","month_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
  as_tibble() %>%
  dplyr::select(month_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
  dplyr::mutate(month_id=as.numeric(month_id)) %>% 
  ggplot(aes(x=month_id,y=est,ymin=lwb,ymax=upb)) +
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue")

#bias: bias for model M1 in some ages
age_bias_df = fit$summary(variables = c("age_bias"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>%
  tidyr::extract(variable,into=c("variable","age_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
  as_tibble() %>%
  dplyr::select(age_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
  dplyr::mutate(age_id=as.numeric(age_id))

age_bias_df %>% 
  ggplot(aes(x=age_id))+
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
  geom_line(aes(y=est),col="darkred")

birth_prob_by_age_df = fit$summary(variables = c("birth_prob"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
  tidyr::extract(variable,into=c("variable","year_id","age_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
  as_tibble() %>% 
  dplyr::select(year_id,age_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
  dplyr::mutate(year_id = as.numeric(year_id),
                age_id=as.numeric(age_id))

birth_prob_by_age_df %>% 
  filter(year_id %in% c(1, 10,24)) %>% 
  ggplot(aes(x=age_id,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(year_id)),alpha=0.2)+
  geom_line(aes(col=factor(year_id)))


gp_df = fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
  tidyr::extract(variable,into=c("variable","year_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
  as_tibble() %>% 
  dplyr::select(year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  dplyr::mutate(year_id=as.numeric(year_id))

gp_df %>% 
  ggplot(aes(x=year_id,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue")


pred_df = fit$summary(variables = c("n_birth_pred"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>%
  tidyr::extract(variable,into=c("variable","row_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
  as_tibble() %>%
  dplyr::select(row_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>%
  cbind(stan_df %>% dplyr::select(year_id1,age_id,n_birth))


pred_df %>% 
  filter(age_id %in% (c(15,16,18,19,20,24,26)-14)) %>% 
  ggplot(aes(x=year_id1))+
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
  geom_line(aes(y=est),col="darkred")+
  geom_point(aes(y=n_birth),size=2)+
  facet_grid(age_id~.,scale="free_y")

pred_df %>% 
  filter(age_id %in% ( c(27:35)-15)) %>% 
  ggplot(aes(x=year_id1))+
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
  geom_line(aes(y=est),col="darkred")+
  geom_point(aes(y=n_birth),size=2)+
  facet_grid(age_id~.,scale="free_y")+
  coord_cartesian(ylim=c(0,1000))

pred_df %>% 
  filter(age_id %in% ( c(36,38,40,42,45,48,50)-15)) %>% 
  ggplot(aes(x=year_id1))+
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
  geom_line(aes(y=est),col="darkred")+
  geom_point(aes(y=n_birth),size=2)+
  facet_grid(age_id~.,scale="free_y")
