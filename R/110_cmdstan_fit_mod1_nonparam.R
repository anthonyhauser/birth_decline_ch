cmstan_fit_mod1_nonparam = function(birth_df, pop_df,stan_years = 2000:2024,
                                dist = "negbin",#"normal"
                                cut_age_group_year_gp = NULL, #c(28,35)
                                seed_id=123){
  #data-------------------------------------------------------------------------
  birth_mod_df = birth_df %>% 
    filter(!is.na(mother_mun_id),mother_age %in% 15:50) %>% 
    group_by(year,mother_age) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  pop_mod_df = pop_df %>% 
    filter(month==7) %>% 
    group_by(year,mother_age=age) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  
  stan_df = pop_mod_df %>% 
    left_join(birth_mod_df,by=c("year","mother_age")) %>% 
    filter(year %in% stan_years) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  year_id = year-min(year) + 1,
                  age_id = mother_age - min(mother_age) + 1,
                  age_group_id = cut(mother_age,
                                      breaks = c(0,cut_age_group_year_gp,Inf),
                                      labels = FALSE,
                                      right = FALSE))
  #Data plots-------------------------------------------------------------------
  if(FALSE){
    stan_df %>% 
      filter(year %in% c(2000,2001,2010,2020,2024)) %>% 
      dplyr::mutate(p_birth = n_birth/n_pop,
                    log_p_birth = log(p_birth)) %>% 
      ggplot(aes(x=mother_age, y=p_birth,col=factor(year),group=factor(year)))+
      geom_point()+geom_line()
    
    
    stan_df %>% 
      dplyr::mutate(p_birth = n_birth/n_pop,
                    log_p_birth = log(p_birth)) %>% 
      group_by(year) %>% 
      dplyr::summarise(p_peak = max(p_birth),.groups="drop") %>% 
      ggplot(aes(x=year, y=p_peak))+
      geom_point()+geom_line()
    
    stan_df %>% 
      group_by(year) %>% 
      dplyr::summarise(p_birth = sum(n_birth)/sum(n_pop),
                       log_p_birth = log(p_birth),.groups="drop") %>% 
      ggplot(aes(x=year, y=p_birth))+
      geom_point()+geom_line()+
      expand_limits(y=0)
    
    stan_df %>% 
      filter(mother_age %in% c(25,28,30,32,35,38,40)) %>% 
      dplyr::mutate(p_birth = n_birth/n_pop,
                    log_p_birth = log(p_birth)) %>% 
      ggplot(aes(x=year, y=p_birth,col=factor(mother_age)))+
      geom_point()+geom_line()+
      expand_limits(y=0)
  }
  
  #Stan list--------------------------------------------------------------------
  stan_data = list(N = dim(stan_df)[1],
                   N_year = length(unique(stan_df$year)),
                   N_age = length(unique(stan_df$mother_age)),
                   N_group_year = length(unique(stan_df$age_group_id)),
                   
                   year_id = stan_df$year_id,
                   age_id = stan_df$age_id,
                   group_year_id = stan_df$age_group_id,
                   
                   n_pop = stan_df$n_pop,
                   n_birth = stan_df$n_birth,
                   
                   x = sort(unique(stan_df$year_id)),
                   y = sort(unique(stan_df$age_id)),
                   
                   M_year = 8, 
                   c_year = 5,
                   M_age = 8, 
                   c_age = 5,
                   
                   p_intercept = c(-4,1),
                   
                   p_lambda_year = c(0.5, 1),
                   p_alpha_year = c(0,2),
                   p_lambda_age = c(0.5, 1),
                   p_alpha_age = c(0,2),
                   
                   p_inv_sigma = 10,
                   
                   inference = which(dist == c("negbin","normal")))
  
  #checks
  #lengthscale
  l = exp(rnorm(1000,stan_data$p_lambda_year[1],stan_data$p_lambda_year[1]))
  xn = (stan_data$x-mean(stan_data$x))/sd(stan_data$x)
  tuning_parameter_cond_EQ(l, xn)
  l = exp(rnorm(1000,stan_data$p_lambda_age[1],stan_data$p_lambda_age[1]))
  yn = (stan_data$y-mean(stan_data$y))/sd(stan_data$y)
  tuning_parameter_cond_EQ(l, yn)
  
  #intercept
  log(sum(stan_df$n_birth[stan_df$age_id==1])/sum(stan_df$n_pop[stan_df$age_id==1]))
  
  #Stan model-------------------------------------------------------------------
  mod <- cmdstan_model("stan/mod1_nonparam.stan")
  fit <- mod$sample(data = stan_data,
                    #init=initfun, #avoid because some chains finished unexpectedly
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 200,
                    iter_warmup = 200,
                    #adapt_delta = 0.95,
                    refresh = 10,
                    seed = seed_id)
  
  #diagnostic of the fit
  cmdstan_diagnostic(fit)
  
  #checks estimates
  if(FALSE){
    #estimates
    fit$summary(variables = c("intercept","lambda_year","alpha_year","lambda_age","alpha_age"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
    
    #by chain (e.g., when rhat is high)
    d=fit$draws()
    apply(d[, , "age_peak1", drop = TRUE],2,mean)
    cmdstan_est_by_chain(fit, var = c("h_peak1","h_peak2","inv_sigma"), chains=1:4)
    
    shinystan::launch_shinystan(fit)
  }
  
  
  #Plots------------------------------------------------------------------------
  if(FALSE){
    #GP age
    fit$summary(variables = c("f_age"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      tidyr::extract(variable,into=c("variable","age_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      as_tibble() %>% 
      dplyr::select(age_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
      dplyr::mutate(age_id=as.numeric(age_id)) %>% 
      left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    #GP year
    fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      tidyr::extract(variable,into=c("variable","group_year_id","year_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      as_tibble() %>% 
      dplyr::select(year_id,group_year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
      dplyr::mutate(year_id=as.numeric(year_id),
                    group_year_id = as.numeric(group_year_id)) %>% 
      left_join(stan_df %>% dplyr::select(year,year_id) %>% distinct(),by="year_id") %>% 
      ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(group_year_id)),alpha=0.2)+
      geom_line(aes(col=factor(group_year_id)))
  }
  
  
  #Posterior estimates----------------------------------------------------------
  # par_df
  # birth_prob_by_age_df
  # gp_df
  # pred_birth_df
  return("Posterior estimate missing in the script.")
}


