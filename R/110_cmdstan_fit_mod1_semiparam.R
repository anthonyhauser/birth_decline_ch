cmdstan_fit_mod1_semiparam = function(birth_df, pop_df, stan_years = 2000:2024,
                                  mod_name = c("mod1_rw1.stan","mod1_gp_gp.stan","mod1_gp_gp_diff2.stan",
                                               "mod1_gp_gp_diff2_lengthscale.stan",
                                               "mod1_gp_diff.stan"),
                                  effect_on_age_shift = c("birth_year","cal_year"),
                                  save_draw = FALSE, save.date,
                                  seed_id=123){
  effect_on_age_shift = "birth_year"
  effect_on_age_shift = "cal_year"
  #data-------------------------------------------------------------------------
  #to do:
  #try by calendar year
  #increase c_age, because boundary condition (but already at 5 so should be ok)
  #try to understand sum of sin curve is it same as fourier transform
  #scale at f=0
  #log(logit)
  ##############################################################################
  #year aggregation
  #population
  pop_mod_df = pop_df %>% 
    filter(month==7) %>% #take population size at the middle of the year
    group_by(year,mother_age=age) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  #birth
  birth_mod_df = birth_df %>% 
    filter(mother_age %in% 15:50) %>% 
    group_by(year,mother_age) %>% 
    dplyr::summarise(n_birth=sum(n),.groups="drop")

  #bind pop and birth
  stan_df = pop_mod_df %>% 
    filter(year<=2024) %>% 
    left_join(birth_mod_df,by=c("year","mother_age")) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  birth_year = year - mother_age,
                  age_id = mother_age - min(mother_age) + 1) 
  #year_id1
  if(effect_on_age_shift=="birth_year"){
    min_age = 25
    stan_df = stan_df %>% 
      filter(birth_year >= 1987-min_age) %>% 
      dplyr::mutate(year_id1 = birth_year-min(birth_year) + 1)
  }else if(effect_on_age_shift=="cal_year"){
    stan_df = stan_df %>% 
      filter(year %in% stan_years) %>% 
      dplyr::mutate(year_id1 = year-min(year) + 1)
  }

  ##############################################################################
  #month aggregation
  #population
  pop_mod_df = pop_df %>% 
    group_by(year,mother_age=age,month) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  #birth
  birth_mod_df = birth_df %>% 
    filter(mother_age %in% 15:50) %>% 
    group_by(year,mother_age,month) %>% 
    dplyr::summarise(n_birth=sum(n),.groups="drop")
  
  #bind pop and birth
  stan_df = pop_mod_df %>% 
    filter(year<=2024) %>% 
    left_join(birth_mod_df,by=c("year","mother_age","month")) %>% 
    filter(month %in% c(1:12)) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  birth_year = year - mother_age,
                  age_id = mother_age - min(mother_age) + 1) 
  #year_id1
  if(effect_on_age_shift=="birth_year"){
    min_age = 25
    stan_df = stan_df %>% 
      filter(birth_year >= 1987-min_age) %>% 
      dplyr::mutate(year_id1 = birth_year-min(birth_year) + 1)
  }else if(effect_on_age_shift=="cal_year"){
    stan_df = stan_df %>% 
      filter(year %in% stan_years) %>% 
      dplyr::mutate(year_id1 = year-min(year) + 1)
  }
  
  #Data plots-------------------------------------------------------------------
  if(FALSE){
    age_min = 22
    age_max = 38
    birth_year_min = 1987-age_min
    birth_year_max = 2024-age_max
    d = pop_mod_df %>% 
      left_join(birth_mod_df,by=c("year","mother_age")) %>% 
      dplyr::mutate(birth_year = year-mother_age,
                    p_birth = n_birth/n_pop) %>% 
      filter(birth_year>=birth_year_min, birth_year<=birth_year_max,
             mother_age>=age_min, mother_age<=age_max)
    d %>% 
      filter(birth_year %in% c(1967,1970,1975,1980,1985,1990)) %>% 
      ggplot(aes(x=mother_age,y=p_birth,col=factor(birth_year)))+
        geom_line(size=1)

    #mean and median
    d %>% group_by(birth_year) %>%
      arrange(mother_age, .by_group = TRUE) %>%
      summarise(mean = sum(n_birth * mother_age) / sum(n_birth),
                median = mother_age[min(which(cumsum(n_birth) / sum(n_birth) >= 0.5))]) %>%
      ggplot(aes(x = birth_year)) + geom_line(aes(y = mean), linewidth = 1) +
      geom_line(aes(y = median), linetype = 2, linewidth = 1)
    
    
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
  
  stan_df0=stan_df
  stan_df = stan_df0 %>% filter(month %in% 1:12)
  
  if(FALSE){
    set.seed(123)
    
    simulated_stan_df <- stan_df %>%
      dplyr::select(age_id, year_id1, n_pop, n_birth) %>%
      group_by(year_id1, age_id) %>%
      slice_sample(prop = 1) %>%   # shuffle within group
      ungroup() %>%
      mutate(n_birth_sim = round(n_birth * runif(n(), 0.99, 1.01)))
    saveRDS(stan_df, "data/stan_df.RDS")
    saveRDS(simulated_stan_df, "data/simulated_stan_full_df.RDS")
    saveRDS(simulated_stan_df %>% dplyr::select(-n_birth_sim) , "data/simulated_stan_df.RDS")
    
    
    simulated_stan_df %>%
      filter(age_id %in% c(5,10,15,20,30)) %>% 
      ggplot(aes(x=year_id1,y=n_birth_sim))+
      geom_point(aes(y=n_birth),color="green")+
      geom_point()+
      facet_grid(age_id ~ .,scale="free_y")+
      expand_limits(y=0)
  }
  
  #Stan list--------------------------------------------------------------------
  stan_data = list(N = dim(stan_df)[1],
                   N_year1 = length(unique(stan_df$year_id1)),
                   N_age = length(unique(stan_df$mother_age)),
                   N_sigma =1,# length(unique(stan_df$mother_age)) +2*max_age_shift,
                   
                   year_id1 = stan_df$year_id1,
                   age_id = stan_df$age_id,
                   sigma_id = rep(1,length(stan_df$age_id)),#stan_df$age_id, 
                   
                   n_pop = stan_df$n_pop,
                   n_birth = stan_df$n_birth,
                   
                   x1 = 1:max(stan_df$age_id),
                   x2 = stan_df$year_id1 %>% unique() %>% sort(),
                   
                   M_year = 10, 
                   c_year = 5,
                   
                   M_age = 25, 
                   c_age = 5,
                   
                   rho=0.6,
                   p_rho = c(2,5),
                   p_alpha = c(3,1),
                   
                   lambda_year = 2,
                   p_lambda_year = c(2, 2),
                   p_alpha_year = c(2,2),
                   
                   p_inv_sigma = 100,
                   p_delta0 = c(-5-log(12),2),
                   
                   inference = 1)
  
  #Check prior values
  if(FALSE){
    #lengthscale
    l = exp(rnorm(1000,stan_data$p_lambda_year[1],stan_data$p_lambda_year[1]))
    xn = (stan_data$x-mean(stan_data$x))/sd(stan_data$x)
    tuning_parameter_cond_EQ(l, xn)
    
    #intercept
    log(sum(stan_df$n_birth[stan_df$age_id==1])/sum(stan_df$n_pop[stan_df$age_id==1]))
    
    #p_h_peak and log_p_h_peak
    data.frame(y = c(rnorm(100000,stan_data$p_h_peak1[1],stan_data$p_h_peak1[2]),
                     exp(rnorm(100000,stan_data$p_log_h_peak1[1],stan_data$p_log_h_peak1[2]))),   
               dist =rep(c("p_h_peak","log_p_h_peak"),each=100000)) %>% 
      ggplot(aes(x = y, fill = dist)) +
      geom_histogram(alpha = 0.4, position = "identity", bins = 40) +
      scale_x_continuous() 
  }
  
  #Check newer version gives same results---------------------------------------
  if(FALSE){
    mod1 <- cmdstan_model(paste0("stan/","mod1_gp_gp_diff.stan"))
    mod2 <- cmdstan_model(paste0("stan/","mod1_gp_gp_diff2.stan"))
    fit1 <- mod1$sample(data = stan_data,
                        chains = 1,
                        parallel_chains = 1,
                        iter_sampling = 1,
                        iter_warmup = 0,
                        fixed_param = TRUE,
                        seed = 1)
    fit2 <- mod2$sample(data = stan_data,
                        chains = 1,
                        parallel_chains = 1,
                        iter_sampling = 1,
                        iter_warmup = 0,
                        fixed_param = TRUE,
                        seed = 1)
    
    fit1$draws(variables = "f_year[1]")
    fit2$draws(variables = "f_year[1]")
    
    fit1$draws(variables = "f_age[1,1]")
    fit2$draws(variables = "f_age[1,1]")
    fit1$draws(variables = "f_age[36,46]")
    fit2$draws(variables = "f_age[36,46]")
  }
  
  #init functiob 
  initfun <- function() { list(delta0=-8) }
  
  #Stan model-------------------------------------------------------------------
  mod <- cmdstan_model(paste0("stan/",mod_name))
  fit <- mod$sample(data = stan_data,
                    init=0,#init=initfun, #avoid because some chains finished unexpectedly
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 200,
                    iter_warmup = 200,
                    adapt_delta = 0.8,
                    refresh = 10,
                    seed = 1)
  if(save_draw){
    fit$save_object(file = paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name,".RDS"))
  }
  fit$summary() %>% arrange(-rhat) %>% View()
  
  
  #diagnostic of the fit
  cmdstan_diag = cmdstan_diagnostic(fit)
  print(cmdstan_diag)
  
  #Checks----------------------------------------------------------
  #diagnostic of the fit
  
  #variables
  if(mod_name %in% c("mod1_gp_diff.stan")){
    var = c("delta0","alpha", "inv_sigma","sigma","rho")
  }else if(mod_name %in% c("mod1_gp_gp_diff.stan","mod1_gp_gp_diff2.stan")){
    var = c("inv_sigma","sigma","delta0","alpha","alpha_year", "rho","lambda_year")
  }
  
  #exploration
  if(FALSE){
    fit$summary(variables = var, "mean",~quantile(.x, probs = c(0.025, 0.975)))
    
    #by chain (e.g., when rhat is high)
    d=fit$draws()
    apply(d[, , "lambda_year", drop = TRUE],2,mean)
    cmdstan_est_by_chain(fit, var = c("h_peak1","h_peak2","inv_sigma"), chains=1:4)
    
    #trajectory birth_prob
    d_sample <- fit$draws(variables=c("birth_prob"))
    n_iter_per_chain = d_sample[,1,1] %>% length()
    sample_birth_prob = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","year_id","age_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(year_id = as.numeric(year_id),
                    age_id = as.numeric(age_id)) %>% 
      left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id")
    
    
    sample_birth_prob %>% 
      group_by(chain, var,year_id,age_id,mother_age) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      filter(year_id %in% c(1,20,40)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(chain)),alpha=0.2)+
      geom_line(aes(col=factor(chain)))+
      facet_grid(year_id~.)
    
    sample_birth_prob %>% 
      group_by(chain, var,year_id,age_id,mother_age) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      filter(year_id %in% c(1,20,40)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(year_id)),alpha=0.2)+
      geom_line(aes(col=factor(year_id)))+
      facet_grid(chain~.)
    
    #trajectory f_age two dimensions
    d_sample <- fit$draws(variables=c("f_age"))
    f_age_df = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","age_id","year_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(year_id = as.numeric(year_id),
                    age_id = as.numeric(age_id)) 
    
    f_age_df %>%  
      group_by(chain, var,age_id) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      ggplot(aes(x=age_id,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(chain)),alpha=0.2)+
      geom_line(aes(col=factor(chain)))+
      facet_grid(chain~.)
    
    
    #trajectory f_age
    d_sample <- fit$draws(variables=c("f_age"))
    sample_f_age = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","age_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(age_id = as.numeric(age_id)) %>% 
      left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") 
    
    sample_f_age %>%  
      group_by(chain, var,age_id,mother_age) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(chain)),alpha=0.2)+
      geom_line(aes(col=factor(chain)))
    
    #delta0
    d_sample <- fit$draws(variables=c("delta0"))
    delta0_df = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq)
    
    delta0_df %>% 
      group_by(chain, var) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") 
    
    #trajectory logit_birth_prob_int
    d_sample <- fit$draws(variables=c("logit_birth_prob_int"))
    sample_logit_birth_prob_int = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","age_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(age_id = as.numeric(age_id)) %>% 
      left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") 
    
    sample_logit_birth_prob_int %>% 
      group_by(chain, var,age_id,mother_age) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(chain)),alpha=0.2)+
      geom_line(aes(col=factor(chain)))
    
    #trajectory f_year
    d_sample <- fit$draws(variables=c("f_year"))
    sample_f_year = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","year_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(year_id = as.numeric(year_id))
    
    sample_f_year %>% 
      group_by(chain,var,year_id) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      ggplot(aes(x=year_id,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(chain)),alpha=0.2)+
      geom_line(aes(col=factor(chain)))
    
    #trajectory n_birth_pred
    d_sample <- fit$draws(variables=c("n_birth_pred"))
    n_birth_pred = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","data_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(data_id = as.numeric(data_id)) %>% 
      left_join(stan_df %>% dplyr::select(mother_age,age_id,year,year_id,n_birth ) %>% 
                  dplyr::mutate(data_id = row_number()),by="data_id")
    
    n_birth_pred %>% 
      group_by(chain, var,year_id,age_id,mother_age,year) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),
                       n_birth = n_birth[1],.groups="drop") %>% 
      filter(mother_age %in% c(15,18)) %>% 
      ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(mother_age)),alpha=0.2)+
      geom_line(aes(col=factor(mother_age)))+
      geom_point(aes(y=n_birth,col=factor(mother_age)))+
      facet_grid(chain~.,scales="free_y")
    
    ############################################################################
    #Shinystan
    shinystan::launch_shinystan(fit)
  }
  
  #Posterior estimates----------------------------------------------------------
  par_df =  fit$summary(variables = var, "mean",~quantile(.x, probs = c(0.025, 0.975)))
  #birth probability
  birth_prob_by_age_df = fit$summary(variables = c("birth_prob"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","year_id","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(year_id,age_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id = as.numeric(year_id),
                  age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") %>% 
    left_join(stan_df %>% dplyr::select(year,year_id=year_id1) %>% distinct(),by="year_id") 
  
  birth_prob_by_age_df = fit$summary(variables = c("birth_prob"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","year_id","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(year_id,age_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id = as.numeric(year_id),
                  age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") %>% 
    left_join(stan_df %>% dplyr::select(birth_year,year_id=year_id1) %>% distinct(),by="year_id") 
  
  
  logit_birth_prob_int_df = fit$summary(variables = c("logit_birth_prob_int"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(age_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") 
  #GP
  gp_df = fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","year_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::mutate(group_id=1) %>% 
    dplyr::select(group_id,year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id=as.numeric(year_id)) #%>% 
    #left_join(stan_df %>% dplyr::select(birth_year,year_id=year_id1) %>% distinct(),by="year_id")
  
  #sigma
 inv_sigma_df = fit$summary(variables = c("inv_sigma"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(age_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(age_id=as.numeric(age_id))
  
  
  #prediction
  pred_df =NA
  pois_pred_df=NA
  age_bias_df = NA
  
  pred_df = fit$summary(variables = c("n_birth_pred"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>%
    tidyr::extract(variable,into=c("variable","row_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
    as_tibble() %>%
    dplyr::select(row_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>%
    cbind(stan_df %>% dplyr::select(year,mother_age,n_birth))
  
  pois_pred_df = fit$summary(variables = c("n_birth_pois_pred"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>%
    tidyr::extract(variable,into=c("variable","row_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
    as_tibble() %>%
    dplyr::select(row_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>%
    cbind(stan_df %>% dplyr::select(year,mother_age,n_birth))
  
  # #bias
  age_bias_df = fit$summary(variables = c("age_bias"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>%
    tidyr::extract(variable,into=c("variable","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
    as_tibble() %>%
    dplyr::select(age_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
    dplyr::mutate(age_id=as.numeric(age_id)) %>%
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") %>%
    left_join(stan_df %>% group_by(age_id) %>% dplyr::summarise(n_tot_birth=sum(n_birth)),by="age_id")
  age_bias_df = rbind(age_bias_df %>%  dplyr::mutate(indicator="absolute"),
                      age_bias_df %>%  dplyr::mutate(indicator="relative") %>%
                        dplyr::mutate(  dplyr::across(c(est, lwb, upb), ~ .x / n_tot_birth) ))
  
  
  if(FALSE){
    #main parameters
    par_df
    #parametric function
    birth_prob_by_age_df %>% 
      filter(year %in% c(2000,2010,2020)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
      geom_line(aes(col=factor(year)))
    birth_prob_by_age_df %>% 
      filter(birth_year %in% c(1962,1990,2000)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(birth_year)),alpha=0.2)+
      geom_line(aes(col=factor(birth_year)))
    logit_birth_prob_int_df %>% 
      ggplot(aes(x=age_id,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(alpha=0.2)+
      geom_line()
    #GP
    gp_df %>% 
      ggplot(aes(x=birth_year,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    gp_df %>% 
      ggplot(aes(x=year_id,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    #inv_sigma
    inv_sigma_df %>% 
      ggplot(aes(x=age_id,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    #predictive intervals
    
    pred_df %>% 
      filter(mother_age %in% c(15,16,18,19,20,22,24,26)) %>% 
      ggplot(aes(x=year))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
    pred_df %>% 
      filter(mother_age %in% c(27:35)) %>% 
      ggplot(aes(x=year))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
    pred_df %>% 
      filter(mother_age %in% c(36,38,40,42,45,48,50)) %>% 
      ggplot(aes(x=year))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
    #predictive intervals: poisson and negbin
    rbind(pred_df %>% dplyr::mutate(dist="negbin"),
          pois_pred_df %>% dplyr::mutate(dist="pois")) %>% 
      filter(mother_age %in% c(18,22,30,32,34,46)) %>% 
      ggplot(aes(x=year))+
      geom_ribbon(aes(ymin=lwb,ymax=upb,fill=dist),alpha=0.2)+
      facet_grid(mother_age~.,scale="free_y")
    
    age_bias_df %>% 
      ggplot(aes(x=mother_age))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      facet_grid(indicator~.,scales="free")
  }
  
  return(list(mod_name = mod_name,
              seed_id = seed_id,
              cmdstan_diag = cmdstan_diag,
              par_df = par_df,
              birth_prob_by_age_df = birth_prob_by_age_df,
              gp_df = gp_df,
              pred_df = pred_df,
              age_bias_df = age_bias_df))
}



nb_loglik_individual <- function(data, mu, size) {
  data |>
    dplyr::mutate(
      ll = dnbinom(
        x = y,
        size = size,
        mu = mu,
        log = TRUE
      )
    )
}


df <- data.frame(y = c(0, 10, 20))

data.frame(mean=seq(1,20,by=0.01)) %>% 
  rowwise() %>% 
  dplyr::mutate(ll=nb_loglik_individual(data = df,mu = mean, size = 5) %>% pull(ll) %>% sum) %>% 
  ggplot(aes(x=mean,y=ll))+geom_line()

