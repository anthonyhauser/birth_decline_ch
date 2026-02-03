cmdstan_fit_mod2_semiparam = function(birth_df, pop_df, stan_years = 2000:2024,
                                      mod_name = c("mod2_gp_gp.stan"),
                                      save_draw = FALSE, save.date,
                                      seed_id=123){
  
  #data-------------------------------------------------------------------------
  birth_mod_df = birth_df %>% 
    filter(mother_age %in% 15:50) %>% 
    group_by(year,month,mother_age) %>% 
    dplyr::summarise(n_birth=sum(n),.groups="drop")
  
  pop_mod_df = pop_df %>% 
    group_by(year,month,mother_age=age) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  
  min_age = 25
  
  stan_df =  pop_mod_df %>% 
    left_join(birth_mod_df,by=c("year","month","mother_age")) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  birth_year = year - mother_age) %>% 
    #filter(year %in% stan_years) %>% 
    filter(birth_year >= 1987-min_age) %>% #,year<=2024) %>% 
    dplyr::mutate(age_id = mother_age - min(mother_age) + 1,
                  birth_year_id = birth_year-min(birth_year) + 1,
                  cal_year_id = year-min(year) + 1,
                  #assign year_id (either calendar or birth year)
                  year_id = cal_year_id,
                  year_id1 = birth_year_id,
                  month_id1 = (year_id1-min(year_id1)) * 12 + month)
  
  max_age_shift = 0
  stan_df = stan_df %>% 
    dplyr::mutate(age_id = age_id + max_age_shift)
  
  stan_df = stan_df %>% filter(year<=2024)
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
  
  #Stan list--------------------------------------------------------------------
  stan_data = list(N = dim(stan_df)[1],
                   N_year = length(unique(stan_df$year)),
                   N_year1 = length(unique(stan_df$year_id1)),
                   N_month1 = length(unique(stan_df$month_id1)),
                   N_age = length(unique(stan_df$mother_age)) +2*max_age_shift,
                   N_group = 1,#number of GPs
                   
                   year_id = stan_df$year_id,
                   month_id1 = stan_df$month_id1,
                   year_id1 = stan_df$year_id1,
                   age_id = stan_df$age_id,
                   
                   n_pop = stan_df$n_pop,
                   n_birth = stan_df$n_birth,
                   
                   #x = sort(unique(stan_df$year_id)),
                   x = 1:(max(stan_df$age_id) + max_age_shift), #only for mod1_gp.stan
                   x1 = 1:(max(stan_df$age_id) + max_age_shift),
                   x2 = stan_df$year_id1 %>% unique() %>% sort(),
                   
                   M_year = 8, 
                   c_year = 5,
                   
                   p_rho = c(2,5),
                   p_alpha = c(3,3),
                   
                   lambda_year = 2,
                   p_lambda_year = c(0.5, 1),
                   p_alpha_year = c(0,0.5),
                   
                   p_age_peak1 = c(15,2),
                   p_age_peak2 = c(0,1),
                   
                   p_log_h_peak1 = c(log(0.12)-0.2^2/2,0.2),
                   p_log_h_peak2 = c(0,1),
                   
                   p_birth_prob_sigma1 = c(5,1),
                   p_birth_prob_sigma2 = c(0,0.4),
                   
                   p_inv_sigma = 100,
                   
                   p_delta0 = c(-5,3),
                   p_sigma_rw = c(0,2),
                   p_delta = c(-5,3),
                   
                   inference = 1)
  
  if(mod_name %in% c("mod1_param_2expgp_1fixed.stan", "mod1_param_1gp_1expgp_1fixed.stan")){
    stan_data$N_group = 2
  }else if(mod_name %in% c("mod1_param_3expgp.stan")){
    stan_data$N_group = 3
  }
  
  #checks
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
  
  #init function
  initfun <- function() { list(inv_sigma = rexp(1,stan_data$p_inv_sigma),
                               age_peak1=rnorm(1,stan_data$p_age_peak1[1],stan_data$p_age_peak1[2]),
                               age_peak2=rnorm(1,stan_data$p_age_peak2[1],stan_data$p_age_peak2[2]),
                               h_peak1=abs(rnorm(1,stan_data$p_h_peak1[1],stan_data$p_h_peak1[2])),
                               h_peak2=rnorm(1,stan_data$p_h_peak2[1],stan_data$p_h_peak2[2]),
                               birth_prob_sigma=rnorm(1,stan_data$p_birth_prob_sigma[1],stan_data$p_birth_prob_sigma[2])) }
  
  initfun <- function() { list(delta0=-8) }
  
  #Stan model-------------------------------------------------------------------
  mod <- cmdstan_model(paste0("stan/",mod_name))
  fit <- mod$sample(data = stan_data,
                    #init=initfun, #avoid because some chains finished unexpectedly
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 200,
                    iter_warmup = 500,
                    adapt_delta = 0.8,
                    refresh = 10,
                    seed = 2)
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
  if(mod_name=="mod1_param_3fixed.stan"){
    var = c("inv_sigma","age_peak1","log_h_peak1", "birth_prob_sigma1")
  }else if(mod_name=="mod1_param_1lin_2fixed.stan"){
    var = c("inv_sigma","age_peak1","age_peak2","log_h_peak1", "birth_prob_sigma1")
  }else if(mod_name=="mod1_param_1lin_1explin_1fixed.stan"){
    var = c("inv_sigma","age_peak1","age_peak2","log_h_peak1","log_h_peak2", "birth_prob_sigma1")
  }else if(mod_name=="mod1_param_2explin_1fixed.stan"){
    var = c("inv_sigma","age_peak1","age_peak2","log_h_peak1","log_h_peak2", "birth_prob_sigma1")
  }else if(mod_name %in% c("mod1_param_1expgp_2fixed.stan","mod1_param_2expgp_1fixed.stan","mod1_param_3expgp.stan","mod1_param_1gp_1expgp_1fixed.stan")){
    var = c("inv_sigma","age_peak1","log_h_peak1", "birth_prob_sigma1","alpha_year","lambda_year")
  }else if(mod_name %in% c("mod1_rw1.stan")){
    var = c("inv_sigma","sigma_rw","beta")
  }
  
  #exploration
  if(FALSE){
    fit$summary(variables = var, "mean",~quantile(.x, probs = c(0.025, 0.975)))
    
    #by chain (e.g., when rhat is high)
    d=fit$draws()
    apply(d[, , "age_peak1", drop = TRUE],2,mean)
    cmdstan_est_by_chain(fit, var = c("h_peak1","h_peak2","inv_sigma"), chains=1:4)
    
    
    #trajectory birth_prob
    d_sample <- fit$draws(variables=c("birth_prob"))
    n_iter_per_chain = d_sample[,1,1] %>% length()
    sample_birth_prob = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","month_id","age_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(month_id = as.numeric(month_id),
                    age_id = as.numeric(age_id)) %>% 
      left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") %>% 
      left_join(stan_df %>% dplyr::select(birth_year,month,month_id=month_id1) %>% distinct(),by="month_id") 
    
    
    sample_birth_prob %>% 
      group_by(chain, var,month_id,age_id,mother_age,birth_year) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      filter(birth_year %in% c(1970,2000)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(chain)),alpha=0.2)+
      geom_line(aes(col=factor(chain)))+
      facet_grid(birth_year~.)
    
    sample_birth_prob %>% 
      group_by(chain, var,month_id,age_id,mother_age,birth_year) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      filter(year %in% c(2000,2011,2020)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
      geom_line(aes(col=factor(year)))+
      facet_grid(chain~.)
    
    #trajectory birth_prob
    d_sample <- fit$draws(variables=c("f_year"))
    n_iter_per_chain = d_sample[,1,1] %>% length()
    sample_f_year = as.data.frame(ftable(d_sample[,,])) %>% 
      dplyr::mutate(chain = as.numeric(as.character(chain)),
                    iteration = as.numeric(as.character(iteration)),
                    iter=iteration+n_iter_per_chain*(chain-1)) %>% 
      dplyr::select(chain,iter,var=variable,values=Freq) %>% 
      tidyr::extract(var,into=c("var","year_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(year_id = as.numeric(year_id)) %>% 
      left_join(stan_df %>% dplyr::select(year,year_id) %>% distinct(),by="year_id") 
    
    sample_f_year %>% 
      group_by(chain, var,year_id,year) %>% 
      dplyr::summarise(est = mean(values),
                       lwb = quantile(values,probs=0.025),
                       upb = quantile(values,probs=0.975),.groups="drop") %>% 
      ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(chain)),alpha=0.2)+
      geom_line(aes(col=factor(chain)))
    
    
    
    #trajectory 
    d_sample <- fit$draws(variables=c("n_birth_pred"))
    n_iter_per_chain = d_sample[,1,1] %>% length()
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
    dplyr::mutate(year_id=as.numeric(year_id)) %>% 
    left_join(stan_df %>% dplyr::select(birth_year,year_id=year_id1) %>% distinct(),by="year_id")
  
  if(stan_data$N_group==1){
    gp_df = fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      tidyr::extract(variable,into=c("variable","year_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      as_tibble() %>% 
      dplyr::mutate(group_id=1) %>% 
      dplyr::select(group_id,year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
      dplyr::mutate(year_id=as.numeric(year_id)) %>% 
      left_join(stan_df %>% dplyr::select(year,year_id) %>% distinct(),by="year_id")
  }else{
    gp_df = fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      tidyr::extract(variable,into=c("variable","group_id","year_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      as_tibble() %>% 
      dplyr::select(group_id,year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
      dplyr::mutate(year_id=as.numeric(year_id)) %>% 
      left_join(stan_df %>% dplyr::select(year,year_id) %>% distinct(),by="year_id")
  }
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
      filter(birth_year %in% c(1962,1972,1986,2009)) %>% 
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
      geom_line(col="blue")+
      facet_grid(group_id ~.)
    gp_df %>% 
      ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")+
      facet_grid(group_id ~.)
    #predictive intervals
    pred_df %>% 
      filter(mother_age %in% c(15:18)) %>% 
      ggplot(aes(x=year))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")+
      coord_cartesian(ylim=c(0,500))
    
    
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


