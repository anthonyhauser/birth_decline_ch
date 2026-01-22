cmdstan_fit_mod2 = function(birth_df, pop_df, stan_years = 2000:2024,
                                 mod_name = c("mod2_1expgp_2fixed.stan","mod2_1expgp_1periodic.stan"),
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
  
  stan_df = pop_mod_df %>% 
    left_join(birth_mod_df,by=c("year","month","mother_age")) %>% 
    filter(year %in% stan_years) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  month_id = (year-min(year)) * 12 + month,
                  age_id = mother_age - min(mother_age) + 1)
  #Data plots-------------------------------------------------------------------
  if(FALSE){
    #birth over year
    #pop over year
    #prob birth over year
    #birth over month
    #dist according to mother age
    
    
    
    #Distribution of probability of birth according to mother age
    stan_df %>% 
      filter(year %in% c(2000,2001,2010,2020,2024),month==1) %>% 
      dplyr::mutate(p_birth = n_birth/n_pop,
                    log_p_birth = log(p_birth)) %>% 
      ggplot(aes(x=mother_age, y=p_birth,col=factor(year),group=factor(year)))+
      geom_point()+geom_line()
    #Peak height of probability of birth by year
    stan_df %>% 
      dplyr::mutate(p_birth = n_birth/n_pop,
                    log_p_birth = log(p_birth)) %>% 
      group_by(year) %>% 
      dplyr::summarise(p_peak = max(p_birth),.groups="drop") %>% 
      ggplot(aes(x=year, y=p_peak))+
      geom_point()+geom_line()
    #mean probability of birth by year
    stan_df %>% 
      group_by(year) %>% 
      dplyr::summarise(p_birth = sum(n_birth)/sum(n_pop),
                       log_p_birth = log(p_birth),.groups="drop") %>% 
      ggplot(aes(x=year, y=p_birth))+
      geom_point()+geom_line()+
      expand_limits(y=0)
    #mean probability of birth by year and by mother age: decrease for young age, increase for old due to the increase of mean age at birth
    stan_df %>% 
      filter(mother_age %in% c(25,28,30,32,35,38,40),month==1) %>% 
      dplyr::mutate(p_birth = n_birth/n_pop,
                    log_p_birth = log(p_birth)) %>% 
      ggplot(aes(x=year, y=p_birth,col=factor(mother_age)))+
      geom_point()+geom_line()+
      expand_limits(y=0)
  }
  
  cowplot::plot_grid(
  stan_df %>% 
    dplyr::mutate(date = make_date(year, month, 1)) %>% 
    filter(mother_age  %in% c(25,30,35)) %>% 
    ggplot(aes(x=date,y=n_birth,col=factor(mother_age)))+
    geom_point()+
    expand_limits(y=0)+theme(legend.position = "bottom"),
  stan_df %>% 
    filter(month==1) %>% 
    ggplot(aes(x=mother_age,y=n_birth,col=year))+
    geom_point()+
    expand_limits(y=0)+theme(legend.position = "bottom"),
  ncol=2,labels=c("A) Number of birth over time","B) Number of birth by mother age"))
  
  
  stan_df %>% 
    group_by(year,month) %>% 
    dplyr::summarise(p_birth=sum(n_birth)/sum(n_pop)) %>% ungroup() %>% 
    dplyr::mutate(year_gp = as.numeric(year>=2010)+as.numeric(year>=2020)) %>% 
    ggplot(aes(x=month,y=p_birth,col=factor(year)))+
    geom_point()+geom_line()+
    facet_grid(year_gp~.)
  
  stan_df %>% 
    dplyr::mutate(age_group = cut(mother_age,c(0,15,20,25,30,35,40,Inf))) %>% 
    group_by(year,month,age_group) %>% 
    dplyr::summarise(p_birth=sum(n_birth)/sum(n_pop)) %>% ungroup() %>% 
    dplyr::mutate(year_gp = as.numeric(year>=2010)+as.numeric(year>=2020)) %>% 
    ggplot(aes(x=month,y=p_birth,col=factor(year)))+
    geom_point()+geom_line()+
    facet_wrap(.~age_group)
  
  stan_df %>% 
    dplyr::mutate(date = make_date(year, month, 1)) %>% 
    filter(mother_age %in% c(20,21)) %>% 
    ggplot(aes(x=date,y=n_pop,col=factor(mother_age)))+
    geom_point()+geom_line()
  
  
  #Stan list--------------------------------------------------------------------
  stan_data = list(N = dim(stan_df)[1],
                   N_year = length(unique(stan_df$year)),
                   N_month = length(unique(stan_df$month_id)),
                   N_age = length(unique(stan_df$mother_age)),
                   N_group = 1,#number of GPs
                   
                   month_id = stan_df$month_id,
                   age_id = stan_df$age_id,
                   
                   n_pop = stan_df$n_pop,
                   n_birth = stan_df$n_birth,
                   
                   x = sort(unique(stan_df$month_id)),
                   
                   M_year = 50, 
                   c_year = 5,
                   J_month = 20,
                   
                   p_lambda_year = c(log(2), 0.2),
                   p_alpha_year = c(0,0.5),
                   
                   p_lambda_month = c(1, 0.5),
                   p_alpha_month  = c(0,0.5),
                   
                   p_age_peak1 = c(15,2),
                   p_age_peak2 = c(0,1),
                   
                   p_log_h_peak1 = c(log(0.01)-0.2^2/2,0.2),
                   p_log_h_peak2 = c(0,1),
                   
                   p_birth_prob_sigma1 = c(5,1),
                   p_birth_prob_sigma2 = c(0,0.4),
                   
                   p_inv_sigma = 1,
                   
                   inference = 1)
  
  if(mod_name %in% c("mod1_param_2expgp_1fixed.stan", "mod1_param_1gp_1expgp_1fixed.stan")){
    stan_data$N_group = 2
  }else if(mod_name %in% c("mod1_param_3expgp.stan")){
    stan_data$N_group = 3
  }
  
  #checks
  if(FALSE){
    #lengthscale
    l = exp(rnorm(1000,stan_data$p_lambda_year[1],stan_data$p_lambda_year[2]))
    xn = (stan_data$x-mean(stan_data$x))/sd(stan_data$x)
    tuning_parameter_cond_EQ(l, xn)
    
    #intercept mean
    log(sum(stan_df$n_birth[stan_df$age_id==1])/sum(stan_df$n_pop[stan_df$age_id==1]))
    
    #intercept distribution
    data.frame(x= exp(rnorm(100000,stan_data$p_log_h_peak1[1],stan_data$p_log_h_peak1[2]))) %>% 
      ggplot(aes(x=x))+geom_histogram()+
      expand_limits(x=0)
    
    data.frame(x= exp(rnorm(100000,0.8,0.8))) %>% 
      ggplot(aes(x=x))+geom_histogram()+#xlim(c(0,10))
      expand_limits(x=0)
    #lambda
    data.frame(x= exp(rnorm(100000,stan_data$p_lambda_year[1],stan_data$p_lambda_year[2]))) %>% 
      ggplot(aes(x=x))+geom_histogram()+xlim(c(0,10))
      expand_limits(x=0)
    
    #p_h_peak and log_p_h_peak
    data.frame(y = c(rnorm(100000,stan_data$p_h_peak1[1],stan_data$p_h_peak1[2]),
                     exp(rnorm(100000,stan_data$p_log_h_peak1[1],stan_data$p_log_h_peak1[2]))),   
               dist =rep(c("p_h_peak","log_p_h_peak"),each=100000)) %>% 
      ggplot(aes(x = y, fill = dist)) +
      geom_histogram(alpha = 0.4, position = "identity", bins = 40) +
      scale_x_continuous() 
  }
  
  #init function
  initfun <- function() { list(lamba_year=exp(rnorm(1,stan_data$p_lambda_year[1],stan_data$p_lambda_year[2]))) }
  initfun <- function() { list(inv_sigma=1,
                               age_peak1=15,
                               log_h_peak=-4) }
  
  #Stan model-------------------------------------------------------------------
  mod <- cmdstan_model(paste0("stan/",mod_name))

  fit <- mod$sample(data = stan_data,
                    #init=initfun,#init=initfun,
                    chains = 4,
                    parallel_chains = 4,
                    iter_sampling = 200,
                    iter_warmup = 500,
                    adapt_delta = 0.99,
                    refresh = 50,
                    seed = 1)
  
  if(save_draw){
    fit$save_object(file = paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name,".RDS"))
  }
  
  if(FALSE){
    #load
    fit <- readRDS(paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name,".RDS"))
  }
  
  
  v=fit$sampler_diagnostics(format = "df")[["stepsize__"]] %>% mean()
  
  quantile(v,probs=c(0.25,0.5,0.975))
  
  #diagnostic of the fit
  cmdstan_diag = cmdstan_diagnostic(fit)
  print(cmdstan_diag)
  
  #Checks----------------------------------------------------------
  #diagnostic of the fit
  
  #variables
  if(mod_name=="mod2_1expgp_2fixed.stan"){
    var = c("inv_sigma","age_peak1","log_h_peak1", "birth_prob_sigma1","alpha_year","lambda_year")
  }else if(mod_name=="mod2_1expgp_1periodic.stan"){
    var = c("inv_sigma","age_peak1","log_h_peak1", "birth_prob_sigma1","alpha_year","lambda_year","alpha_month","lambda_month")
  }
  
  #exploration
  if(FALSE){
    fit$summary(variables = var, "mean",~quantile(.x, probs = c(0.025, 0.975)))
    
    #by chain (e.g., when rhat is high)
    d=fit$draws()
    apply(d[, , "age_peak1", drop = TRUE],2,mean)
    cmdstan_est_by_chain(fit, var = c("alpha_year","lambda_year","inv_sigma"), chains=1:4)
    
    shinystan::launch_shinystan(fit)
  }
  
  #Posterior estimates----------------------------------------------------------
  par_df =  fit$summary(variables = var, "mean",~quantile(.x, probs = c(0.025, 0.975)))
  birth_prob_by_age_df = fit$summary(variables = c("birth_prob"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","month_id","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(month_id,age_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(month_id = as.numeric(month_id),
                  age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") %>% 
    left_join(stan_df %>% dplyr::select(year,month,month_id) %>% distinct(),by="month_id") %>% 
    dplyr::mutate(date = make_date(year, month, 1))
  
  gp_year_df = fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","month_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(month_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(month_id=as.numeric(month_id)) %>% 
    left_join(stan_df %>% dplyr::select(year,month,month_id) %>% distinct(),by="month_id") %>% 
    dplyr::mutate(date = make_date(year, month, 1))
  
  gp_month_df = fit$summary(variables = c("f_month"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","month_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(month_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(month_id=as.numeric(month_id)) %>% 
    left_join(stan_df %>% dplyr::select(year,month,month_id) %>% distinct(),by="month_id") %>% 
    dplyr::mutate(date = make_date(year, month, 1))
  
  pred_df = fit$summary(variables = c("n_birth_pred"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","row_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(row_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    cbind(stan_df %>% dplyr::select(year,month,mother_age,n_birth)) %>% 
    dplyr::mutate(date = make_date(year, month, 1))
  
  pois_pred_df = fit$summary(variables = c("n_birth_pois_pred"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","row_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(row_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    cbind(stan_df %>% dplyr::select(year,month,mother_age,n_birth)) %>% 
    dplyr::mutate(date = make_date(year, month, 1))

  
  
  
  if(FALSE){
    #main parameters
    par_df
    #parametric function
    birth_prob_by_age_df %>% 
      filter(year %in% c(2000,2010,2021,2024),month==1) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
      geom_line(aes(col=factor(year)))
    #GP
    gp_year_df %>% 
      ggplot(aes(x=date,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")#facet_grid(group_id ~.)
    
    gp_month_df %>%
      filter(year<=2001) %>% 
      ggplot(aes(x=date,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    
    pred_df %>% 
      filter(mother_age %in% c(15,16,18,20,22,24,26)) %>% 
      ggplot(aes(x=date))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
    
    pred_df %>% 
      filter(mother_age %in% c(28:34)) %>% 
      ggplot(aes(x=date))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
    
    pred_df %>% 
      filter(mother_age %in% c(36,38,40,42,45,48,50)) %>% 
      ggplot(aes(x=date))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
    
    rbind(pred_df %>% dplyr::mutate(dist="negbin"),
          pois_pred_df %>% dplyr::mutate(dist="pois")) %>% 
      filter(mother_age %in% c(18,22,30,32,34,46)) %>% 
      ggplot(aes(x=date))+
      geom_ribbon(aes(ymin=lwb,ymax=upb,fill=dist),alpha=0.2)+
      facet_grid(mother_age~.,scale="free_y")
      
  }
  
  return(list(mod_name = mod_name,
              seed_id = seed_id,
              cmdstan_diag = cmdstan_diag,
              par_df = par_df,
              birth_prob_by_age_df = birth_prob_by_age_df,
              gp_df = gp_df,
              pred_df = pred_df))
}


