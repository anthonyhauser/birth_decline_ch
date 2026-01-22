cmdstan_fit_mod3 = function(birth_df, pop_df, final_mun_df, stan_years = 2000:2024,
                          mod_name = c("mod3_1expgp_1periodic.stan"),
                          seed_id=123){
  
  stan_years = 2003:2010
  #data-------------------------------------------------------------------------
  #add municipality and i
  # birth_mun_df = birth_df %>% 
  #   filter(mother_municipality<8100) %>% 
  #   left_join(final_mun_df %>% dplyr::select(hist_mun_id,mun_id,dist_id,ctn_abbr),by=c("mother_municipality"="hist_mun_id"))
  # if(FALSE){
  #   #check that no missing ctn
  #   birth_mun_df %>% filter(is.na(ctn_abbr))
  # }
  birth_mod_df = birth_mun_df %>% 
    filter(mother_age %in% 15:50) %>% 
    group_by(year,month,mother_age,ctn_abbr) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  pop_mod_df = pop_df %>% 
    group_by(year,month,mother_age=age,ctn_abbr) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  
  stan_df = pop_mod_df %>% 
    left_join(birth_mod_df,by=c("year","month","mother_age","ctn_abbr")) %>% 
    filter(year %in% stan_years) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  month_id = (year-min(year)) * 12 + month,
                  age_id = mother_age - min(mother_age) + 1,
                  reg_id = as.numeric(factor(ctn_abbr)))
  #Data plots-------------------------------------------------------------------
  if(FALSE){
  
  }
  
  
  #Stan list--------------------------------------------------------------------
  stan_data = list(N = dim(stan_df)[1],
                   N_year = length(unique(stan_df$year)),
                   N_month = length(unique(stan_df$month_id)),
                   N_age = length(unique(stan_df$mother_age)),
                   N_reg =  length(unique(stan_df$reg_id)),
                   
                   month_id = stan_df$month_id,
                   age_id = stan_df$age_id,
                   reg_id = stan_df$reg_id,
                   
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
                   
                   p_beta_reg = 1,
                   
                   p_inv_sigma = 1,
                   
                   inference = 1)
  
  #checks
  if(FALSE){
  }
  
  #init function
  
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
                    seed = seed_id)
  
  #diagnostic of the fit
  cmdstan_diag = cmdstan_diagnostic(fit)
  print(cmdstan_diag)
  
  #Checks----------------------------------------------------------
  #diagnostic of the fit
  
  #variables
  if(mod_name=="mod3_1expgp_1periodic.stan"){
    var = c("inv_sigma","age_peak1","log_h_peak1", "birth_prob_sigma1","alpha_year","lambda_year","alpha_month","lambda_month","beta_reg")
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
  gp_df = fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","group_id","month_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(group_id,month_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(month_id=as.numeric(month_id)) %>% 
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
      filter(year<=2003) %>% 
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


