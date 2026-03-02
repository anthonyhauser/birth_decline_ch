cmstan_fit_mod5 = function(pop_df, birth_agg_df,
                           mod_name ="mod5",
                           stan_years=  2000:2024,
                           effect_on_age_shift = "cal_year",
                           save_draw = FALSE, save.date,
                           seed_id = 123){
  
  if(FALSE){
    mod_name ="mod5"
    pop_df
    birth_agg_df
    effect_on_age_shift="birth_year"
    seed_id=1
  }
  mod_name0 = mod_name#used to compile model
  mod_name = if_else(effect_on_age_shift=="cal_year",mod_name,paste0(mod_name,"_birthyear")) #used as text to name saved results
  #Load data--------------------------------------------------------------------
  #population
  pop_mod_df = pop_df %>% 
    group_by(year,mother_age=age,month) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  #birth
  birth_mod_df = birth_agg_df %>% 
    filter(mother_age %in% 15:50) %>% 
    group_by(year,mother_age,month) %>% 
    dplyr::summarise(n_birth=sum(n),.groups="drop")
  
  #bind pop and birth
  stan_df = pop_mod_df %>% 
    filter(year<=2024) %>% 
    left_join(birth_mod_df,by=c("year","mother_age","month")) %>% 
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
  saveRDS(stan_df, paste0("results/",mod_name,"_standf.RDS"))
  
  #Stan list--------------------------------------------------------------------
  stan_month_data = list(N = dim(stan_df)[1],
                         N_year1 = length(unique(stan_df$year_id1)),
                         N_month = max(stan_df$month),
                         N_age = length(unique(stan_df$age_id)),
                         N_sigma =1,
                         
                         year_id1 = stan_df$year_id1,
                         month_id = stan_df$month,
                         age_id = stan_df$age_id,
                         sigma_id = rep(1,length(stan_df$age_id)),
                         
                         n_pop = stan_df$n_pop,
                         n_birth = stan_df$n_birth,
                         
                         log_mean_n_birth_n_pop = log(mean(stan_df$n_birth)/mean(stan_df$n_pop)),
                         
                         x1 = 1:max(stan_df$age_id),
                         x2 = stan_df$year_id1 %>% unique() %>% sort(),
                         
                         M_age = 25, 
                         c_age = 1.5,
                         
                         M_year = 10, 
                         c_year = 1.5,
                         
                         rho = 0.6,
                         lambda_year = 1,
                         
                         #p_rho = c(2,5),
                         p_alpha = c(3,1),
                         #p_lambda_year = c(2, 2),
                         p_alpha_year = c(2,2),
                         p_inv_sigma = 100,
                         p_gamma_month0 = c(0,1),
                         
                         inference = 1)
  stan_month_data$N
  
  #Stan model-------------------------------------------------------------------
  mod5 <- cmdstan_model(paste0("stan/",mod_name0,".stan"))
  fit <- mod5$sample(data = stan_month_data,
                            init=0, #this needs to be relaxed later on
                            metric = "dense_e",
                            chains = 4,
                            parallel_chains = 4,
                            iter_sampling = 500,
                            iter_warmup = 1000,
                            adapt_delta = 0.99,
                            refresh = 10,
                            seed = seed_id)
  
  if(save_draw){
    fit$save_object(file = paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name,"_seedid",seed_id,".RDS"))
    #saveRDS(fit, file = paste("results/fit_",save.date,"_",mod_name,".RDS"))
  }
  if(FALSE){
    fit <- readRDS(paste0(code_root_path, "results/cmdstan_draw/", save.date, "_", mod_name,"_seedid",seed_id,".RDS"))
  }
  
  
  #diagnostic
  stan_diag_df = cmdstan_diagnostic(fit)
  
  # fit$diagnostic_summary()
  # fit$summary() %>% arrange(-rhat) %>% .[,1:10]
  
  #posterior
  var=c("alpha","alpha_year","inv_sigma","gamma_month")
  #paremeters
  par_df = fit$summary(variables = var, "mean",~quantile(.x, probs = c(0.025, 0.975)))
  #birth probability by age
  birth_prob_by_age_df = fit$summary(variables = c("birth_prob"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","year_id","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(year_id,age_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id = as.numeric(year_id),
                  age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(year,mother_age,birth_year,age_id,year_id=year_id1) %>% distinct(),by=c("age_id","year_id")) 
  #gp
  gp_df = fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("variable","year_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id=as.numeric(year_id))
  
  #gp, relative to first year
  gp_rel_df = fit$draws("f_year",format = "df") %>%
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    pivot_longer( cols = starts_with("f_year["),names_to = "var",values_to = "value") %>% 
    tidyr::extract(var,into=c("var","year_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(year_id = as.numeric(year_id)) %>% 
    #difference with respect to first year
    group_by(draw) %>% 
    dplyr::mutate(value= value - value[year_id==1],
                  value = -value) %>% ungroup() %>% 
    #summarise
    group_by(year_id) %>% 
    dplyr::summarise(est = mean(value),
                     lwb = quantile(value, probs = 0.025),
                     upb = quantile(value, probs = 0.975))
    
  
  if(grepl("birth_year",mod_name)){
    gp_df = gp_df %>% 
      left_join(stan_df %>% dplyr::select(birth_year,year_id=year_id1) %>% distinct(),by="year_id") 
    gp_rel_df = gp_rel_df %>% 
      left_join(stan_df %>% dplyr::select(birth_year,year_id=year_id1) %>% distinct(),by="year_id") 
  }else{
    gp_df = gp_df %>% 
      left_join(stan_df %>% dplyr::select(year,year_id=year_id1) %>% distinct(),by="year_id") 
    gp_rel_df = gp_rel_df %>% 
      left_join(stan_df %>% dplyr::select(year,year_id=year_id1) %>% distinct(),by="year_id") 
  }
  #bias
  age_bias_df = fit$summary(variables = c("age_bias"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>%
    tidyr::extract(variable,into=c("variable","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
    as_tibble() %>%
    dplyr::select(age_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
    dplyr::mutate(age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") 
  
  #prediction
  pred_df = fit$summary(variables = c("n_birth_pred"), "median",~quantile(.x, probs = c(0.025, 0.975))) %>%
    tidyr::extract(variable,into=c("variable","row_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
    as_tibble() %>%
    dplyr::select(row_id,est=median,lwb=`2.5%`,upb=`97.5%`) %>%
    cbind(stan_df %>% dplyr::select(year_id=year_id1,month,age_id,n_birth)) %>% 
    left_join(stan_df %>% dplyr::select(year,mother_age,birth_year,age_id,year_id=year_id1) %>% distinct(),by=c("age_id","year_id")) %>% 
    dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
  
  saveRDS(stan_diag_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_standiag.RDS"))
  saveRDS(par_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_par.RDS"))
  saveRDS(birth_prob_by_age_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_birthprob.RDS"))
  saveRDS(gp_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp.RDS"))
  saveRDS(gp_rel_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp_rel.RDS"))
  saveRDS(age_bias_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_agebias.RDS"))
  saveRDS(pred_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_pred.RDS"))
  
  return(list(stan_diag_df = stan_diag_df,
              par_df = par_df,
              birth_prob_by_age_df = birth_prob_by_age_df,
              gp_df = gp_df,
              age_bias_df = age_bias_df,
              pred_df = pred_df,
              par_df = par_df))
  
  if(FALSE){
    stan_diag_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_standiag.RDS"))
    par_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_par.RDS"))
    birth_prob_by_age_df=  readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_birthprob.RDS"))
    gp_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp.RDS"))
    gp_rel_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp_rel.RDS"))
    age_bias_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_agebias.RDS"))
    pred_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_pred.RDS"))
    par_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_par.RDS"))
    
    
    par_df %>%
      filter(str_detect(variable, "gamma_month")) %>% 
      tidyr::extract(variable,into=c("variable","month_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
      as_tibble() %>%
      dplyr::select(month_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
      dplyr::mutate(month_id=as.numeric(month_id)) %>% 
      ggplot(aes(x=month_id,y=est,ymin=lwb,ymax=upb)) +
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    
    birth_prob_by_age_df %>% 
      filter(year_id %in% c(1, 10,24,40)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
      geom_line(aes(col=factor(year)))
    
    gp_df %>% 
      ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    
    age_bias_df %>% 
      ggplot(aes(x=mother_age))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")
    
    pred_df %>% 
      filter(age_id %in% (c(15,16,18,19,20,24,26)-14)) %>% 
      ggplot(aes(x=year_id))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
    
    pred_df %>% 
      filter(age_id %in% ( c(27,29,31,33,35)-15)) %>% 
      ggplot(aes(x=year_id))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")+
      coord_cartesian(ylim=c(0,1000))
    
    pred_df %>% 
      filter(age_id %in% ( c(36,38,40,42,45,48,50)-15)) %>% 
      ggplot(aes(x=year_id))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(age_id~.,scale="free_y")
  }
}