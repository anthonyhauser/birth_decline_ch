cmstan_fit_mod5 = function(pop_df, birth_agg_df,
                           mod_name ="mod5",
                           stan_years=  2000:2024,
                           effect_on_age_shift = "cal_year",
                           save_draw = FALSE, save.date,
                           N_add_age = 4,
                           filter_ctz = c("swiss","non-swiss"),
                           filter_parity = "all",#filter_parity="first"
                           seed_id = 123,
                           use_cmdstanr = get("use_cmdstanr", envir=.GlobalEnv)){
  
  if(FALSE){
    mod_name ="mod8"
    pop_df
    birth_agg_df #birth_agg_first_df
    stan_years=  2000:2024 #stan_years=  2005:2024
    effect_on_age_shift="cal_year"
    seed_id=1
    filter_ctz = c("swiss","non-swiss")
    filter_parity = "all"
    N_add_age = 4
  }
  # backend-agnostic helpers
  if(use_cmdstanr){
    fit_summary = function(fit, vars){
      fit$summary(variables = vars, "mean", ~quantile(.x, probs = c(0.025, 0.975))) %>%
        as.data.frame() %>%
        dplyr::rename(lwb = `2.5%`, upb = `97.5%`)
    }
    get_draws_df = function(fit, var){
      fit$draws(var, format="df") %>%
        dplyr::rename(chain=.chain, iter=.iteration, draw=.draw)
    }
  } else {
    fit_summary = function(fit, vars){
      rstan::summary(fit, pars=vars)$summary %>%
        as.data.frame() %>%
        dplyr::mutate(variable=rownames(.)) %>%
        dplyr::rename(lwb=`2.5%`, upb=`97.5%`)
    }
    get_draws_df = function(fit, var){
      draws_mat = rstan::extract(fit, pars=var, permuted=TRUE)[[var]]  # n_draws x N_year
      colnames(draws_mat) = paste0(var, "[", seq_len(ncol(draws_mat)), "]")
      as.data.frame(draws_mat) %>% dplyr::mutate(draw=dplyr::row_number())
    }
  }

  mod_name0 = mod_name#used to compile model
  mod_name = if_else(effect_on_age_shift=="cal_year",mod_name,paste0(mod_name,"_birthyear")) #used as text to name saved results
  mod_name = paste0(mod_name,ifelse(length(filter_ctz)==2,"",paste0("_",filter_ctz))) #add whether we filter on citizenship or not
  mod_name = if_else(filter_parity=="all",mod_name,paste0(mod_name,"_",filter_parity)) #used as text to name saved results
  mod_name = if_else(last_year!=2025,mod_name,paste0(mod_name,"_",last_year))
  
  #Load data--------------------------------------------------------------------
  #population
  pop_mod_df = pop_df %>% 
    filter(citizenship %in% filter_ctz) %>% 
    group_by(year,mother_age=age,month) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  #birth
  birth_mod_df = birth_agg_df %>% 
    filter(citizenship %in% filter_ctz) %>% 
    filter(mother_age %in% 15:50) %>% 
    group_by(year,mother_age,month) %>% 
    dplyr::summarise(n_birth=sum(n),.groups="drop")
  
  #bind pop and birth
  stan_df = pop_mod_df %>% 
    filter(year<=last_year) %>% 
    left_join(birth_mod_df,by=c("year","mother_age","month")) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  birth_year = year - mother_age,
                  age_id2 = mother_age - min(mother_age) + 1,
                  age_id = age_id2 + N_add_age,
                  )
  #year_id1
  if(effect_on_age_shift=="birth_year"){
    min_age = 25
    stan_df = stan_df %>% 
      filter(birth_year>=1965, birth_year<=1998,year>=1990) %>% 
      #filter(birth_year >= 1987-min_age) %>% 
      dplyr::mutate(year_id1 = birth_year-min(birth_year) + 1,
                    year_id2 = year-min(year) + 1)
  }else if(effect_on_age_shift=="cal_year"){
    stan_df = stan_df %>% 
      filter(year %in% stan_years) %>% 
      dplyr::mutate(year_id1 = year-min(year) + 1,
                    year_id2 = year-min(year) + 1)
  }
  
  
  
  saveRDS(stan_df, paste0("results/", mod_name, "_standf.RDS"))
  
  if(FALSE){
    stan_df %>% group_by(birth_year) %>% dplyr::summarise(min_age = min(mother_age),
                                                          max_age = max(mother_age),
                                                          min_year = min(year),
                                                          max_year = max(year)) %>% View()
    
    stan_df %>% group_by(mother_age) %>% dplyr::summarise(n=n()) %>% View()
    
    stan_df %>% group_by(year) %>% dplyr::summarise(n=n()/12) %>% View()
    
    stan_df %>% filter(mother_age==15) %>% 
      group_by(year) %>% dplyr::summarise(n_birthsum(n_birth))
  }
  
  #Stan list--------------------------------------------------------------------
  stan_month_data = list(N = dim(stan_df)[1],
                         N_year1 = length(unique(stan_df$year_id1)),
                         N_year2 = length(unique(stan_df$year_id2)),
                         N_month = max(stan_df$month),
                         N_age = length(unique(stan_df$age_id)) + 2*N_add_age,
                         N_age2 = length(unique(stan_df$age_id)),
                         N_sigma =1,
                         
                         year_id1 = stan_df$year_id1,
                         year_id2 = stan_df$year_id2,
                         month_id = stan_df$month,
                         age_id = stan_df$age_id,
                         age_id2 = stan_df$age_id2,
                         sigma_id = rep(1,length(stan_df$age_id)),
                         
                         n_pop = stan_df$n_pop,
                         n_birth = stan_df$n_birth,
                         
                         log_mean_n_birth_n_pop = stan_df %>% group_by(mother_age) %>% dplyr::summarise(est = log(mean(n_birth/n_pop))) %>% pull(est) %>% mean,
                         
                         x1 = 1:(max(stan_df$age_id) + N_add_age),
                         x2 = stan_df$year_id1 %>% unique() %>% sort(),
                         x3 = 1:max(stan_df$age_id - N_add_age),
                         
                         M_age = 25, 
                         c_age = 1.5,
                         
                         M_year = 10, 
                         c_year = 1.5,
                         
                         rho = 0.6,
                         lambda_year = 1,
                         lambda_sigma = 1,
                         
                         #p_rho = c(2,5),
                         p_alpha = c(3,1),
                         p_alpha_year = c(2,2),
                         p_alpha_sigma = c(2,2),
                         p_inv_sigma = 100,
                         p_gamma_month0 = c(0,1),
                         p_sigma_year = 1,
                         
                         inference = 1)
  stan_month_data$N
  
  #Stan model-------------------------------------------------------------------
  if(use_cmdstanr){
    mod5 <- cmdstan_model(paste0("stan/",mod_name0,".stan"))
    fit  <- mod5$sample(data = stan_month_data,
                        init = 0,
                        metric = "dense_e",
                        chains = 4,
                        parallel_chains = 4,
                        iter_sampling = 500,
                        iter_warmup = 1000,
                        adapt_delta = 0.99,
                        refresh = 10,
                        seed = seed_id)
  } else {
    mod5 <- rstan::stan_model(paste0("stan/",mod_name0,".stan"))
    fit  <- rstan::sampling(mod5,
                            data    = stan_month_data,
                            init    = 0,
                            chains  = 4,
                            cores   = 4,
                            warmup  = 1000,
                            iter    = 1500,
                            control = list(adapt_delta=0.99, metric="dense_e"),
                            refresh = 10,
                            seed    = seed_id)
  }
  
  if(save_draw){
    if(use_cmdstanr){
      fit$save_object(file = paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name,"_seedid",seed_id,".RDS"))
    } else {
      saveRDS(fit, file = paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name,"_seedid",seed_id,".RDS"))
    }
  }
  if(FALSE){
    fit <- readRDS(paste0(code_root_path, "results/cmdstan_draw/", save.date, "_", mod_name,"_seedid",seed_id,".RDS"))
    
    fit1 <- readRDS(paste0(code_root_path, "results/cmdstan_draw/20260309_mod8_seedid1.RDS"))
    fit2 <- readRDS(paste0(code_root_path, "results/cmdstan_draw/20260309_mod8_birthyear_seedid1.RDS"))
    fit3 <- readRDS(paste0(code_root_path, "results/cmdstan_draw/20260309_mod8_swiss_seedid1.RDS"))
    fit4 <- readRDS(paste0(code_root_path, "results/cmdstan_draw/20260309_mod8_non-swiss_seedid1.RDS"))
    
    d1 = fit1$summary(variables = c("n_birth_pred"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
    dim(d1)
    d2 = fit2$summary(variables = c("n_birth_pred"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
    dim(d2)
    d3 = fit3$summary(variables = c("n_birth_pred"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
    dim(d3)
    d4 = fit$summary(variables = c("n_birth_pred"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
    dim(d4)
  }
  
  #diagnostic
  stan_diag_df = if(use_cmdstanr) cmdstan_diagnostic(fit) else rstan_diagnostic(fit)
  
  # fit$diagnostic_summary()
  # fit$summary() %>% arrange(-rhat) %>% .[,1:10]
  
  #posterior
  var=c("alpha","alpha_year","inv_sigma")
  #paremeters
  par_df = fit_summary(fit, var)

  #fixed effect of month: gamma_month
  gamma_month_df = fit_summary(fit, c("gamma_month")) %>%
    tidyr::extract(variable,into=c("variable","month_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(month_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(month_id = as.numeric(month_id))
  
  #random effect of calendar year: b_year
  b_year_df = fit_summary(fit, c("b_year")) %>%
    tidyr::extract(variable,into=c("variable","year_id2"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(year_id2,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id2 = as.numeric(year_id2)) %>% 
    left_join(stan_df %>% dplyr::select(year,year_id2) %>% distinct(),by=c("year_id2")) 
  
  #overdispersion by age: sigma_year
  sigma_year_df = fit_summary(fit, c("sigma")) %>%
    tidyr::extract(variable,into=c("variable","age_id2"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(age_id2,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(age_id2 = as.numeric(age_id2)) %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id2) %>% distinct(),by=c("age_id2")) 
  
  #birth probability by age
  birth_prob_by_age_df = fit_summary(fit, c("birth_prob")) %>%
    tidyr::extract(variable,into=c("variable","year_id","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(year_id,age_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id = as.numeric(year_id),
                  age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(year,mother_age,birth_year,age_id,year_id=year_id1) %>% distinct(),by=c("age_id","year_id")) %>% 
    filter(!is.na(mother_age)) #remove rows with NA because of N_add_age
  
  #gp: increase in mother age over year (calendar or birth year)
  gp_df = fit_summary(fit, c("f_year")) %>%
    tidyr::extract(variable,into=c("variable","year_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
    dplyr::mutate(year_id=as.numeric(year_id))
  
  if(FALSE){
    #gp age (used for fertility curve): only used to check model
    gp_age_df = fit$summary(variables = c("f_age"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      tidyr::extract(variable,into=c("variable","age_id","year_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      as_tibble() %>% 
      dplyr::select(age_id,year_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
      dplyr::mutate(age_id=as.numeric(age_id),
                    year_id = as.numeric(year_id))
  }
  
  #gp, relative to first year
  gp_rel_df = get_draws_df(fit, "f_year") %>%
    pivot_longer(cols = starts_with("f_year["), names_to = "var", values_to = "value") %>%
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
                     upb = quantile(value, probs = 0.975),.groups="drop")
    
  #adapt gp whether birth_year or cal_year
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
  age_bias_df = fit_summary(fit, c("age_bias")) %>%
    tidyr::extract(variable,into=c("variable","age_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
    as_tibble() %>%
    dplyr::select(age_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
    dplyr::mutate(age_id=as.numeric(age_id)) %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id") 
  
  #prediction
  pred_df = fit_summary(fit, c("n_birth_pred")) %>%
    tidyr::extract(variable,into=c("variable","row_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>%
    as_tibble() %>%
    dplyr::select(row_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
    cbind(stan_df %>% dplyr::select(year_id=year_id1,month,age_id,n_birth)) %>% 
    left_join(stan_df %>% dplyr::select(year,mother_age,birth_year,age_id,year_id=year_id1) %>% distinct(),by=c("age_id","year_id")) %>% 
    dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
  
  saveRDS(stan_diag_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_standiag.RDS"))
  saveRDS(par_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_par.RDS"))
  saveRDS(gamma_month_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gammamonth.RDS"))
  saveRDS(b_year_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_byear.RDS"))
  saveRDS(sigma_year_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_sigmayear.RDS"))
  saveRDS(birth_prob_by_age_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_birthprob.RDS"))
  saveRDS(gp_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp.RDS"))
  saveRDS(gp_rel_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp_rel.RDS"))
  saveRDS(age_bias_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_agebias.RDS"))
  saveRDS(pred_df, paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_pred.RDS"))
  
  return(list(stan_diag_df = stan_diag_df,
              gamma_month_df = gamma_month_df,
              b_year_df = b_year_df,
              sigma_year_df = sigma_year_df,
              par_df = par_df,
              birth_prob_by_age_df = birth_prob_by_age_df,
              gp_df = gp_df,
              age_bias_df = age_bias_df,
              pred_df = pred_df,
              par_df = par_df))
  
  if(FALSE){
    stan_diag_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_standiag.RDS"))
    par_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_par.RDS"))
    gamma_month_df=  readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gammamonth.RDS"))
    b_year_df=  readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_byear.RDS"))
    sigma_year_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_sigmayear.RDS"))
    birth_prob_by_age_df=  readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_birthprob.RDS"))
    gp_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp.RDS"))
    gp_rel_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_gp_rel.RDS"))
    age_bias_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_agebias.RDS"))
    pred_df = readRDS(paste0("results/",save.date,"_",mod_name,"_seedid",seed_id,"_pred.RDS"))
    
    b_year_df %>% 
      ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb)) +
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    
    birth_prob_by_age_df %>% 
      filter(year_id %in% c(1, 10,25)) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
      geom_line(aes(col=factor(year)))
    
    gp_df %>% 
      ggplot(aes(x=year_id,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    
    gp_rel_df %>% 
      ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(fill="blue",alpha=0.2)+
      geom_line(col="blue")
    
    sigma_year_df %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
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
      facet_grid(mother_age~.,scale="free_y")
    
    pred_df %>% 
      filter(age_id %in% ( c(36,38,40,42,45,48,50)-15)) %>% 
      ggplot(aes(x=year_id))+
      geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2,fill="darkred")+
      geom_line(aes(y=est),col="darkred")+
      geom_point(aes(y=n_birth),size=2)+
      facet_grid(mother_age~.,scale="free_y")
  }
}