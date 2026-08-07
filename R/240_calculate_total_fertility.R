calculate_total_fertility = function(save.date,mod_name_full, seed_id){
  fit = readRDS(paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name_full,"_seedid",seed_id,".RDS"))
  stan_df = readRDS(paste0("results/2025/", mod_name_full, "_standf.RDS"))
  
  log_mean_n_birth_n_pop = stan_df %>% group_by(mother_age) %>% dplyr::summarise(est = log(mean(n_birth/n_pop))) %>%
    pull(est) %>% mean
  
  gamma_month_draw_df = fit$draws("gamma_month",format = "df")  %>%
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    pivot_longer( cols = starts_with("gamma_month["),names_to = "var",values_to = "gamma_month") %>% 
    tidyr::extract(var,into=c("var","month_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(month_id = as.numeric(month_id))
  
  log_f_age_draw_df = fit$draws("f_age",format = "df")  %>%
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    pivot_longer( cols = starts_with("f_age["),names_to = "var",values_to = "log_f_age") %>% 
    tidyr::extract(var,into=c("var","age_id","year_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(age_id = as.numeric(age_id),
                  year_id = as.numeric(year_id))
  
  total_fertility = log_f_age_draw_df %>% dplyr::select(-var) %>% 
    left_join(gamma_month_draw_df %>% dplyr::select(-var),by=c("chain","draw","iter"),relationship = "many-to-many") %>% 
    dplyr::mutate(f_age_month = exp(log_f_age + gamma_month + log_mean_n_birth_n_pop)) %>% 
    group_by(year_id,draw) %>% 
    dplyr::summarise(total_f_age = sum(f_age_month), .groups="drop_last") %>% 
    dplyr::summarise(mean = mean(total_f_age),
                     lwb = quantile(total_f_age,probs=0.025),
                     upb = quantile(total_f_age,probs=0.975),.groups="drop") %>% 
    #we keep the last year (2025) but all years give the same result
    filter(year_id==26)
  
  return(total_fertility %>% filter(year_id==26))
}

