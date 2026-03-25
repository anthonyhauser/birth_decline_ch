get_excess_est = function(var_group,draw_df){
  excess_df = draw_df %>%
    group_by(across(all_of(var_group)), draw) %>% 
    dplyr::summarise(n_birth = sum(n_birth),
                     n_pred = sum(n_pred),
                     n_exc = sum(n_exc),.groups="drop_last") %>% 
    dplyr::summarise(n_birth = n_birth[1],
                     n_exp_mean = mean(n_pred),
                     n_exp_lwb = quantile(n_pred,probs=0.025),
                     n_exp_upb = quantile(n_pred,probs=0.9725),
                     n_exc_mean = mean(n_exc),
                     n_exc_lwb = quantile(n_exc,probs=0.025),
                     n_exc_upb = quantile(n_exc,probs=0.975),.groups="drop") %>% 
    dplyr::mutate(rel_exc_mean = n_exc_mean/n_exp_mean,
                  rel_exc_lwb = n_exc_lwb/n_exp_mean,
                  rel_exc_upb = n_exc_upb/n_exp_mean) 
  return(excess_df)
}

summarise_excess_birth_mun = function(excess_birth_year_adj_mun_draw_df, excess_birth_year_adj2_mun_draw_df, excess_birth_year_mun_draw_df){
  
  excess_birth_year_mun_df = get_excess_est(var_group=c("mun_id","mun_name","year"),excess_birth_year_mun_draw_df)
  excess_birth_year_adj_mun_df = get_excess_est(var_group=c("mun_id","mun_name","year"),excess_birth_year_adj_mun_draw_df)
  excess_birth_year_adj2_mun_df = get_excess_est(var_group=c("mun_id","mun_name","year"),excess_birth_year_adj2_mun_draw_df)
  excess_birth_mun_df = get_excess_est(var_group=c("mun_id","mun_name"), excess_birth_year_mun_draw_df %>% filter(year %in% 2017:2024))
  excess_birth_adj_mun_df = get_excess_est(var_group=c("mun_id","mun_name"), excess_birth_year_adj_mun_draw_df %>%  filter(year %in% 2017:2024))
  excess_birth_adj2_mun_df = get_excess_est(var_group=c("mun_id","mun_name"), excess_birth_year_adj2_mun_draw_df %>%  filter(year %in% 2017:2024))
  
  saveRDS(excess_birth_year_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_mun_df",".RDS"))
  saveRDS(excess_birth_year_adj2_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj2_mun_df",".RDS"))
  saveRDS(excess_birth_year_adj_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj_mun_df",".RDS"))
  saveRDS(excess_birth_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_mun_df",".RDS"))
  saveRDS(excess_birth_adj_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_adj_mun_df",".RDS"))
  saveRDS(excess_birth_adj2_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_adj2_mun_df",".RDS"))
  
  return(list(excess_birth_year_mun_df = excess_birth_year_mun_df,
              excess_birth_year_adj_mun_df = excess_birth_year_adj_mun_df,
              excess_birth_mun_df = excess_birth_mun_df,
              excess_birth_adj_mun_df = excess_birth_adj_mun_df))
}