summarise_excess_birth_mun = function(excess_birth_year_adj_mun_draw_df, excess_birth_year_mun_draw_df){
  #excess by year and mun_id
  excess_birth_year_mun_df = excess_birth_year_mun_draw_df %>% 
    #summarise by year and mun_id
    dplyr::summarise(n_exp_mean = mean(n_pred),
                     n_exc_mean = mean(n_exc),
                     n_exc_lwb = quantile(n_exc,probs=0.025),
                     n_exc_upb = quantile(n_exc,probs=0.975),.groups="drop") %>% 
    dplyr::mutate(rel_exc_mean = n_exc_mean/n_exp_mean,
                  rel_exc_lwb = n_exc_lwb/n_exp_mean,
                  rel_exc_upb = n_exc_upb/n_exp_mean) 
  #excess by year and mun_id, adjacency
  excess_birth_year_adj_mun_df = excess_birth_year_adj_mun_draw_df %>% 
    #summarise by year and mun_id
    dplyr::summarise(n_exp_mean = mean(n_pred),
                     n_exc_mean = mean(n_exc),
                     n_exc_lwb = quantile(n_exc,probs=0.025),
                     n_exc_upb = quantile(n_exc,probs=0.975),.groups="drop") %>% 
    dplyr::mutate(rel_exc_mean = n_exc_mean/n_exp_mean,
                  rel_exc_lwb = n_exc_lwb/n_exp_mean,
                  rel_exc_upb = n_exc_upb/n_exp_mean) 
  
  #excess by mun_id
  excess_birth_year_adj_mun_draw_df = excess_birth_year_adj_mun_draw_df %>% 
    filter(year %in% 2017:2024) %>% 
    group_by(mun_id,mun_name,draw) %>% 
    dplyr::summarise(adj_n_pred = sum(n_pred),
                     adj_n_exc = sum(n_exc),.groups="drop_last")
  #excess by mun_id, adjacency
  excess_birth_year_mun_draw_df = excess_birth_year_mun_draw_df %>% 
    filter(year %in% 2017:2024) %>% 
    group_by(mun_id,mun_name,draw) %>% 
    dplyr::summarise(adj_n_pred = sum(n_pred),
                     adj_n_exc = sum(n_exc),.groups="drop_last")
  
  
  saveRDS(excess_birth_year_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_mun_df",".RDS"))
  saveRDS(excess_birth_year_adj_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj_mun_df",".RDS"))
  saveRDS(excess_birth_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_mun_df",".RDS"))
  saveRDS(excess_birth_adj_mun_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_adj_mun_df",".RDS"))
  
  
  return(list(excess_birth_year_mun_df = excess_birth_year_mun_df,
              excess_birth_year_adj_mun_df = excess_birth_year_adj_mun_df,
              excess_birth_mun_df = excess_birth_mun_df,
              excess_birth_adj_mun_df = excess_birth_adj_mun_df))
}