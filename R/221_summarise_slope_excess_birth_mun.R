get_slope_excess_est = function(var_group,draw_df){
  slope_excess_df = draw_df %>%
    #for each draw sum by var_group
    group_by(across(all_of(var_group)), year, draw) %>% 
    dplyr::summarise(n_pred = sum(n_pred),
                     n_exc = sum(n_exc),.groups="drop_last") %>% 
    #define relative excess as excess/mean(expectation) for each draw
    dplyr::mutate(rel_exc = n_exc/mean(n_pred)) %>% 
    #calculate the slope for each draw
    group_by(across(all_of(var_group)), draw) %>%
    arrange(year, .by_group = TRUE) %>%
    dplyr::summarise(n     = dplyr::n(),
                     slope = 12 / (n * (n^2 - 1)) * (sum(seq_len(n) * rel_exc) - (n + 1) / 2 * sum(rel_exc)), .groups = "drop" ) %>%
    #summarise slope over draws
    group_by(across(all_of(var_group))) %>%
    dplyr::summarise(slope_rel_exc_mean = mean(slope),
                     slope_rel_exc_lwb  = quantile(slope, probs = 0.025),
                     slope_rel_exc_upb  = quantile(slope, probs = 0.975), .groups = "drop") 
  return(slope_excess_df)
}

summarise_slope_excess_birth_mun = function(excess_birth_year_adj_mun_draw_df, excess_birth_year_adj2_mun_draw_df, excess_birth_year_mun_draw_df,
                                            save.date, mod_name, seed_id, res_path = "results/", year_range=2017:2024){
  
  slope_excess_birth_year_mun_df = get_slope_excess_est(var_group=c("mun_id","mun_name"),excess_birth_year_mun_draw_df %>% filter(year %in% year_range))
  slope_excess_birth_year_adj_mun_df = get_slope_excess_est(var_group=c("mun_id","mun_name"),excess_birth_year_adj_mun_draw_df %>% filter(year %in% year_range))
  slope_excess_birth_year_adj2_mun_df = get_slope_excess_est(var_group=c("mun_id","mun_name"),excess_birth_year_adj2_mun_draw_df %>% filter(year %in% year_range))
  
  saveRDS(slope_excess_birth_year_mun_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","slope_excess_birth_year_mun_df",".RDS"))
  saveRDS(slope_excess_birth_year_adj_mun_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","slope_excess_birth_year_adj_mun_df",".RDS"))
  saveRDS(slope_excess_birth_year_adj2_mun_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","slope_excess_birth_year_adj2_mun_df",".RDS"))
  
  return(list(slope_excess_birth_year_mun_df = slope_excess_birth_year_mun_df,
              slope_excess_birth_year_adj_mun_df = slope_excess_birth_year_adj_mun_df,
              slope_excess_birth_year_adj2_mun_df = slope_excess_birth_year_adj2_mun_df))
}

summarise_slope_excess_birth_ctzreg = function(excess_birth_year_ctz_draw_df,
                                               save.date, mod_name, seed_id, res_path = "results/",year_range=2017:2024){
  
  slope_excess_birth_year_ctzreg_ctn_df =  get_slope_excess_est(var_group=c("ctz_region","ctn_abbr"), excess_birth_year_ctz_draw_df %>% filter(year %in% year_range))
  slope_excess_birth_year_ctzreg_df =  get_slope_excess_est(var_group=c("ctz_region"), excess_birth_year_ctz_draw_df %>% filter(year %in% year_range))
  
  saveRDS(slope_excess_birth_year_ctzreg_ctn_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","slope_excess_birth_year_ctzreg_ctn_df",".RDS"))
  saveRDS(slope_excess_birth_year_ctzreg_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","slope_excess_birth_year_ctzreg_df",".RDS"))
  
  return(list(slope_excess_birth_year_ctzreg_ctn_df = slope_excess_birth_year_ctzreg_ctn_df,
              slope_excess_birth_year_ctzreg_df = slope_excess_birth_year_ctzreg_df))
}
