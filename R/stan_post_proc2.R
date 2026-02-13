get_pred_birth_draw_by_dist = function(fit, #cmdstanr fit
                                       pop_dist_df,#pop by district
                                       birth_df, #birth_df (individual level)
                                       n_draw_subset = 100){
  
  if(FALSE){
    fit = fit5_month
    n_draw_subset = 100
    pop_dist_d
    birth_df
  }
  
  #number of draws
  n_draws = fit$num_chains() * fit$metadata()$iter_sampling
  
  #prediction numbers at the national level (i.e., level at which data is fitted)
  pred_n_birth_draw_df = fit$draws("n_birth_pred",format = "df")  %>%
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    pivot_longer( cols = starts_with("n_birth_pred["),names_to = "var",values_to = "n_pred") %>% 
    tidyr::extract(var,into=c("var","data_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(data_id = as.numeric(data_id)) %>% 
    left_join(stan_df %>% dplyr::select(age=mother_age,year,month,n_pop,n_birth) %>% 
                dplyr::mutate(data_id = row_number()),by="data_id") %>% 
    dplyr::select(draw,year,month,age,n_pred,n_birth,n_pop)
  
  #subset
  subset_draw_ids <- sample(seq_len(n_draws), n_draw_subset, replace = FALSE) %>% sort()
  pred_n_birth_draw_df = pred_n_birth_draw_df %>% 
    filter(draw %in% subset_draw_ids)
  
  #birth by reg, year, month, age
  birth_year_month_age_reg_df = birth_df %>% 
    group_by(year, month,age=mother_age,dist_id=mother_dist_id) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  #distribute over region, only for 2011-2024
  pred_n_birth_reg_draw_df = pred_n_birth_draw_df %>% 
    dplyr::rename(n_pred_nat = n_pred) %>% 
    dplyr::select(-c(n_birth,n_pop)) %>% 
    inner_join(pop_dist_df %>% dplyr::rename(n_pop=n), by=c("year","month","age"),relationship = "many-to-many")  %>%
    group_by(draw,year, month, age) %>%
    dplyr::mutate(n_pred = {p <- n_pop / sum(n_pop)
    as.vector(rmultinom(1, size = n_pred_nat[1], prob = p))}) %>%
    ungroup() %>% 
    #add n_birth
    left_join(birth_year_month_age_reg_df,by=c("year","month","age","dist_id"))  %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0))
  
  if(FALSE){
    #check dimension
    dim(pred_n_birth_reg_draw_df)
    144 * 14 * 36 * 12 * 100 #dist year age month draw
    #check that sum of pred is national pred
    pred_n_birth_reg_draw_df %>% 
      group_by(draw,year, month, age) %>%
      dplyr::summarise(n_pred_nat=n_pred_nat[1],
                       n_pred_region=sum(n_pred))
  }
  
  return(list(pred_n_birth_draw_df = pred_n_birth_draw_df,
                     pred_n_birth_reg_draw_df = pred_n_birth_reg_draw_df))
}