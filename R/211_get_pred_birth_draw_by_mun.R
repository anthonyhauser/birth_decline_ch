#draws by year (and not month) for 2011:2024, by municipality
get_pred_birth_draw_by_mun = function(fit, #cmdstanr fit
                                      stan_df,
                                      pop_mun_df,#pop by dmunicipality
                                      birth_df, #birth_df (individual level)
                                      new_mun_sf,
                                      n_draw_subset = 100,
                                      save.date,
                                      mod_name,
                                      use.p_childless = FALSE,
                                      seed_id,
                                      res_path = "results/"){
  
  if(FALSE){
    fit = fit5_month
    n_draw_subset = 100
    pop_dist_df
    birth_df
  }
  
  #define ctz population
  ctz_name = case_when(grepl("non-swiss",mod_name) ~ "non-swiss",
                       grepl("swiss",mod_name) ~ "swiss",
                       TRUE ~ "")
  if(ctz_name==""){
    filter_ctz = c("swiss","non-swiss")
  }else{
    filter_ctz = ctz_name
  }
  
  #parity
  filter_parity = case_when(grepl("first",mod_name) ~ "first",
                            grepl("second",mod_name) ~ "second",
                            TRUE ~ "all")
  
  #load probability of childlessness
  p_childless_df =  get_prob_childless_by_mun(birth_df, pop_mun_df, filter_ctz,   p_mun_changes = 0.06)
  if(use.p_childless & filter_parity!="all"){
    mod_name = paste0(mod_name,"_childless") #adapt name to save
    if(filter_parity=="first"){
      p_childless_df$p_susc = p_childless_df$p_childless_pos2
    }else if(filter_parity=="second"){
      p_childless_df$p_susc = 1 - p_childless_df$p_childless_pos2
    }
  }else{
    p_childless_df$p_susc = 1
  }
  print(p_childless_df %>% filter(is.na(p_susc) | (!is.na(p_susc) & (p_susc<=0 | p_susc>=1))))
  
  #filter birth if parity is first
  if(grepl("first",filter_parity)){
    birth_df = birth_df %>% 
      filter(year>=2005,!is.na(parity),parity==1)
  }else if(grepl("second",filter_parity)){
    birth_df = birth_df %>% 
      filter(year>=2005,!is.na(parity),parity>1)
  }
  
  ##############################################################################
  #Load data
  #prediction numbers-----------------------------------------------------------
  print("---")
  #prediction numbers at the national level (i.e., level at which data is fitted)
  pred_n_birth_draw_df = fit$draws("n_birth_pred",format = "df")  %>%
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    pivot_longer( cols = starts_with("n_birth_pred["),names_to = "var",values_to = "n_pred") %>% 
    tidyr::extract(var,into=c("var","data_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(data_id = as.numeric(data_id)) %>% 
    left_join(stan_df %>% dplyr::select(age=mother_age,year,birth_year,month,n_pop,n_birth) %>% 
                dplyr::mutate(data_id = row_number()),by="data_id") %>% 
    dplyr::select(draw,year,birth_year,month,age,n_pred,n_birth,n_pop)
  print("---")
  #draws by year and age
  pred_n_birth_draw_df = pred_n_birth_draw_df %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year,age,draw) %>% #sum over month
    dplyr::summarise(n_pred = sum(n_pred),.groups = "drop")
  print("---")
  #subset
  n_draws = fit$num_chains() * fit$metadata()$iter_sampling
  subset_draw_ids <- sample(seq_len(n_draws), n_draw_subset, replace = FALSE) %>% sort()
  pred_n_birth_draw_df = pred_n_birth_draw_df %>% 
    filter(draw %in% subset_draw_ids)
  print("---")
  #births by year and mun_id----------------------------------------------------
  birth_year_mun_df = birth_df %>% 
    filter(mother_citizenship2 %in% filter_ctz) %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year,mun_id=mother_mun_id) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  print("---")
  #pop--------------------------------------------------------------------------
  pop_mun_df2 = pop_mun_df %>%
    filter(citizenship %in% filter_ctz) %>% 
    filter(month==1) %>% 
    group_by(year,age,mun_id,mun_name) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop") %>% 
    left_join(p_childless_df %>% dplyr::select(age,mun_id,mun_name,p_susc),by=c("age","mun_id","mun_name")) %>% 
    dplyr::mutate(n_pop=n_pop*p_susc) %>% dplyr::select(-p_susc)
  print(p_childless_df %>% filter(is.na(p_susc)))
  print("---trying---")
  #calculate adjacent municipalities--------------------------------------------
  nb <- poly2nb(new_mun_sf, queen = TRUE) #create neighbor list (Queen contiguity)
  print("---working---")
  adj_table <- tibble(mun_id = rep(new_mun_sf$mun_id, lengths(nb)),
                      adj_mun_id = unlist(lapply(nb, function(x) new_mun_sf$mun_id[x]))) %>% 
    rbind(tibble(mun_id = new_mun_sf$mun_id,
                 adj_mun_id = new_mun_sf$mun_id)) %>% 
    arrange(mun_id,adj_mun_id) %>% 
    #add weights
    group_by(mun_id) %>% 
    dplyr::mutate(w = if_else(adj_mun_id==mun_id,0.5,0.5/(n()-1))) %>% #w=0.5 for the mun_id itself and 0.5 shared over the neighbours
    ungroup()
  print("---")
  ##############################################################################
  #Calculate draws of predicted numbers of birth by mun_id and year
  #distribute over mun_id, only for 2011-2024 (multinomial)
  pred_n_birth_mun_draw_df = pred_n_birth_draw_df %>% 
    dplyr::rename(n_pred_nat = n_pred) %>% 
    inner_join(pop_mun_df2, by=c("year","age"),relationship = "many-to-many")  %>%
    group_by(draw,year,age) %>%
    dplyr::mutate(n_pred = {p <- n_pop / sum(n_pop)
    as.vector(rmultinom(1, size = n_pred_nat[1], prob = p))}) %>%
    ungroup() 
  print("---")
  #municipality, by year: sum over age (by year, mun_id and draw)---------------
  excess_birth_year_mun_draw_df = pred_n_birth_mun_draw_df %>% 
    group_by(year,mun_id,mun_name,draw) %>% 
    dplyr::summarise(n_pred = sum(n_pred),.groups="drop") %>% 
    left_join(birth_year_mun_df,by=c("year","mun_id")) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  n_exc = n_birth - n_pred)
  
  #adjacent municipality, by year-----------------------------------------------
  df = adj_table %>% 
    #add mun_name
    left_join(excess_birth_year_mun_draw_df %>% dplyr::select(mun_id,mun_name) %>% distinct(),
              by="mun_id") %>% 
    #add n_birth, n_pred and n_exc
    left_join(excess_birth_year_mun_draw_df %>% dplyr::select(adj_mun_id=mun_id,year,draw,
                                                              adj_n_pred = n_pred, adj_n_exc = n_exc, adj_n_birth = n_birth),
              by="adj_mun_id", relationship = "many-to-many")
  
  #sum of expected and excess births over adjacent municipalities: equal weights
  excess_birth_year_adj_mun_draw_df = df %>% 
    dplyr::mutate(w=1) %>% 
    group_by(year,mun_id,mun_name,draw) %>% 
    dplyr::summarise(n_birth = sum(adj_n_birth*w)/sum(w),
                     n_pred = sum(adj_n_pred*w)/sum(w),
                     n_exc = sum(adj_n_exc*w)/sum(w),.groups="drop_last")
  
  #sum of expected and excess births over adjacent municipalities: weight is 50% for municipality, 50% for neighbours
  excess_birth_year_adj2_mun_draw_df = df %>% 
    group_by(year,mun_id,mun_name,draw) %>% 
    dplyr::summarise(n_birth = sum(adj_n_birth*w)/sum(w),
                     n_pred = sum(adj_n_pred*w)/sum(w),
                     n_exc = sum(adj_n_exc*w)/sum(w),.groups="drop_last")
  
  #save
  saveRDS(excess_birth_year_mun_draw_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS"))
  saveRDS(excess_birth_year_adj_mun_draw_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj_mun_draw_df",".RDS"))
  saveRDS(excess_birth_year_adj2_mun_draw_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj2_mun_draw_df",".RDS"))
  
  if(FALSE){
    excess_birth_year_mun_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS"))
    excess_birth_year_adj_mun_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj_mun_draw_df",".RDS"))
    excess_birth_year_adj2_mun_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj2_mun_draw_df",".RDS"))
  }
  
  return(list(excess_birth_year_mun_draw_df = excess_birth_year_mun_draw_df,
              excess_birth_year_adj_mun_draw_df = excess_birth_year_adj_mun_draw_df,
              excess_birth_year_adj2_mun_draw_df = excess_birth_year_adj2_mun_draw_df))
} 
