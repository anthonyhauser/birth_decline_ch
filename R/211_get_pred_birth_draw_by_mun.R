#draws by year (and not month) for 2011:2024, by municipality
get_pred_birth_draw_by_mun = function(fit, #cmdstanr fit
                                                           stan_df,
                                                           pop_mun_df,#pop by district
                                                           birth_df, #birth_df (individual level)
                                                           n_draw_subset = 1000,
                                                           save.date,
                                                           mod_name,
                                                           seed_id){
  
  if(FALSE){
    fit = fit5_month
    n_draw_subset = 100
    pop_dist_df
    birth_df
  }
  
  ##############################################################################
  #Load data
  #prediction numbers-----------------------------------------------------------
  #number of draws
  n_draws = fit$num_chains() * fit$metadata()$iter_sampling
  
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
  
  #draws by year
  pred_n_birth_draw_df = pred_n_birth_draw_df %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year,age,draw) %>% #sum over age and month
    dplyr::summarise(n_pred = sum(n_pred),.groups = "drop")
  
  #subset
  subset_draw_ids <- sample(seq_len(n_draws), n_draw_subset, replace = FALSE) %>% sort()
  pred_n_birth_draw_df = pred_n_birth_draw_df %>% 
    filter(draw %in% subset_draw_ids)
  
  #births by year and mun_id----------------------------------------------------
  birth_year_mun_df = birth_df %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year,mun_id=mother_mun_id) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  #pop--------------------------------------------------------------------------
  pop_mun_df = pop_mun_df %>%
    filter(month==1) %>% 
    group_by(year,age,mun_id,mun_name) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  
  
  #calculate adjacent municipalities--------------------------------------------
  mun_sf <- st_read(paste0(code_root_path,"data/boundary_data/Boundaries_K4_Commune_20260101.shp")) #source: https://www.agvchapp.bfs.admin.ch/fr/boundaries?SnapshotDate=01.01.2018&Unit=BAE2018
  mun_sf = mun_sf[,c(1,3,7)]
  names(mun_sf)[c(1,2)] <- c("mun_name2", "mun_id")
  
  nb <- poly2nb(mun_sf, queen = TRUE) #create neighbor list (Queen contiguity)
  adj_table <- tibble(mun_id = rep(mun_sf$mun_id, lengths(nb)),
                      adj_mun_id = unlist(lapply(nb, function(x) mun_sf$mun_id[x]))) %>% 
    rbind(tibble(mun_id = mun_sf$mun_id,
                 adj_mun_id = mun_sf$mun_id)) %>% 
    arrange(mun_id,adj_mun_id)
  
  ##############################################################################
  #Calculate draws of predicted numbers of birth by mun_id and year
  #distribute over mun_id, only for 2011-2024 (multinomial)
  pred_n_birth_mun_draw_df = pred_n_birth_draw_df %>% 
    dplyr::rename(n_pred_nat = n_pred) %>% 
    inner_join(pop_mun_df, by=c("year","age"),relationship = "many-to-many")  %>%
    group_by(draw,year,age) %>%
    dplyr::mutate(n_pred = {p <- n_pop / sum(n_pop)
    as.vector(rmultinom(1, size = n_pred_nat[1], prob = p))}) %>%
    ungroup() 
  
  #municipality,by year: sum over age (by year, mun_id and draw)
  excess_birth_year_mun_draw_df = pred_n_birth_mun_draw_df %>% 
    group_by(year,mun_id,mun_name,draw) %>% 
    dplyr::summarise(n_pred = sum(n_pred),.groups="drop") %>% 
    left_join(birth_year_mun_df,by=c("year","mun_id")) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  n_exc = n_birth - n_pred)
  
  #adjacent, by year
  excess_birth_year_adj_mun_draw_df = adj_table %>% 
    #add mun_name
    left_join(excess_birth_year_mun_draw_df %>% dplyr::select(mun_id,mun_name) %>% distinct(),
              by="mun_id") %>% 
    #add n_pred and n_exc
    left_join(excess_birth_year_mun_draw_df %>% dplyr::select(adj_mun_id=mun_id,year,draw,adj_n_pred = n_pred, adj_n_exc = n_exc, adj_n_birth = n_birth),
              by="adj_mun_id", relationship = "many-to-many") %>%
    #sum of expected and excess births over adjacent municipalities
    group_by(year,mun_id,mun_name,draw) %>% 
    dplyr::summarise(n_pred = sum(adj_n_pred),
                     n_exc = sum(adj_n_exc),.groups="drop_last")
  
  #save
  saveRDS(excess_birth_year_mun_draw_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS"))
  saveRDS(excess_birth_year_adj_mun_draw_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_adj_mun_draw_df",".RDS"))
  
  return(list(excess_birth_year_mun_draw_df = excess_birth_year_mun_draw_df,
              excess_birth_year_adj_mun_draw_df = excess_birth_year_adj_mun_draw_df))
} 
