#draws by year (and not month) for 2011:2024, by ctz region and ctn 
get_pred_birth_draw_by_ctzreg = function(fit, #cmdstanr fit
                                      stan_df,
                                      pop_detctz_df,#pop by district
                                      birth_df, #birth_df (individual level)
                                      n_draw_subset = 100,
                                      save.date,
                                      mod_name,
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
  
  #filter birth if parity is first
  if(filter_parity=="first"){
    birth_df = birth_df %>% 
      filter(year>=2005,!is.na(parity),parity==1)
  }else if(filter_parity=="second"){
    birth_df = birth_df %>% 
      filter(year>=2005,!is.na(parity),parity>1)
  }
  
  ##############################################################################
  #get country name for each ctz_id (as birth_df only report ctz_id)
  find_country_name_in_birth_df = function(birth_df, pop_detctz_df){
    #construct a list of all country ids and names reported in birth data
    world_reg_ctz_df = birth_df %>% 
      filter(year>=2011) %>% #filter already here as country-specific citizenship population data only available from 2011
      dplyr::select(ctz_id = mother_citizenship) %>% distinct() %>% 
      filter(!(ctz_id %in% c(8998,8999))) %>%  #remove apatride (8998) and other (8999)
      left_join(pop_detctz_df %>% dplyr::select(ctz_id,ctz_name) %>% distinct(), by="ctz_id")
    
    if(FALSE){
      #check that all ids are present in pop data: only Tibet missing
      world_reg_ctz_df %>% filter(is.na(ctz_name))
    }
    
    #add Tibet
    world_reg_ctz_df = rbind(world_reg_ctz_df %>% filter(ctz_id!=8543),
                             data.frame(ctz_id = 8543, ctz_name="Tibet"))
    
    return(world_reg_ctz_df)
  }
  world_reg_ctz_df = find_country_name_in_birth_df(birth_df, pop_detctz_df)
  
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
  #draws by year and age (over month)
  pred_n_birth_draw_df = pred_n_birth_draw_df %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year,age,draw) %>% #sum over age and month
    dplyr::summarise(n_pred = sum(n_pred),.groups = "drop")
  print("---")
  #subset
  n_draws = fit$num_chains() * fit$metadata()$iter_sampling
  subset_draw_ids <- sample(seq_len(n_draws), n_draw_subset, replace = FALSE) %>% sort()
  pred_n_birth_draw_df = pred_n_birth_draw_df %>% 
    filter(draw %in% subset_draw_ids)
  print("---")
  
  #births by year and ctz region and ctn----------------------------------------------------
  birth_year_ctn_detctz_df = birth_df %>% 
    filter(mother_citizenship2 %in% filter_ctz) %>% 
    filter(year %in% 2011:2024,
           !(mother_citizenship %in% c(8998, 8999))) %>% 
    dplyr::rename(ctz_id=mother_citizenship) %>% 
    left_join(world_reg_ctz_df,by="ctz_id") %>% 
    left_join(ctz_map,by="ctz_name")
  
  if(FALSE){
    #check that there is no missing ctn_abbr
    birth_year_ctn_detctz_df %>% 
      filter(year %in% 2011:2024) %>% 
      group_by(mother_ctn_abbr) %>% dplyr::summarise(n=n()) %>% print(n=30)
    
    birth_year_ctn_detctz_df %>% filter(is.na(ctz_region))
  }
  
  birth_year_ctn_detctz_df = birth_year_ctn_detctz_df %>% 
    group_by(year, ctn_abbr = mother_ctn_abbr, ctz_region) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  print("---")
  
  #pop--------------------------------------------------------------------------
  pop_detctz_df2 = pop_detctz_df %>%
    dplyr::mutate(citizenship2 = ifelse(ctz_name=="Suisse","swiss","non-swiss")) %>% 
    filter(citizenship2 %in% filter_ctz) %>% 
    left_join(ctz_map,by="ctz_name") 
  
  if(FALSE){
    pop_detctz_df2 %>% filter(is.na(ctz_region))
  }
  
  pop_detctz_df2 = pop_detctz_df2 %>% 
    group_by(year,age,ctn_abbr,ctz_region) %>% 
    dplyr::summarise(n_pop=sum(n),.groups="drop")
  
  print("---")
  
  ##############################################################################
  #Calculate draws of predicted numbers of birth by ctz region, ctn and year
  #distribute over mun_id, only for 2011-2024 (multinomial)
  pred_n_birth_ctz_draw_df = pred_n_birth_draw_df %>% 
    dplyr::rename(n_pred_nat = n_pred) %>% 
    inner_join(pop_detctz_df2, by=c("year","age"),relationship = "many-to-many")  %>%
    group_by(draw,year,age) %>%
    dplyr::mutate(n_pred = {p <- n_pop / sum(n_pop)
    as.vector(rmultinom(1, size = n_pred_nat[1], prob = p))}) %>%
    ungroup() 
  print("---")
  #ctz reg, by ctn and year: sum over age (by year, ctz region, year and draw)---------------
  excess_birth_year_ctz_draw_df = pred_n_birth_ctz_draw_df %>% 
    group_by(year,ctn_abbr,ctz_region,draw) %>% 
    dplyr::summarise(n_pred = sum(n_pred),.groups="drop") %>% 
    left_join(birth_year_ctn_detctz_df,by=c("year","ctn_abbr","ctz_region")) %>% 
    dplyr::mutate(n_birth = replace_na(n_birth,0),
                  n_exc = n_birth - n_pred)
 
  
  #save
  saveRDS(excess_birth_year_ctz_draw_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_ctn_ctzreg_draw_df",".RDS"))
  
  if(FALSE){
    excess_birth_year_ctz_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_ctn_ctzreg_draw_df",".RDS"))
  }
  
  return(list(excess_birth_year_ctz_draw_df = excess_birth_year_ctz_draw_df))
} 
