# save.date
# mod_name
# seed_id = 1
# pop_mun_df = new_pop_mun_df
# rural_urban_df
# pop_dens_df
# sep_df3
# childcare_institutions_df

excess_by_ntiles = function(save.date, mod_name, seed_id,
                            use.p_childless_v,
                            pop_mun_df, rural_urban_df, pop_dens_df, sep_df3, childcare_institutions_df, vote_mun_df,
                            year_range = 2017:2024,
                            res_path = "results/"){
  
  #load data
  excess_birth_year_mun_draw_df = list()
  for(w in use.p_childless_v){
    excess_birth_year_mun_draw_df[[as.character(w)]] = readRDS(paste0(code_root_path, res_path,save.date,"_",mod_name,"_childless"[w],"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS")) %>% 
                                          dplyr::mutate(childless = w)
  }
  excess_birth_year_mun_draw_df = rbindlist(excess_birth_year_mun_draw_df)
  
  #Variables
  df_var0 = excess_birth_year_mun_draw_df %>% dplyr::select(mun_id,mun_name) %>% distinct() %>% 
    #add population of 2024
    left_join(pop_mun_df %>% filter(year==2024,month==1) %>% 
                group_by(mun_id) %>% dplyr::summarise(n_pop = sum(n),.groups="drop"), by=c("mun_id")) %>% 
    #add rural urban (9 categories)
    left_join(rural_urban_df %>% dplyr::select(mun_id,urban_rural_type2),by="mun_id") %>% 
    #Population density
    left_join(pop_dens_df %>% pivot_wider(id_cols = mun_id,names_from = "variable",values_from = "value"),by="mun_id") %>% 
    #SEP
    left_join(sep_df3 %>% dplyr::select(mun_id,sep=ssep3_d_mean),by="mun_id") %>% 
    #childcare
    left_join(childcare_institutions_df %>% dplyr::select(mun_id,rel_empl_mean),by="mun_id") %>% 
    #votation
    left_join(vote_mun_df %>% 
                dplyr::mutate(vote_object = paste0("votation_",vote_object)) %>% 
                group_by(mun_id, vote_object) %>% 
                dplyr::summarise(yes_prop = sum(yes_prop *n_pop)/sum(n_pop),.groups="drop") %>% 
                pivot_wider(id_cols=mun_id ,names_from=vote_object, values_from = yes_prop),by="mun_id")
  
  if(FALSE){
    #Rows with any NA: for pop_dens_building, prop_building, prop_infrastructure and SEP (28 mun_id, can be considered as negligible)
    df_var0 %>%
      filter(if_any(everything(), is.na))
  }
  
  #function splitting municipalities into n-tiles
  make_pop_ntile <- function(data, var, n = 5) {
    total_pop = sum(data$n_pop)
    data %>%
      arrange({{ var }}) %>%
      dplyr::mutate(cum_pop = cumsum(n_pop),
                    group = cut(cum_pop,breaks = seq(0, total_pop, length.out = n+1),labels = FALSE,  include.lowest = TRUE )) %>% 
      dplyr::select(-cum_pop) %>% 
      arrange(mun_id) %>%
      pull(group)
  }
  
  vars = setdiff(names(df_var0), c("urban_rural_type2", "mun_id", "mun_name")) # c("n_pop", "pop_dens_building")
  
  #split in 10 categories
  df_var = df_var0
  for (v in vars) {
    df_var[[paste0(v, "_ntile")]] <- make_pop_ntile(df_var, !!rlang::sym(v), 10)
  }
  #for urban_rural already split in 9 categories
  df_var$urban_rural_type2_ntile = dense_rank(df_var$urban_rural_type2)
  vars = c(vars, "urban_rural_type2")
  
  if(FALSE){
    #check that the number of pop is more or less the same between groups (except for urban_rural_type2)
    df_var %>% group_by(n_pop_ntile) %>% 
      dplyr::summarise(n_pop = sum(n_pop),
                       n=n())
    #some are correlation: population size and prop of childcare employment by women of childbearing age
    df_var %>%
      group_by(n_pop_ntile) %>%
      summarise(rel_mean = mean(rel_empl_mean_ntile, na.rm = TRUE)) %>%
      ggplot(aes(x = n_pop_ntile, y = rel_mean)) +
      geom_point() +
      geom_line()
    df_var %>%
      count(n_pop_ntile, rel_empl_mean_ntile) %>%
      group_by(n_pop_ntile) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(x = n_pop_ntile, y = rel_empl_mean_ntile, fill = prop)) +
      geom_tile()
    df_var %>%
      group_by(n_pop_ntile) %>%
      summarise(rel_mean = mean(votation_paternity_leave_ntile, na.rm = TRUE)) %>%
      ggplot(aes(x = n_pop_ntile, y = rel_mean)) +
      geom_point() +
      geom_line()
    df_var %>%
      count(n_pop_ntile, votation_paternity_leave_ntile) %>%
      group_by(n_pop_ntile) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(x = n_pop_ntile, y = votation_paternity_leave_ntile, fill = prop)) +
      geom_tile()
      
  }
  
  #aggregate excess by group
  excess_df = list()
  excess_year_df = list()
  for(v in paste0(vars, "_ntile")){
    print(v)
    for(w in use.p_childless_v){
      print(w)
      excess_df[[paste0(v,"_",w)]] = get_excess_est(var_group = "ntile",
                                                    excess_birth_year_mun_draw_df %>% 
                                                      filter(year %in% year_range,
                                                             childless==w) %>% 
                                                      left_join(df_var %>% dplyr::select(mun_id,ntile = !!v),by="mun_id")) %>% 
        dplyr::mutate(explanatory_var=!!v,
                      childless = !!w)
      
      excess_year_df[[paste0(v,"_",w)]] = get_excess_est(var_group = c("year","ntile"),
                                                    excess_birth_year_mun_draw_df %>% 
                                                      filter(year %in% year_range,
                                                             childless==w) %>% 
                                                      left_join(df_var %>% dplyr::select(mun_id,ntile = !!v),by="mun_id")) %>% 
        dplyr::mutate(explanatory_var=!!v,
                      childless = !!w)
    }
  }
  excess_df = rbindlist(excess_df)
  excess_year_df = rbindlist(excess_year_df)
  
  if(FALSE){
    #plot
    excess_df %>% 
      #filter(explanatory_var %in% c("n_pop_ntile","pop_dens_building_ntile", "prop_building_ntile", "prop_individual_houses_ntile","sep_ntile","urban_rural_type2_ntile")) %>% 
      ggplot(aes(x = ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
      geom_ribbon(aes(fill=childless),alpha = 0.2) +
      geom_line(aes(col=childless)) +
      geom_point(aes(col=childless)) +
      geom_hline(aes(yintercept = 0), linetype = 2)+
      scale_y_continuous(name="Relative excess birth",labels = scales::percent)+
      facet_wrap(explanatory_var~.,ncol=2)
  }
  
  #check for an individual variable
  if(FALSE){
    var="rel_empl_mean"
    df_var = df_var0 %>% 
      filter(n_pop>100,n_pop<10000)
    
    sum(df_var$n_pop)/sum(df_var0$n_pop)
    length(df_var$n_pop)/length(df_var0$n_pop)
    excess_df = excess_birth_year_mun_draw_df %>% 
      filter(mun_id %in% df_var$mun_id)
    
    df_var$var_ntile = make_pop_ntile(df_var,  !!rlang::sym(var), 10)
    
    get_excess_est(var_group=c("var_ntile"),
                   excess_df %>% 
                     filter(year %in% 2017:2024) %>% 
                     left_join(df_var %>% dplyr::select(mun_id,var_ntile),by="mun_id")) %>% 
      ggplot(aes(x = var_ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
      #ggplot(aes(x = var_ntile, y = n_exc_mean, ymin = n_exc_lwb, ymax = n_exc_upb))+
      geom_ribbon(aes(),alpha = 0.2) +
      geom_line() +
      geom_point() +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      scale_y_continuous(name="Relative excess birth",labels = scales::percent)
  }
  
  year_suffix = paste0("_",min(year_range),"_",max(year_range))
  saveRDS(excess_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,year_suffix,"_","excess_birth_ntiles_df",".RDS"))
  saveRDS(excess_year_df, paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,year_suffix,"_","excess_birth_ntiles_year_df",".RDS"))
  
  return(excess_df)
}

