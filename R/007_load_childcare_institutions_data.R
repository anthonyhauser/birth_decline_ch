
load_childcare_institutions_data = function(pop_mun_df, mun_df){
  #population in 2024 by mun_id
  pop_mun_2024_df = pop_mun_df %>% 
    dplyr::filter(year==2024,month==1) %>% 
    group_by(mun_id,mun_name) %>% 
    dplyr::summarise(n_pop = sum(n),.groups="drop")
  
  childcare_institutions_df <- read_excel(paste0(code_root_path,"data/municipality_var_data/childcare_institutions.xlsx"),sheet=1,skip = 5) %>% 
    # rename columns
    dplyr::rename(year = `Referenzjahr STATENT`,
                  hist_mun_id_name = `Code + Gemeinde.`,
                  type = `5 NOGA 2008 - Code + Genre`,
                  n_inst = Arbeitsstätte,
                  n_empl = Beschäftigte) %>% 
    separate(hist_mun_id_name,
             into = c("hist_mun_id", "hist_mun_name"),
             sep = " ",
             extra = "merge") %>% 
    dplyr::mutate(hist_mun_id = as.integer(hist_mun_id),
                  n_empl_mean = as.numeric(ifelse(n_empl=="<4",1.5,n_empl)),
                  n_empl_na = as.numeric(ifelse(n_empl=="<4",NA,n_empl))) %>% 
    #add current mun_id 
    left_join(mun_df %>% dplyr::select(mun_id,mun_name,hist_mun_id), by="hist_mun_id") %>% 
    arrange(mun_id)
  
  if(FALSE){
    #check that there is only one row per historical municipality per year
    childcare_institutions_df %>% 
      group_by(hist_mun_id,hist_mun_name,year) %>% 
      dplyr::mutate(n=n()) %>% filter(n>1)
    
    #some municipalities are linked with multiple historical municipalities
    childcare_institutions_df %>% 
      group_by(mun_id,mun_name,year) %>% 
      dplyr::mutate(n=n()) %>% filter(n>1)
    
    #the number of years by municipality varies 
    childcare_institutions_df %>% 
      group_by(mun_id) %>% 
      dplyr::summarise(n_year = length(unique(year)),.groups="drop") %>% 
      group_by(n_year) %>% 
      dplyr::summarise(n=n(),.groups="drop")
  }
  
  
  #aggregate the number of employment over mun_id and average over year
  childcare_institutions_df = childcare_institutions_df %>% 
    #group over historical municipalities with same mun_id
    group_by(year,mun_id, mun_name) %>% 
    dplyr::summarise(n_empl_mean = sum(n_empl_mean),
                     n_empl_na = sum(n_empl_na),.groups="drop") %>% 
    #take the mean over years
    group_by(mun_id,mun_name) %>% 
    dplyr::summarise(n_empl_mean = mean(n_empl_mean),
                     n_empl_na = mean(n_empl_na),.groups="drop")
  
  
  
  
  
  #add missing mun_id (missing because there are 0 institutions)
  childcare_institutions_df = pop_mun_2024_df %>% 
    left_join(childcare_institutions_df %>% dplyr::select(-mun_name),by="mun_id") %>% 
    dplyr::mutate(n_empl_mean = if_else(is.na(n_empl_mean),0,n_empl_mean )) %>% 
    dplyr::mutate(rel_empl_mean = n_empl_mean/n_pop) %>% 
    arrange(mun_id) 
  
  #average over adjacent municipalities
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
  
  childcare_institutions_adj_df = adj_table %>% 
    #add mun_name
    left_join(childcare_institutions_df %>% dplyr::select(mun_id,mun_name) %>% distinct(),
              by="mun_id") %>% 
    #add n_birth, n_pred and n_exc
    left_join(childcare_institutions_df %>% dplyr::select(adj_mun_id=mun_id,n_pop,
                                                          adj_n_pop = n_pop, adj_n_empl_mean  = n_empl_mean),
              by="adj_mun_id", relationship = "many-to-many") %>% 
    #aggregate over mun_id
    group_by(mun_id,mun_name) %>% 
    dplyr::summarise(n_empl_mean = sum(w * adj_n_empl_mean),
                     n_pop = sum(w * adj_n_pop), .groups="drop") %>% 
    dplyr::mutate(rel_empl_mean = n_empl_mean/n_pop) %>% 
    arrange(mun_id)
  
  if(FALSE){
    df = childcare_institutions_adj_df %>% 
      left_join(new_mun_df %>% dplyr::select(mun_id, dist_id) %>% distinct(), by = "mun_id") %>% 
      left_join(new_mun_sf %>% mutate(mun_id = as.numeric(mun_id)), by = "mun_id")
    lims <- range(df$rel_empl_mean, na.rm = TRUE)
    mid  <- df %>% dplyr::summarise(mean = sum(n_empl_mean)/sum(n_pop)) %>% pull(mean)
    
    df %>% 
      st_as_sf() %>%
      ggplot() +
      geom_sf(aes(fill = rel_empl_mean), color = NA) +
      geom_sf(data = regions_sf %>% mutate(dist_id = as.numeric(dist_id)), 
              fill = NA, color = "black", size = 0.3) +
      geom_sf(data = lake_sf, fill = "lightblue", color = NA, alpha = 0.5) +
      scale_fill_gradient2(
        name = "Relative excess birth",
        low = "red",
        mid = "lightyellow",
        high = "green",
        midpoint = mid,
        limits = lims,
        labels = scales::percent_format(accuracy = 1)
      ) +
      theme(legend.position = "bottom",
            legend.direction = "horizontal")
  }
  
  return(list(childcare_institutions_df = childcare_institutions_df,
              childcare_institutions_adj_df = childcare_institutions_adj_df))
}





