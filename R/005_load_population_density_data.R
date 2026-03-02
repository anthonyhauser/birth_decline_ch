load_population_density_data = function(){
  #https://www.bfs.admin.ch/bfs/fr/home/statistiques/catalogues-banques-donnees.assetdetail.36074341.html
  pop_dens_df <- read_excel(paste0(code_root_path,"data/municipality_var_data/population_density_data.xlsx"),sheet=1,skip = 2) %>%
    # use first row as header
    row_to_names(row_number = 1) %>%
    # rename columns
    dplyr::rename(hist_mun_id  = `Code`,
                  hist_mun_name = `Libellé`,
                  pop_dens = `Densité de la population (surface productive) 2024`) %>%
    # clean types
    dplyr::mutate(hist_mun_id = as.integer(hist_mun_id),
                  pop_dens = as.numeric(pop_dens)) %>% 
    #add current mun_id 
    left_join(mun_df %>% dplyr::select(mun_id,mun_name,hist_mun_id), by="hist_mun_id")
    arrange(mun_id)
  
  if(FALSE){
    #missing mun_id
    pop_dens_df %>% filter(is.na(mun_id))
    #different historical  and current municipality names
    pop_dens_df %>% filter(hist_mun_name!=mun_name)
    #different historical  and current mun_id
    pop_dens_df %>% filter(hist_mun_id!=mun_id)
    #multiple hist_mun_id for the same mun_id
    pop_dens_df %>% 
      group_by(mun_id) %>% 
      dplyr::mutate(n=n()) %>% ungroup() %>% filter(n>1) %>% arrange(mun_id) %>% View()
  }
    
  #summarise by mun_id (as some mun_id have multiple rows)
  #use max because historical municipality with high density is often the main place and therefore is representative of the density of the current mun_id
  pop_dens_df = pop_dens_df %>% 
    group_by(mun_id,mun_name) %>% 
    dplyr::summarise(pop_dens = max(pop_dens),.groups="drop")
  
  return(pop_dens_df)
}

