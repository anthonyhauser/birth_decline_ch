load_population_density_data = function(mun_df){
  #https://www.bfs.admin.ch/bfs/fr/home/statistiques/catalogues-banques-donnees.assetdetail.36074341.html
  pop_dens_df <- read_excel(paste0(code_root_path,"data/municipality_var_data/population_density_data2.xlsx"),sheet=1,skip = 2) %>%
    # use first row as header
    row_to_names(row_number = 1) %>%
    # rename columns
    dplyr::rename(hist_mun_id  = `Code`,
                  hist_mun_name = `Libellé`,
                  pop_dens = `Densité de la population (surface productive) 2024`,
                  pop_dens_tot = `Densité de la population (surface totale) 2024`,
                  prop_infrastructure = `Habitat et infrastructure 2013/18`, #Part des surfaces d'habitat et d'infrastructure dans la superficie totale (%)
                  prop_building = `Aires de bâtiments 2013/18`, #Part des aires de bâtiments dans la superficie totale 
                  prop_individual_houses = `Part des maisons individuelles 2024`) %>%
    # clean types
    dplyr::mutate(hist_mun_id = as.integer(hist_mun_id)) %>% 
    dplyr::mutate(across( c(pop_dens, pop_dens_tot, prop_infrastructure, prop_building, prop_individual_houses), as.numeric)) %>% 
    dplyr::mutate(pop_dens_building = pop_dens_tot/prop_building) %>% 
    #add current mun_id 
    left_join(mun_df %>% dplyr::select(mun_id,mun_name,hist_mun_id), by="hist_mun_id") %>% 
    dplyr::select(mun_id,mun_name,hist_mun_id,pop_dens,pop_dens_tot,pop_dens_building,prop_infrastructure,prop_building,prop_individual_houses) %>% 
    arrange(mun_id)
  
  if(FALSE){
    #missing any column
    pop_dens_df %>% 
      filter(if_any(everything(), is.na)) %>% print(n=30)
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
    pivot_longer( cols = c(pop_dens, pop_dens_tot, pop_dens_building, prop_infrastructure, prop_building, prop_individual_houses),
      names_to = "variable",values_to = "value") %>% 
    group_by(mun_id,mun_name,variable) %>% 
    dplyr::summarise(value = max(value,na.rm = TRUE),.groups="drop") %>% 
    mutate(value = ifelse(is.infinite(value), NA, value))
  
  if(FALSE){
    #missing any column
    pop_dens_df %>% 
      filter(if_any(everything(), is.na)) %>% print(n=30)
  }
  
  if(FALSE){
    mun_sf <- st_read(paste0(code_root_path,"data/boundary_data/Boundaries_K4_Commune_20260101.shp")) #source: https://www.agvchapp.bfs.admin.ch/fr/boundaries?SnapshotDate=01.01.2018&Unit=BAE2018
    mun_sf = mun_sf[,c(1,3,7)]
    names(mun_sf)[c(1,2)] <- c("mun_name2", "mun_id")
    
    plots = pop_dens_df %>% 
      left_join(mun_sf %>% mutate(dist_id = as.numeric(mun_id)), by = "mun_id") %>% 
      st_as_sf() %>% 
      split(.$variable) %>% 
      lapply(function(df) {
        vals = df$value
        ggplot(df, aes(fill = value)) +
          geom_sf(color = "black", size = 0.01) +
          scale_fill_gradientn(
            colors = c("green", "yellow", "red"),
            values = scales::rescale(c(min(vals, na.rm = TRUE), median(vals, na.rm = TRUE), max(vals, na.rm = TRUE))),
            limits = range(vals, na.rm = TRUE)
          ) +
          theme(legend.position = "bottom", legend.direction = "horizontal") +
          ggtitle(unique(df$variable))
      })
    
    wrap_plots(plots, ncol = 2)
    
    pop_dens_df %>% filter(variable=="pop_dens_building") %>% arrange(-value) %>% View()
  }
  
  return(pop_dens_df)
}

