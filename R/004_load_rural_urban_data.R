load_rural_urban_data = function(pop_mun_df){
  #population in 2024 by mun_id
  pop_mun_2024_df =pop_mun_df %>% 
    dplyr::filter(year==2024) %>% 
    group_by(mun_id) %>% 
    dplyr::summarise(n_pop = sum(n),.groups="drop")

  #https://www.agvchapp.bfs.admin.ch/fr/typologies/query, Typologie des communes 2020 (25, 9 catégories et typologie urbain-rural)
  urban_rural_df <- read_excel("data/municipality_var_data/urban_municipality_data.xlsx",sheet="Données") %>%
    # use first row as header
    row_to_names(row_number = 1) %>%
    # remove metadata row
    slice(-1) %>%
    # rename columns
    dplyr::rename(mun_id  = `Numéro de la commune`,
                  mun_name      = `Nom de la commune`,
                  ctn_id     = `N° du canton`,
                  ctn_abbr   = `Canton`,
                  dist_id   = `Numéro du district`,
                  dist_name = `Nom du district`,
                  urban_rural_type1 = `Typologie urbain-rural`,
                  urban_rural_type2 = `Typologie des communes (9 types)`,
                  urban_rural_type3 = `Typologie des communes (25 types)`) %>%
    # keep only relevant variables
    dplyr::select(mun_id, mun_name, ctn_id, ctn_abbr,
                  dist_id, dist_name,
                  urban_rural_type1, urban_rural_type2) %>%
    # clean types
    dplyr::mutate(mun_id = as.integer(mun_id),
                  ctn_id = as.integer(ctn_id),
                  dist_id = as.integer(dist_id),
                  urban_rural_type1 = as.numeric(as.character(urban_rural_type1)),
                  urban_rural_type2 = as.numeric(as.character(urban_rural_type2))) %>% 
    # population, used to produce weighted mean by district
    left_join(pop_mun_2024_df, by="mun_id")
  #read_excel("data/municipality_var_data/urban_municipality_data.xlsx",sheet=3) 
  
  return(urban_rural_df)
}

