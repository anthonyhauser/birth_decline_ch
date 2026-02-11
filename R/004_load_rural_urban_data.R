load_rural_urban_data = function(){
  #https://www.agvchapp.bfs.admin.ch/fr/typologies/query
  urban_rural_df <- read_excel("data/municipality_var_data/urban_municipality_data.xlsx",sheet="Données") %>%
    # use first row as header
    row_to_names(row_number = 1) %>%
    # remove metadata row
    slice(-1) %>%
    # rename columns
    dplyr::rename(mun_id  = `Numéro de la commune`,
                  mun_name      = `Nom de la commune`,
                  canton_id     = `N° du canton`,
                  canton_abbr   = `Canton`,
                  district_id   = `Numéro du district`,
                  district_name = `Nom du district`,
                  urban_rural_type1 = `Typologie urbain-rural`,
                  urban_rural_type2 = `Typologie des communes (9 types)`,
                  urban_rural_type3 = `Typologie des communes (25 types)`) %>%
    # keep only relevant variables
    dplyr::select(mun_id, mun_name, canton_id, canton_abbr,
                  district_id, district_name, urban_rural_type1) %>%
    # clean types
    dplyr::mutate(mun_id = as.integer(mun_id),
                  canton_id = as.integer(canton_id),
                  district_id = as.integer(district_id),
                  urban_rural_type1 = as.factor(urban_rural_type1))
  return(urban_rural_df)
}