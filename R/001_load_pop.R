load_pop_year_age_nat_ctn = function(){
  if(file.exists("data/population_data/pop_year_age_nat_ctn.RDS")) {
    pop_df = readRDS("data/population_data/pop_year_age_nat_ctn.RDS")
   }else {
    pop_df = read_excel(paste0(data_folder, "population_data/pop_year_age_nat_ctn.xlsx"))
    
    ctn_map <- tibble::tribble( ~ctn_abbr, ~region,
                                "CH",      "Suisse",
                                "ZH",      "Zürich",
                                "BE",      "Bern / Berne",
                                "LU",      "Luzern",
                                "UR",      "Uri",
                                "SZ",      "Schwyz",
                                "OW",      "Obwalden",
                                "NW",      "Nidwalden",
                                "GL",      "Glarus",
                                "ZG",      "Zug",
                                "FR",      "Fribourg / Freiburg",
                                "SO",      "Solothurn",
                                "BS",      "Basel-Stadt",
                                "BL",      "Basel-Landschaft",
                                "SH",      "Schaffhausen",
                                "AR",      "Appenzell Ausserrhoden",
                                "AI",      "Appenzell Innerrhoden",
                                "SG",      "St. Gallen",
                                "GR",      "Graubünden / Grigioni / Grischun",
                                "AG",      "Aargau",
                                "TG",      "Thurgau",
                                "TI",      "Ticino",
                                "VD",      "Vaud",
                                "VS",      "Valais / Wallis",
                                "NE",      "Neuchâtel",
                                "GE",      "Genève",
                                "JU",      "Jura",
                                NA,        "Sans indication")
    
    pop_df = pop_df[3:325586,-1] %>% as_tibble() %>% 
      rename(
        year = 1,
        region = 2,
        citizenship = 3,
        sex = 4,
        age = 5,
        n_start = 6,
        n_end = 7 ) %>%
      #fill in empty parts
      fill(year,  region,citizenship, sex, age, .direction = "down") %>% 
      #filter
      filter(sex=="Femme",
             citizenship!="Nationalité (catégorie) - total",
             region!="Suisse",
             age!="Âge - total") %>% 
      #clean column
      left_join(ctn_map, by="region") %>% 
      dplyr::mutate(year = as.numeric(year),
                    citizenship = case_when(citizenship == "Suisse"   ~ 1,
                                            citizenship == "Étranger" ~ 0,
                                            TRUE                      ~ NA_real_),
                    age = case_when( str_detect(age, "^\\d+\\s+an") ~ as.numeric(str_extract(age, "^\\d+")),
                                     age == "99 ans ou plus"        ~ 99,
                                     TRUE                           ~ NA_real_ ),
                    n_start = as.numeric(n_start),
                    n_end = as.numeric(n_end)) %>% 
      #filter age
      filter(age %in% 15:50) %>% 
      #add month
      expand_grid(month=1:12) %>% 
      dplyr::mutate(n = n_start + (n_end-n_start) * (month-1)) %>% 
      dplyr::select(year,month,ctn_abbr,citizenship,age,n) %>% 
      arrange(year,month,ctn_abbr,citizenship,age)
    
    saveRDS(pop_df,"data/population_data/pop_year_age_nat_ctn.RDS")
  }
  
  return(pop_df)
}



 


