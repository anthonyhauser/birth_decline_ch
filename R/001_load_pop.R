load_pop_year_age_nat_ctn = function(){
  if(file.exists("data/population_data/pop_year_age_nat_ctn.RDS")) {
    pop_df = readRDS("data/population_data/pop_year_age_nat_ctn.RDS")
   }else {
    pop_df = read_excel(paste0(data_folder, "population_data/pop_year_age_nat_ctn.xlsx"))
    
    pop_df0 = pop_df[3:325586,-1] %>% as_tibble() %>% 
      rename( year = 1,
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
             age!="Âge - total",
             region!="Sans indication") %>% #we can remove them as the number of people in this category is 0
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
      dplyr::select(-sex)
    
   
   pop_df =  rbind(
                   #1987-2024
                   pop_df0 %>% 
                      filter(age %in% 15:50) %>% 
                      dplyr::select(year,ctn_abbr,citizenship,age,n=n_start),
                   #add 2025 to extrapolate pop during 2024 months
                    pop_df0 %>% 
                      filter(age %in% 14:49,year==2024) %>% 
                      dplyr::mutate(age=age+1,
                                    year=year+1) %>% 
                      dplyr::select(year,ctn_abbr,citizenship,age,n=n_end)) %>% 
                expand_grid(month=1:12) %>% 
                arrange(ctn_abbr,citizenship,age,year,month) %>%
                # create a continuous time index and n_year only for month 1
                dplyr::mutate(t = year + (month - 1) / 12,
                              n_year = if_else(month == 1, n, NA_real_)) %>%
                # linear interpolation (uses next year's value)
                group_by(ctn_abbr, citizenship, age) %>%
                dplyr::mutate(n = approx(t[!is.na(n_year)],
                              n_year[!is.na(n_year)],
                              xout = t,
                              rule = 2)$y) %>%
                ungroup() %>% 
               dplyr::select(year,month,ctn_abbr,citizenship,age,n)
    
    saveRDS(pop_df,"data/population_data/pop_year_age_nat_ctn.RDS")
  }
  
  return(pop_df)
}
