load_pop_year_age_mun_ctz = function(){
  #from https://www.pxweb.bfs.admin.ch/pxweb/fr/
  file_names = c("pop_mun_age_14.xlsx",#added because the numbers correspond to the population at 31 Dec
                 "pop_mun_age_15_35_permanent.xlsx",
                 #"pop_mun_age_15_35_nonpermanent.xlsx",
                 "pop_mun_age_36_50_permanent.xlsx")
                 #"pop_mun_age_36_50_nonpermanent.xlsx")
  
  # Prepend folder path
  files <- file.path("data/population_data", file_names)
  
  
  data_list <- list()
  for (i in seq_along(files)) {
    tmp <- read_excel(files[i], skip = 2)
    colnames(tmp)[1:10] = c("year1","year2","reg_id","reg_name","unknown_var1","resident",
                            "unknown_var2","citizenship","unknown_var3","sex")
    tmp = tmp %>% 
      pivot_longer(cols = ends_with("ans"),names_to = "age",values_to = "n") %>%
      dplyr::select(year=year1,reg_id,reg_name,resident,citizenship,age,n) %>% 
      #fill in empty parts
      fill(year,reg_id,reg_name,resident, .direction = "down") %>% 
      filter(reg_name!="Sans indication",citizenship!="Nationalité (catégorie) - total",resident=="Population résidante permanente")
    if(FALSE){
      #check no missing
      tmp %>% dplyr::mutate(row_id = row_number()) %>% filter(if_any(everything(), is.na))
    }
    
    data_list[[i]] = tmp %>% 
      #the number corresponds the population at the 31 Dec of the year for a given age, we thus need to add 1 to the year and to the age to get population of the next age for age +1
      dplyr::mutate(year = as.numeric(year)+1,
                    reg_agg_level = case_when(
                      reg_name == "Suisse"           ~ "national",
                      str_starts(reg_name, "- ")      ~ "canton",
                      str_starts(reg_name, ">> ")     ~ "district",
                      str_starts(reg_name, "......")  ~ "municipality",
                      TRUE                            ~ NA),
                    reg_name = reg_name %>%
                      str_remove("^(- |>> |\\.\\.\\.\\.+)") %>%
                      str_remove("^\\d+\\s+"),
                    # resident = case_when(resident=="Population résidante permanente" ~ "permanent",
                    #                      resident=="Population résidante non permanente" ~ "non-permanent",
                    #                      TRUE ~ NA),
                    citizenship = case_when(citizenship=="Suisse" ~ "swiss",#citizenship=="Nationalité (catégorie) - total" ~ "total",
                                            citizenship=="Étranger" ~ "non-swiss",
                                            TRUE ~ NA),
                    age = as.numeric(readr::parse_number(age))+1) %>% 
      filter(age %in% 15:50) %>%  #discard age 51 that was wrongly included before we knew that population was at 31 Dec
      dplyr::select(-resident) #unselect as only take resident
  }
  pop_df = rbindlist(data_list)
  
  if(FALSE){
    #check that it is the same as population by year age and ctn
    load_pop_year_age_ctn_ctz() %>% 
      filter(year %in% c(2011,2024,2025),month==1,age==20) %>% 
      group_by(year) %>% dplyr::summarise(n=sum(n))
    pop_df %>% 
      filter(year %in% c(2011,2024,2025),age==20) %>% 
      group_by(reg_agg_level,year) %>% 
      dplyr::summarise(n=sum(n))
    
    pop_df %>% 
      filter(year %in% c(2011,2024,2025),citizenship=="total",resident=="permanent",age==20)
  }
  
  #extrapolate numbers by month
  pop_df = pop_df %>% 
    expand_grid(month=1:12) %>% 
    arrange(reg_agg_level, reg_id,reg_name,citizenship,age,year,month) %>%
    # create a continuous time index and n_year only for month 1
    dplyr::mutate(t = year + (month - 1) / 12,
                  n_year = if_else(month == 1, n, NA_real_)) %>%
    # linear interpolation (uses next year's value)
    group_by(reg_agg_level, reg_id,reg_name, citizenship, age) %>%
    dplyr::mutate(n = approx(t[!is.na(n_year)],
                             n_year[!is.na(n_year)],
                             xout = t,
                             rule = 2)$y) %>%
    ungroup() %>% 
    dplyr::select(year,month,reg_agg_level, reg_id,reg_name,citizenship,age,n) %>% 
    filter(!(year==2025 & month>1))
  
  return(pop_df)
}

