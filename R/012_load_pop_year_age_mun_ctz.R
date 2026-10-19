load_pop_year_age_mun_ctz = function(mun_df, last_year = 2024){
  #from https://www.pxweb.bfs.admin.ch/pxweb/fr/
  file_names = c("pop_mun_age_14.xlsx",#added because the numbers correspond to the population at 31 Dec
                 "pop_mun_age_15_35_permanent.xlsx",
                 #"pop_mun_age_15_35_nonpermanent.xlsx",
                 "pop_mun_age_36_50_permanent.xlsx",
                 "pop_mun_2025_age_14_49_permanent.xlsx")
                 #"pop_mun_age_36_50_nonpermanent.xlsx")
  
  # Prepend folder path
  files <- file.path(paste0(code_root_path,"data/population_data"), file_names)
  
  
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
    
    pop_df %>% filter(year==2025,reg_agg_level=="municipality",reg_name =="Zürich",age==15,citizenship=="swiss")
  }
  
  #----------------------------------------------------------------------------
  #Municipality level: map to mun_id (via new_mun_df) and AGGREGATE by mun_id BEFORE the
  #monthly interpolation. Communes that merged in recent years (several hist_mun_id -> one
  #mun_id, e.g. Grolley + Ponthaux -> Grolley-Ponthaux) then form a single continuous
  #series over time, so the interpolation can use the real latest-year value of the merged
  #commune instead of flat-extrapolating the former communes separately.
  #municipality subset mapped to mun_id (via new_mun_df: hist_mun_id -> mun_id)
  mun_joined = pop_df %>%
    filter(reg_agg_level == "municipality") %>%
    dplyr::mutate(reg_id = as.numeric(reg_id)) %>%
    left_join(mun_df %>% dplyr::select(ctn_abbr, ctn_id, dist_name, dist_id, mun_id, mun_name, reg_id = hist_mun_id),
              by = "reg_id")

  #one identity row per mun_id, so a mun_id whose former communes disagree on
  #canton/district/name in new_mun_df is NOT split into several rows (which would
  #double-count that commune); the summation below is on mun_id only
  mun_meta = mun_joined %>%
    dplyr::distinct(mun_id, ctn_abbr, ctn_id, dist_name, dist_id, mun_name) %>%
    group_by(mun_id) %>% dplyr::slice(1) %>% ungroup()

  mun_agg = mun_joined %>%
    group_by(reg_agg_level, mun_id, citizenship, age, year) %>%
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    left_join(mun_meta, by = "mun_id") %>%
    #reuse reg_id/reg_name as the common grouping key for the interpolation below
    dplyr::mutate(reg_id = as.character(mun_id), reg_name = mun_name)

  #per mun_id x year: list of former communes summed into it (for the sanity check below).
  #Built from mun_joined (reg_name still present), so merged communes show their
  #components (e.g. "Grolley, Ponthaux" before the merger, "Grolley-Ponthaux" after).
  mun_components = mun_joined %>%
    group_by(mun_id, year) %>%
    dplyr::summarise(mun_name = dplyr::first(mun_name),
                     n        = sum(n),
                     communes = paste(sort(unique(reg_name)), collapse = ", "),
                     .groups  = "drop")

  #put municipality (now aggregated by mun_id) back with the other aggregation levels
  pop_df = bind_rows(pop_df %>% filter(reg_agg_level != "municipality"),
                     mun_agg)

  #extrapolate numbers by month
  pop_df = pop_df %>%
    expand_grid(month=1:12) %>%
    arrange(reg_agg_level, reg_id,reg_name,citizenship,age,year,month) %>%
    # create a continuous time index and n_year only for month 1
    dplyr::mutate(t = year + (month - 1) / 12,
                  n_year = if_else(month == 1, n, NA_real_)) %>%
    # linear interpolation (uses next year's value); the <2-value branch is a safety net
    # (merged communes are now aggregated above, so they are no longer single-year series)
    group_by(reg_agg_level, reg_id,reg_name, citizenship, age) %>%
    dplyr::mutate(n = {
      idx = !is.na(n_year)
      if(sum(idx) >= 2){
        approx(t[idx], n_year[idx], xout = t, rule = 2)$y
      } else if(sum(idx) == 1){
        rep(n_year[idx], length(t))   # single yearly value: carry it constant over months
      } else {
        rep(NA_real_, length(t))      # no yearly value at all
      }
    }) %>%
    ungroup() %>%
    dplyr::select(year,month,reg_agg_level, reg_id,reg_name,citizenship,age,n,
                  ctn_abbr, ctn_id, dist_name, dist_id, mun_id, mun_name) %>%
    filter(year < last_year+1)

  #list of dataframe by level of aggregation
  pop_df_list <- split(pop_df, pop_df$reg_agg_level)

  if(FALSE){
    #check Swiss mother (i.e., mother_municipality 8100), with missing mun_id
    birth_1987_2024 %>%
      filter(is.na(mother_mun_id),mother_municipality==8100)
  }

  if(FALSE){
    #sanity check for communes that resulted from a RECENT merger (in the last 3 years):
    #once aggregated by mun_id, the total population should be continuous across the merger
    #year (sum of former communes ~ merged commune, no artificial jump/drop).
    #A recent merger = a mun_id whose former communes (hist_mun_id) were abolished
    #(hist_mun_ab_date) within the last 3 years. The 'communes' column lists which
    #municipalities are summed into the current mun_id each year.
    recent_cutoff = as.Date(paste0(last_year - 3, "-01-01"))
    merged_mun_ids = mun_df %>%
      group_by(mun_id) %>%
      dplyr::summarise(recent_merge = any(!is.na(hist_mun_ab_date) & hist_mun_ab_date >= recent_cutoff),
                       .groups = "drop") %>%
      filter(recent_merge) %>% pull(mun_id)
    mun_components %>%
      filter(mun_id %in% merged_mun_ids) %>%
      arrange(mun_id, year) %>%
      print(n = 200)
  }

  if(FALSE){
    #check rows with missing mun_name, i.e., where reg_id could not be matched with any hist_mun_id of mun_df: 0 row
    pop_df_list[["municipality"]] %>% filter(is.na(mun_name))
  }
  
  return(pop_df_list)
}

