load_vote_data = function(mun_df, pop_mun_df){
  #from Kaspar
  file_names <- c("px-x-1703030000_101_20260127-171511.xlsx",
                  "px-x-1703030000_101_20260127-171701.xlsx",
                  "px-x-1703030000_101_20260127-171840.xlsx",
                  "px-x-1703030000_101_20260127-171902.xlsx")
  
  # Prepend folder path
  files <- file.path("data/municipality_var_data", file_names)
  
  # Read in a for loop
  data_list <- list()
  for (i in seq_along(files)) {
    # Read the Excel file skipping first 2 rows
    tmp <- read_excel(files[i], skip = 2)
    
    # Identify first completely blank row
    first_blank <- which(apply(tmp, 1, function(row) all(is.na(row) | row == "")))[1]
    
    # Keep only rows before the first blank
    if (!is.na(first_blank)) {
      tmp <- tmp[1:(first_blank - 1), ]
    }
    
    # Save to list
    data_list[[i]] <- tmp %>% 
      dplyr::rename(reg_id = ...1,
                    reg_name = ...2,
                    vote_id = ...3,
                    vote_info = ...4,
                    eligible_voters = `Stimmberechtigte`,
                    votes_cast = `Abgegebene Stimmen`,
                    part_prop = `Beteiligung in %`,
                    valid_ballots = `Gültige Stimmzettel`,
                    yes_votes = Ja,
                    no_votes = Nein,
                    yes_prop = `Ja in %`) %>% 
      separate(vote_info, into = c("date", "vote_object_de"), sep = " ", extra = "merge") %>%
      dplyr::mutate(date = as.Date(date),
                    reg_agg_level = case_when(
                      reg_name == "Schweiz"           ~ "national",
                      str_starts(reg_name, "- ")      ~ "canton",
                      str_starts(reg_name, ">> ")     ~ "district",
                      str_starts(reg_name, "......")  ~ "municipality",
                      TRUE                            ~ NA),
                    reg_name = str_remove(reg_name, "^(- |>> |\\.\\.\\.\\.+)")) %>% 
      dplyr::select(reg_id,reg_name,reg_agg_level,date,vote_object_de,part_prop, yes_prop)
  }
  
  if(FALSE){
    rbindlist(data_list) %>% 
      pull(vote_object_de) %>% unique()
  }
  
  vote_df = rbindlist(data_list)  %>% 
    dplyr::mutate(vote_object = case_when(str_detect(vote_object_de, "Frauenstimm-") ~ "womens_suffrage",
                                          str_detect(vote_object_de, "Familien stärken") ~ "family_tax_education_support",
                                          str_detect(vote_object_de, "Vaterschaftsurlaub") ~ "paternity_leave",
                                          str_detect(vote_object_de, "Kinderdrittbetreuungskosten") ~ "reduce_childcare_tax",
                                          TRUE ~ NA),
                  part_prop = as.numeric(na_if(part_prop, "...")),
                  yes_prop  = as.numeric(na_if(yes_prop,  "...")))
  
  if(FALSE){
    df_na_prop = vote_df %>% filter(is.na(part_prop) | is.na(yes_prop))
    #missing because: new group ("étranger", which might not have vote right in some region), new municipality, new canton (JU)
    #for some rows, missings only concern part_prop
    rbindlist(data_list) %>% right_join(df_na_prop %>% dplyr::select(reg_id,vote_object_de))
  }
  
  #remove rows where vote result is missing and where it's special population
  special_text <- c( "Anderes","autres", "altri","Ausland","étranger","Korrespondenz","corrispondenza")
  vote_df = vote_df %>% 
    filter(!str_detect(reg_name,
                       str_c(special_text, collapse = "|"))) %>% 
    filter(!is.na(yes_prop))
  
  #split by level of aggregation
  vote_df_list <- split(vote_df, vote_df$reg_agg_level)
  
  #add municipality---------------------------------------------------------------
  vote_df_list[["municipality"]] = vote_df_list[["municipality"]] %>%
    dplyr::mutate(reg_id = as.numeric(reg_id)) %>% 
    left_join(mun_df %>% dplyr::select(ctn_abbr, ctn_id, dist_name, dist_id, mun_id, mun_name, reg_id = hist_mun_id),
              by="reg_id")
  if(FALSE){
    #check the missing district: all are others, correspondance votes, foreigners -> they were already removed
    vote_df_list[["municipality"]] %>% filter(is.na(dist_name)) %>% dplyr::select(reg_id,reg_name,dist_name,dist_id) %>% distinct()
    #other NA: already removed
    vote_df_list[["municipality"]] %>%
      filter(if_any(everything(), ~ is.na(.)) & !is.na(dist_name))
  }
  vote_df_list[["municipality"]] = vote_df_list[["municipality"]] %>% 
    filter(!is.na(dist_id))
  if(FALSE){
    #check reg_id and vote_object with more than 1 row: none found
    vote_df_list[["municipality"]] %>%
      group_by(vote_object,reg_id) %>% 
      dplyr::mutate(n=n()) %>% ungroup() %>% filter(n>1)
  }
  #add population of women 15-50 (for weight sum)-------------------------------
  #population in 2024 by mun_id
  pop_mun_2024_df = pop_mun_df %>% 
    dplyr::filter(year==2024) %>% 
    group_by(mun_id) %>% 
    dplyr::summarise(n_pop = sum(n),.groups="drop")
  
  
  vote_df_list[["municipality"]] = vote_df_list[["municipality"]]  %>% 
    left_join(pop_mun_2024_df, by="mun_id")
  
  if(FALSE){
    #check missing n_pop: none
    vote_df_list[["municipality"]] %>% filter(is.na(n_pop))
  }
  
  return(vote_df_list)
}

