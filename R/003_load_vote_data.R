load_vote_data = function(){
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
    #missing because: new group ("étranger", which might not have vote right in some region), new munipality, new canton (JU)
    #for some rows, missings only concern part_prop
    rbindlist(data_list) %>% right_join(df_na_prop %>% dplyr::select(reg_id,vote_object_de))
  }
  
  #remove rows where vote result is missing
  vote_df = vote_df %>% 
    filter(!is.na(yes_prop))
  
  return(vote_df)
}

