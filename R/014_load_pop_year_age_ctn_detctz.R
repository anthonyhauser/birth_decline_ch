load_pop_year_age_ctn_detctz = function(){

  # Prepare folder path
  file_names = paste0("pop_",2010:2024,"_age_nat2_ctn.xlsx")
  files <- file.path(paste0(code_root_path,"data/population_data"), file_names)
  #Filtering was applied during data downloading: 1) population résidante, 2) Autorisation de résidence - total, 3) Femme
  
  #load dataset by year
  data_list <- list()
  for (i in seq_along(files)) {
    tmp <- read_excel(files[i], skip = 2)
    endrow_n = min(which(is.na(tmp[,9]))) -1
    tmp <- tmp[1:endrow_n,]
    colnames(tmp)[1:12] = c("year1","year2","ctn_abbr","ctn_name","unknown_var1","resident","unknown_var2","autor_residence","ctz_id","ctz_name","unkown_var3","sex")
    
    if(FALSE){
      colnames(tmp)
      res <- lapply(names(tmp[,1:12]), function(col) {
        cat("\n", col, ":\n", sep = "")
        print(unique(tmp[[col]]))
      })
    }
    
    data_list[[i]] = tmp %>% 
      pivot_longer(cols = ends_with("ans"),names_to = "age",values_to = "n") %>%
      dplyr::select(year=year1,ctn_abbr,ctz_id,ctz_name,resident,age,n) %>% 
      #fill in empty parts
      fill(year,ctn_abbr,ctz_id,ctz_name,resident, .direction = "down") %>% 
      filter(!(ctn_abbr %in% c("8100")),#remove total (all Switzerland) 
             !(ctz_id %in% c(-99999)),#remove total nationality
             ctz_name!="Nationalité - total",#remove total nationality
             resident=="Population résidante permanente", #filter on permanent population
             age!="Âge - total") %>% #remove total age
      dplyr::select(-resident) %>% 
      #the number corresponds the population at the 31 Dec of the year for a given age, we thus need to add 1 to the year and to the age to get population of the next age for age +1
      dplyr::mutate(year = as.numeric(year)+1,
                    age = as.numeric(readr::parse_number(age))+1,
                    ctz_id=as.numeric(ctz_id)) %>% 
      filter(age %in% 15:50) 
    
  }
  pop_df = rbindlist(data_list)
  
  if(FALSE){
    #no ctn_abbr with -9 
    pop_df %>% filter(ctn_abbr %in% c("-9")) %>% group_by(ctn_abbr) %>% dplyr::summarise(n=sum(n))
    #Pop without citizenship indication, apatrid or not attributed to current boarder removed
    pop_df %>%
      dplyr::mutate(ctz_id = ifelse(ctz_id %in% c(-1,-6,-9),ctz_id,0),
                    ctz_name = ifelse(ctz_id %in% c(-1,-6,-9),ctz_name,"Identified country")) %>% 
      group_by(ctz_id,ctz_name) %>% dplyr::summarise(n=sum(n)) %>% ungroup() %>% dplyr::mutate(p=n/sum(n))
  }
  
  #remove missing canton (never missing actually) or ctz country
  pop_df = pop_df %>% 
    filter(!(ctn_abbr %in% c("-9")),#remove missing canton
           !(ctz_id %in% c(-1,-6,-9))) #remove missing ctz country
  
  if(FALSE){
    #check no missing
    pop_df %>% dplyr::mutate(row_id = row_number()) %>% filter(if_any(everything(), is.na))
    
    res <- lapply(names(pop_df), function(col) {
      cat("\n", col, ":\n", sep = "")
      print(unique(pop_df[[col]]))
    })
    #check that each year has the same number of rows
    pop_df %>% group_by(year) %>%  dplyr::summarise(n=n())
    #check Tibet (present in birth data) but not in population data
    pop_df %>% filter(ctz_id==8543)
  }
  
  return(pop_df)
}