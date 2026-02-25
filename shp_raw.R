load_shp_data = function(){
  data1<-read.spss("data/shp_data/Data_SPSS/SHP-Data-Longfile-SPSS/SHPLONG_P_USER.sav", # indicates path to access file
                   to.data.frame = TRUE,
                   use.value.labels = FALSE,# Does not convert variables with value labels into factors
                   use.missings = FALSE) # Does not convert negative values into missing values (NA).
  ## • version B - numeric and factor variables
  data_p<-read.spss("data/shp_data/Data_SPSS/SHP-Data-Longfile-SPSS/SHPLONG_P_USER.sav", 
                    to.data.frame = TRUE,
                    add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                    use.missings = TRUE)# Missing coded as NA without any distinction
  
  data_h<-read.spss("data/shp_data/Data_SPSS/SHP-Data-Longfile-SPSS/SHPLONG_H_USER.sav", 
                    to.data.frame = TRUE,
                    add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                    use.missings = TRUE)# Missing coded as NA without any distinction
  colnames(data_p) <- tolower(colnames(data_p))
  colnames(data_p) <-  gsub("99", "", colnames(data_p))
  colnames(data_h) <- tolower(colnames(data_h))
  colnames(data_h) <-  gsub("99", "", colnames(data_h))
  
  data_p2<-read.spss("data/shp_data/Data_SPSS/SHP-Data-W1-W25-SPSS/W1_1999/SHP99_P_USER.sav", 
                     to.data.frame = TRUE,
                     add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                     use.missings = TRUE)# Missing coded as NA without any distinction
  
  data_h2<-read.spss("data/shp_data/Data_SPSS/SHP-Data-W1-W25-SPSS/W1_1999/SHP99_H_USER.sav", 
                     to.data.frame = TRUE,
                     add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                     use.missings = TRUE)# Missing coded as NA without any distinction
  colnames(data_p2) <- tolower(colnames(data_p2))
  colnames(data_p2) <-  gsub("99", "", colnames(data_p2))
  colnames(data_h2) <- tolower(colnames(data_h2))
  colnames(data_h2) <-  gsub("99", "", colnames(data_h2))
  
  
  data_p = data_p %>% 
    as.data.frame() %>% 
    dplyr::select(variables_p_df$var) %>% 
    filter(status %in% c("individual questionnaire","grid only")) %>% 
    dplyr::mutate(pdate = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
                  pyear = year(pdate))
  data_p2 = data_p2 %>% 
    as.data.frame() %>% 
    dplyr::select(dplyr::any_of(variables_p_df$var)) %>% 
    filter(status %in% c("individual questionnaire","grid only")) %>% 
    dplyr::mutate(pdate = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
                  pyear = year(pdate))
  
  data_h = data_h %>% 
    as.data.frame() %>% 
    dplyr::select(variables_h_df$var) %>%
    dplyr::mutate(ctn_abbr = toupper(substr(canton, 1, 2))) %>% dplyr::select(-canton) %>% 
    dplyr::mutate(hdate = as.POSIXct(hdate, origin = "1582-10-14", tz = "UTC"))
  data_h2 = data_h2 %>% 
    as.data.frame() %>% 
    dplyr::select(dplyr::any_of(variables_h_df$var)) %>% 
    dplyr::mutate(ctn_abbr = toupper(substr(canton, 1, 2))) %>% dplyr::select(-canton) %>% 
    dplyr::mutate(hdate = as.POSIXct(hdate, origin = "1582-10-14", tz = "UTC"))
  
  
  data_p[1,] %>% 
    left_join(data_h %>% dplyr::select(idhous,hdate,ctn_abbr),by=c("idhous"="idhous"))
  
  data_p2 %>% 
    left_join(data_h2 %>% dplyr::select(idhous,hdate,ctn_abbr),by=c("idhous"="idhous")) %>% 
    dim()
  
  data_p2 %>% 
    left_join(data_h2 %>% dplyr::select(idhous,hdate,ctn_abbr),by=c("idhous"="idhous")) %>% 
    dim()
  
  data_p2 %>% 
    left_join(data_h2 %>% dplyr::select(idhous,hdate,ctn_abbr),by=c("idhous"="idhous")) %>% 
    filter(is.na(ctn_abbr))
  
  
  
  
  setdiff(colnames(data_p),colnames(data_p2))
  setdiff(colnames(data_p2),colnames(data_p))
  setdiff(colnames(data_h),colnames(data_h2))
  setdiff(colnames(data_h),colnames(data_h2))  
  View(data_h)
  
  
  dim(data_h)
  dim(data_h %>% filter(year==1999))
  dim(data_h2)
  
  dim(data_p)
  dim(data_p %>% filter(year==1999))
  dim(data_p2)
  
  data_p %>% filter(!is.na(filter)) %>% View()
  
  
  
  
  path = "data/shp_data/Data_SPSS/SHP-Data-W1-W25-SPSS/"
  
  folders <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  data_wave = lapply(as.list(folders),function(x){
    year_abbr = stringr::str_sub(x, -2)
    
    data_p_wave = read.spss(paste0( "data/shp_data/Data_SPSS/SHP-Data-W1-W25-SPSS/",x,"/SHP",year_abbr,"_P_USER.sav"),
                            to.data.frame = TRUE,
                            add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                            use.missings = TRUE)# Missing coded as NA without any distinction
    
    data_h_wave = read.spss(paste0( "data/shp_data/Data_SPSS/SHP-Data-W1-W25-SPSS/",x,"/SHP",year_abbr,"_H_USER.sav"),
                            to.data.frame = TRUE,
                            add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                            use.missings = TRUE) 
    
    
    colnames(data_p_wave) <- tolower(colnames(data_p_wave))
    colnames(data_p_wave) <-  gsub(year_abbr, "", colnames(data_p_wave))
    
    colnames(data_h_wave) <- tolower(colnames(data_h_wave))
    colnames(data_h_wave) <-  gsub(year_abbr, "", colnames(data_h_wave))
    
    
    data_p_wave = data_p_wave %>% 
      as.data.frame() %>% 
      dplyr::select(dplyr::any_of(variables_p_df$var)) %>% 
      filter(status %in% c("individual questionnaire","grid only")) %>% 
      dplyr::mutate(pdate = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
                    pyear = year(pdate))
    
    data_h_wave = data_h_wave %>% 
      as.data.frame() %>% 
      dplyr::select(dplyr::any_of(variables_h_df$var)) %>% 
      dplyr::mutate(ctn_abbr = toupper(substr(canton, 1, 2))) %>% dplyr::select(-canton) %>% 
      dplyr::mutate(hdate = as.POSIXct(hdate, origin = "1582-10-14", tz = "UTC"))
    
    print(x)
    
    
    data_p_wave = data_p_wave %>% 
      left_join(data_h_wave %>% dplyr::select(idhous,hdate,ctn_abbr),by=c("idhous"="idhous"),
                relationship = "many-to-one" ) %>% #stop if a row of data_p2 joins with more than one row of data_h2
      filter(!is.na(ctn_abbr))
    
    if(sum(is.na(data_join$ctn_abbr))>1){
      stop()
    }
    
    return(list(data_p_wave = data_p_wave %>% dplyr::mutate(year_abbr = year_abbr),
                data_h_wave = data_h_wave %>% dplyr::mutate(year_abbr = year_abbr)))
  })
  
  
  (data_wave[[1]])[["data_p_wave"]] %>% dim()
  (data_wave[[2]])[["data_p_wave"]] %>% dim()
  
  data_p %>% filter(is.na(age))
  
  
  df_p0 = read.spss("data/shp_data/Data_SPSS/SHP-Data-Longfile-SPSS/SHPLONG_P_USER.sav", 
                    to.data.frame = TRUE,
                    add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                    use.missings = TRUE)
  colnames(df_p0) <- tolower(colnames(df_p0))
  df_h0 = read.spss("data/shp_data/Data_SPSS/SHP-Data-Longfile-SPSS/SHPLONG_H_USER.sav", 
                    to.data.frame = TRUE,
                    add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters 
                    use.missings = TRUE)
  colnames(df_h0) <- tolower(colnames(df_h0))
  
  
  
  
  data_h %>% filter(hdate<ymd("1990-01-01"))
  data_p %>% filter(pdate<ymd("1990-01-01"))
  
  #year never missing for p data
  df_p0 %>% filter(is.na(year)) %>% dim()
  df_h0 %>% filter(is.na(year)) %>% dim()
  df_p0$year %>% range()
  df_h0$year %>% range()
  
  df_p0 %>%
    group_by(year) %>% 
    dplyr::summarise(n=n()) %>% arrange(year)
  df_h0 %>% filter(is.na(year))
  df_h0 %>%
    group_by(year) %>% 
    dplyr::summarise(n=n()) %>% arrange(year)
  
  df_p0 %>% 
    dplyr::mutate(pdate2 = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
                  year2 = year(pdate2)) %>% 
    filter(idhous==51,idpers==5101,age==43) %>% dplyr::select(year,pdate,age,pdate2,year2)
  data_p %>% filter(idhous==51,idpers==5101,age==43) %>% dplyr::select(pdate,age)
  
  
  df_p0 %>% 
    dplyr::mutate(pdate2 = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
                  year2 = year(pdate2)) %>% 
    group_by(year2<1999,is.na(year)) %>% 
    dplyr::summarise(n=n())
  
  
  as.POSIXct(11903500800, origin = "1582-10-14", tz = "UTC")
  
  #dimension
  dim(df_p0)
  dim(df_h0)
  
  #check date
  df_p0 %>% 
    dplyr::mutate(pdate2 = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
                  year2 = year(pdate2)) %>%
    dplyr::select(year,year2,pdate,pdate2) %>% distinct() %>% arrange(year) %>% 
    pull(year2) %>% 
    range()
  
  df_h0 %>% 
    dplyr::mutate(hdate2 = as.POSIXct(hdate, origin = "1582-10-14", tz = "UTC"),
                  year2 = year(hdate2)) %>%
    dplyr::select(year,year2,hdate,hdate2) %>% distinct() %>% arrange(year) %>% 
    pull(year2) %>% range()
  
  
  df_h0 %>% 
    dplyr::mutate(hdate2 = as.POSIXct(hdate, origin = "1582-10-14", tz = "UTC"))
  
  df_p0 %>% filter(is.na(year)) %>% dim()
  df_h0 %>% filter(is.na(year)) %>% dim()
  
  df_h0 %>% filter()
  
  df_p0 %>% filter(is.na(year)) %>% dim()
  df_h0 %>% filter(is.na(year)) %>% dim()
  
  df_p0 %>% filter(idhous==44762,idpers==4476202) %>% pull(pdate)
  
  data_p %>% 
    dplyr::select(idhous, idpers, age) %>% 
    dplyr::group_by(idhous, idpers, age) %>% 
    dplyr::mutate(n=n()) %>% ungroup() %>% 
    dplyr::filter(n > 1) %>% 
    arrange(idhous,idpers,age)
  
  
  data_p_wave = rbindlist(lapply(data_wave,function(x) x[["data_p_wave"]]),
                          fill=TRUE) #some columns are missing for certain wave, they are assign NA
  
  dim(data_p_wave)
  dim(data_p)
  
  data_p %>% filter(is.na(pdate))
  data_p_wave %>% filter(is.na(pdate))
  
  #idpers 11102 has missing pdate in data_p_wave, why? because missing pdate in data_p is given 1959-12-29
  data_p %>% filter(idpers==11102)
  data_p_wave %>% filter(idpers==11102)
  
  data_p %>% 
    left_join(data_p_wave %>% dplyr::select(idhous, ))
  
  
  
  data_p_w1 = data_wave[[1]]$data_p_wave
  data_h_w1 = data_wave[[1]]$data_h_wave
  
  
  setdiff(colnames(data_p2),colnames(data_p_w1))
  setdiff(colnames(data_p_w1),colnames(data_p2))
  setdiff(colnames(data_h2),colnames(data_h_w1))
  setdiff(colnames(data_h_w1),colnames(data_h2))
  
  data_p_w1 %>% dim()
  
  colnames(data_p_w1)
  colnames(data_h_w1)
  
  
  data_p_w1
  
  
  
  
  #household data
  variables_h_df = data.frame(var= c("idhous", "refper", "filter", "hmode","hdate","stathh","sthhre",#"idint"
                                     "nbkid",
                                     "canton",
                                     "hf52"),
                              def =  c("Identification number of household",
                                       "Identification of reference person",
                                       "Identification of the survey",
                                       "Mode of data collection",
                                       "Date of household interview with reference person",
                                       "Status household questionnaire",
                                       "Reason for not completing household questionnaire",#"Identification number of interviewer",
                                       "Number of children in household: 0 to 17 years",
                                       "Canton of residence",
                                       "External help: Child care: Monthly expenses"))
  
  h_df = data_h %>% 
    as.data.frame() %>% 
    dplyr::select(variables_h_df$var) %>% 
    dplyr::mutate(hdate = as.POSIXct(hdate, origin = "1582-10-14", tz = "UTC")) #start of the Gregorian calendar, used by spss
  
  table(h_df$hdate) %>% sort(decreasing=TRUE) %>% .[1:10]
  
  
  variables_p_df = data.frame(var= c("idhous", "idpers", "filter","pdate", "status","plingu",#"idint",
                                     "age","sex",
                                     "pd80", "pd81", "pd82",
                                     "pd90", "pd91", "pd92", "pd93", "pd94", "pd95",
                                     "pp20", "pp21", "pp22", "pp23"),
                              def =  c("Identification number of household",
                                       "identification number of the person",
                                       "Identification of the survey",
                                       "Date of personal interview",
                                       "Type of interviews completed: grid, proxy, personal",
                                       "Interview language",# "Identification number of interviewer",
                                       
                                       "Age in year of interview",
                                       "Sex",
                                       
                                       "Total number children wanted",
                                       "Child wanted in the next 24 months",
                                       "Ideal family size",
                                       
                                       "Opinion on family: marriage is an outdated institution",
                                       "Opinion on family: job preserves independance",
                                       "Opinion on family: child suffers with working mother",
                                       "Opinion on family: child suffers with unmaried parents",
                                       "Opinion on family: happiness of a child living with a single parent",
                                       "Opinion on family: happiness of a child living with same-sex parents",
                                       
                                       "Gender: Women in general penalized",
                                       "Gender: Personnally penalized",
                                       "Gender: In favour of measures",
                                       "Gender: Personal action"
                              ))
  
  
  data_p %>% filter(idhous==41) %>% View()
  
  p_df = data_p %>% 
    as.data.frame() %>% 
    dplyr::select(variables_p_df$var) %>% 
    filter(status %in% c("individual questionnaire","grid only")) %>% 
    dplyr::mutate(pdate = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
                  pyear = year(pdate))
  
  p_df %>% 
    dplyr::select(idhous,pdate,age) %>% 
    #.[1,] %>% 
    left_join(h_df %>% dplyr::select(idhous,hdate,ctn_abbr),by=c("idhous"="idhous")) %>% 
    dplyr::mutate(diff_days = abs(as.numeric(difftime(pdate, hdate, units = "days")))) %>%
    group_by(idhous, pdate) %>%
    slice_min(diff_days, n = 1, with_ties = FALSE) %>%
    ungroup()
  table(p_df$pdate) %>% sort(decreasing=TRUE) %>% .[1:10]
  dim(p_df)
  
  
  p_df %>% filter(p)
  
  p_df %>% filter(pdate==ymd("1959-12-29")) %>% View()
  
  p_df %>% filter(diff_days>200)
  
  p_df %>% filter(is.na(ctn_abbr)) %>% View()
  
  
  
  
  p_df %>% filter(idpers==4104) %>% View()
  
  
  p_df %>% head()
  
  
  p_df |>
    dplyr::summarise(dplyr::across(everything(),
                                   list(#n= function(x) length(x),
                                     p_duplicate = function(x) mean(grepl("duplicate",x)),
                                     n_nna = ~sum(!is.na(.)),
                                     prop_nna = ~mean(!is.na(.))))) |>
    tidyr::pivot_longer(everything(), names_to = c("variable", "outcome"),names_pattern = "^([^_]+)_(.+)$" ) |>
    mutate(variable = factor(variable,levels=variables_p_df$var)) %>% 
    ggplot(aes(x=variable,y=value))+
    geom_point()+
    facet_grid(outcome~.,scale="free_y")+
    expand_limits(y=0)
  
  
  p_df |>
    dplyr::summarise(dplyr::across(where(is.numeric),
                                   list(n= function(x) length(x),
                                        n_na = ~sum(is.na(.)),
                                        prop_na = ~mean(is.na(.)),
                                        mean = ~mean(., na.rm = TRUE),
                                        median = ~median(., na.rm = TRUE)))) |>
    tidyr::pivot_longer(everything(), names_to = c("variable", "outcome"),names_pattern = "^([^_]+)_(.+)$" ) |>
    tidyr::pivot_wider(  names_from = variable, values_from = value )
  
  
  
  data_p$pd81 %>% class()
  
  c("Total number children wanted", "Child wanted in the next 24 months", "Ideal family size"
    
    
    data1 %>% colnames()
    data1$age
    
    data1$pc81
    
    vars <- c("age","sex","civsta","cohast","hab_ch",
              "nat_1_","nat_2_","nat_3_","reg_1_","reg_2_","reg_3_","ownkid",
              "pl26t","pd36","pd40","pd41","pd45","pd46","pd50","pd51","pd55","pd56","pd60","pd61","pd65","pd66","pd70","pd71")
    
    vars <- c("pd80","pd81","pd82",
              "pd160","pd161","pd162","pd163","pd164",
              "pi58a",
              "pf64","pd90","pd91","pd92","pd93","pd94","pd95",
              "pp10","pp13","pp20","pp21","pp22","pp23",
              "pp63","pp71",
              "pc126")
    data1[,vars] %>% summary()
    
    data1 |> dplyr::mutate(pd80 = dplyr::na_if(pd80, -3))
    
    data1 %>% colnames()
    
    data1[, vars]
    
    c("idpers", "year", "idint", "filter")
    
    |>
      dplyr::mutate(pd80 = dplyr::na_if(pd80, -3)) %>% 
      dplyr::summarise(dplyr::across(where(is.numeric),
                                     list(n= function(x) length(x),
                                          n_na = ~sum(is.na(.)),
                                          prop_na = ~mean(is.na(.)),
                                          mean = ~mean(., na.rm = TRUE),
                                          median = ~median(., na.rm = TRUE)))) |>
      tidyr::pivot_longer(everything(), names_to = c("variable", "outcome"),names_pattern = "^([^_]+)_(.+)$" ) |>
      tidyr::pivot_wider(  names_from = variable, values_from = value )
    
    data1[,"pd80"] %>% sum(is.na(.))
}
