# load_shp_data = function(){
#   #load data
#   data_p<-read.spss("data/shp_data/Data_SPSS/SHP-Data-Longfile-SPSS/SHPLONG_P_USER.sav",
#                    to.data.frame = TRUE,
#                    add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters
#                    use.missings = TRUE)# Missing coded as NA without any distinction
#   data_h<-read.spss("data/shp_data/Data_SPSS/SHP-Data-Longfile-SPSS/SHPLONG_H_USER.sav",
#                     to.data.frame = TRUE,
#                     add.undeclared.levels="no",# Does not convert to factor in case of numeric SPSS levels (not labels), and still converts to factor if the SPSS levels are characters
#                     use.missings = TRUE)# Missing coded as NA without any distinction
# 
#   #variables
#   #personal data
#   variables_p_df = data.frame(var= c("idhous","year", "idpers", "filter","pdate", "status","plingu",#"idint",
#                                      "age","sex",
#                                      "pd80", "pd81", "pd82",
#                                      "pd90", "pd91", "pd92", "pd93", "pd94", "pd95",
#                                      "pp20", "pp21", "pp22", "pp23"),
#                               def =  c("Identification number of household",
#                                        "Year",
#                                        "identification number of the person",
#                                        "Identification of the survey",
#                                        "Date of personal interview",
#                                        "Type of interviews completed: grid, proxy, personal",
#                                        "Interview language",# "Identification number of interviewer",
# 
#                                        "Age in year of interview",
#                                        "Sex",
# 
#                                        "Total number children wanted",
#                                        "Child wanted in the next 24 months",
#                                        "Ideal family size",
# 
#                                        "Opinion on family: marriage is an outdated institution",
#                                        "Opinion on family: job preserves independance",
#                                        "Opinion on family: child suffers with working mother",
#                                        "Opinion on family: child suffers with unmaried parents",
#                                        "Opinion on family: happiness of a child living with a single parent",
#                                        "Opinion on family: happiness of a child living with same-sex parents",
# 
#                                        "Gender: Women in general penalized",
#                                        "Gender: Personnally penalized",
#                                        "Gender: In favour of measures",
#                                        "Gender: Personal action" ))
#   #household data
#   variables_h_df = data.frame(var= c("idhous","year", "refper", "filter", "hmode","hdate","stathh","sthhre",#"idint"
#                                      "nbkid",
#                                      "canton",
#                                      "ctn_abbr",
#                                      "hf52"),
#                               def =  c("Identification number of household",
#                                        "Year",
#                                        "Identification of reference person",
#                                        "Identification of the survey",
#                                        "Mode of data collection",
#                                        "Date of household interview with reference person",
#                                        "Status household questionnaire",
#                                        "Reason for not completing household questionnaire",#"Identification number of interviewer",
#                                        "Number of children in household: 0 to 17 years",
#                                        "Canton of residence",
#                                        "Canton of residence (abbr)",
#                                        "External help: Child care: Monthly expenses"))
#   #all variables
#   variables_df = rbind(variables_p_df,
#                        variables_h_df %>% filter(!(var %in% variables_p_df$var))) %>%
#     dplyr::mutate(group = dplyr::case_when(
#                             var %in% c("idhous", "idpers", "refper") ~ "identifiers",
#                             var %in% c("year", "pdate", "hdate") ~ "time",
#                             var %in% c("status", "hmode", "filter", "stathh", "sthhre") ~ "survey_process",
#                             var %in% c("age", "sex", "plingu", "canton") ~ "sociodemographics",
#                             var %in% c("pd80","nbkid") ~ "number_of_children",
#                             var %in% c("pd81") ~ "child_wanted",
#                             grepl("^pd", var) ~ "family_opinions_preferences",
#                             grepl("^pp", var) ~ "gender_attitudes",
#                             var %in% c("hf52") ~ "household_children_economics",
#                             TRUE ~ NA )) %>%
#     dplyr::mutate(is_reg_var = ifelse(group %in% c("family_opinions_preferences","gender_attitudes","household_children_economics",
#                                                    "number_of_children","child_wanted"), 1, 0),
#                  type = case_when(var %in% c("pd81") ~ "factor_2lev_n_y",
#                                    var %in% c("pd90", "pd91", "pd92", "pd93", "pp20", "pp21", "pp22", "pp23")  ~  "factor_11lev_0_10",
#                                    var %in% c("pd80", "pd82","nbkid")  ~ "numeric_max10",
#                                    var %in% c("pd94", "pd95")  ~ "factor_11lev_0_10",
#                                    var %in% c("hf52") ~ "numeric",
#                                    TRUE ~ NA))
# 
# 
#   #details on questions:
#   #pp20-23: data\shp_data\Documentation\SHP_Questionnaires\W2_2000\QuestionML-P-W2.pdf pages 321-24
#   #pd80-82: data\shp_data\Documentation\SHP_Questionnaires\W4_2002\QuestionML-P-W4.pdf pages 270-1
#   #pd90-93: data\shp_data\Documentation\SHP_Questionnaires\W4_2002\QuestionML-P-W4.pdf pages 344-7
#   #pd994-95: data\shp_data\Documentation\SHP_Questionnaires\W4_2021\QuestionML-P-W23.pdf pages 322-23
# 
# 
#   #clean data
#   data_p = data_p %>%
#     as.data.frame() %>%
#     dplyr::select(variables_p_df$var) %>%
#     filter(status %in% c("individual questionnaire","grid only")) %>%
#     dplyr::mutate(pdate = as.POSIXct(pdate, origin = "1582-10-14", tz = "UTC"),
#                   pyear = year(pdate))
#   data_h = data_h %>%
#     as.data.frame() %>%
#     dplyr::select(any_of(variables_h_df$var)) %>%
#     dplyr::mutate(ctn_abbr = toupper(substr(canton, 1, 2))) %>% dplyr::select(-canton) %>%
#     dplyr::mutate(hdate = as.POSIXct(hdate, origin = "1582-10-14", tz = "UTC"),
#                   hyear = year(hdate))
# 
#   #check levels
#   if(FALSE){
#    data_p %>%
#       dplyr::select(any_of(variables_df %>% filter(is_reg_var==1) %>% pull(var))) %>%
#       glimpse()
#    data_h %>%
#       dplyr::select(any_of(variables_df %>% filter(is_reg_var==1) %>% pull(var))) %>%
#       glimpse()
# 
#   #factor variables: always 11 levels, already in correct order from 0 to 10
#   data_p %>%
#     dplyr::select(any_of(variables_df %>% filter(is_reg_var==1) %>% pull(var))) %>%
#     dplyr::select(where(is.factor)) %>%
#     lapply(levels)
#   data_h %>%
#     dplyr::select(any_of(variables_df %>% filter(is_reg_var==1) %>% pull(var))) %>%
#     dplyr::select(where(is.factor)) %>%
#     lapply(levels)
# 
#   #numeric variables:
#   data_p %>%
#     dplyr::select(any_of(variables_df %>% filter(is_reg_var==1) %>% pull(var))) %>%
#     dplyr::select(where(is.numeric)) %>%
#     lapply(function(x) sort(unique(x)))
#   data_h %>%
#     dplyr::select(any_of(variables_df %>% filter(is_reg_var==1) %>% pull(var))) %>%
#     dplyr::select(where(is.numeric)) %>%
#     lapply(function(x) sort(unique(x)))
# 
#   #pd80, pd81: limit at 10
#   #pd94-95: should be factor from 0 to 10
#   }
# 
#   #convert class
#   data_p <- data_p %>%
#     dplyr::mutate(dplyr::across(.cols =any_of(variables_df %>% filter(type=="factor_2lev_n_y") %>% pull(var)),
#                                 ~ as.numeric(factor(.x, levels = c("no","yes")))-1),
#                   dplyr::across(.cols = any_of(variables_df %>% filter(type=="factor_11lev_0_10") %>% pull(var)),
#                                 ~ as.numeric(.x)),
#                   dplyr::across(.cols = any_of(variables_df %>% filter(type=="numeric_max10") %>% pull(var)),
#                                 ~ pmin(10,.x)),
#                   dplyr::across(.cols = any_of(variables_df %>% filter(type=="numeric") %>% pull(var)),
#                                 ~ as.numeric(.x)))
#   data_h <- data_h %>%
#     dplyr::mutate(dplyr::across(.cols =any_of(variables_df %>% filter(type=="factor_2lev_n_y") %>% pull(var)),
#                                 ~ as.numeric(factor(.x, levels = c("no","yes")))-1),
#                   dplyr::across(.cols = any_of(variables_df %>% filter(type=="factor_11lev_0_10") %>% pull(var)),
#                                 ~ as.numeric(.x)),
#                   dplyr::across(.cols = any_of(variables_df %>% filter(type=="numeric_max10") %>% pull(var)),
#                                 ~ pmin(10,.x)),
#                   dplyr::across(.cols = any_of(variables_df %>% filter(type=="numeric") %>% pull(var)),
#                                 ~ as.numeric(.x)))
# 
# 
#   #check missing
#   if(FALSE){
#     #ctn_abbr
#     data_h %>% filter(is.na(ctn_abbr))# idhous=66735 missing ctn_abr
#     data_h %>% filter(idhous==66735) %>% pull(ctn_abbr) %>% na.omit() %>% unique() #idhous=66735 is AG
#   }
#   #assign
#   data_h = data_h %>%
#     dplyr::mutate(ctn_abbr = if_else(idhous==66735,"AG",ctn_abbr))
# 
#   #combine data_p with data_h
#   p_df = data_p %>%
#     left_join(data_h %>% rename(filterh = filter),by=c("idhous","year"),
#               relationship = "many-to-one")
# 
#   if(FALSE){
#     #check that each row of data_p binds with exactly 1 row of data_h:
#     #1) not more than 1 row as, with the relationship = "many-to-one", it would return an error otherwise
#     #2) not 0 row as no missing ctn_abbr
#     data_h %>% filter(is.na(ctn_abbr))
# 
#     #check if filter is the same between data_p and data_p
#     p_df %>% filter(filter!=filterh)
#   }
#   #remove filterh as same as filter
#   p_df = p_df %>% dplyr::select(-filterh) %>%
#     group_by(idpers) %>% dplyr::mutate(year_start = min(year),
#                                        age_start = min(age)) %>% ungroup()
# 
#   saveRDS(p_df,paste0(code_root_path,"savepoint/cleaned_shp_p_df.RDS"))
#   #check missing (NA or "._duplicate")
#   if(FALSE){
#     df = p_df %>%
#       dplyr::mutate(dplyr::across(everything(), as.character)) %>%
#       dplyr::mutate(row_id = dplyr::row_number()) %>%
#       tidyr::pivot_longer(cols = -row_id, names_to = "variable", values_to = "value") %>%
#       dplyr::group_by(variable) %>%
#       dplyr::summarise( p_duplicate = mean(grepl("duplicate", value)),
#                         p_na = mean(is.na(value)),
#                         p_duplicate_na = mean(grepl("duplicate", value) | is.na(value)), .groups = "drop")
# 
#     df %>%
#       tidyr::pivot_longer(cols = -variable,  names_to = "outcome", values_to = "value" ) %>%
#       mutate(variable = factor(variable,levels=variables_df$var)) %>%
#       ggplot(aes(x=variable,y=value,col=outcome))+
#       geom_point()+
#       scale_y_continuous(labels = scales::percent)+
#       expand_limits(y=0)
# 
#     df %>%
#       mutate(variable = factor(variable,levels=variables_df$var)) %>%
#       ggplot(aes(x=variable,y=1-p_duplicate_na))+
#       geom_point()+
#       scale_y_continuous(labels = scales::percent,name="Proportion of non-missing")+
#       expand_limits(y=0)
#   }
# 
#   # #put NA if "._duplicated_
#   # p_df <- p_df %>%
#   #   dplyr::mutate(dplyr::across(everything(), ~ ifelse(grepl("duplicate", as.character(.)), NA, .)))
# 
#   p_df = readRDS(paste0(code_root_path,"savepoint/cleaned_shp_p_df.RDS"))
#   #number of responses
#   df = p_df %>%
#     dplyr::mutate(dplyr::across(everything(), as.character)) %>%
#     dplyr::mutate(row_id = dplyr::row_number()) %>%
#     tidyr::pivot_longer(cols = -c(year,row_id), names_to = "var", values_to = "value") %>%
#     dplyr::group_by(var,year) %>%
#     dplyr::summarise(p_na = mean(is.na(value)),
#                      n_tot = n(),
#                      n_nna = sum(!is.na(value)), .groups = "drop") %>%
#     left_join(variables_df,by="var") %>%
#     filter(is_reg_var==1) %>%
#     dplyr::mutate(year = ymd(paste0(year,"-01-01")))
# 
#   df %>%
#     ggplot(aes(x=year,y=n_nna))+
#     geom_line(aes(x=year,y=n_tot),col="black",linetype = 2)+
#     geom_point(aes(col=var))+geom_line(aes(col=var))+
#     scale_y_continuous(name="Number of non-missing")+
#     scale_color_discrete(breaks=variables_df$var,labels=variables_df$def) +
#     expand_limits(y=0)+
#     facet_wrap(group~.)+
#     theme(legend.position = "bottom")
# 
#   df %>%
#     ggplot(aes(x=year,y=1-p_na,col=var))+
#     geom_point()+geom_line()+
#     scale_y_continuous(labels = scales::percent,name="Proportion of non-missing")+
#     scale_color_discrete(breaks=variables_df$var,labels=variables_df$def) +
#     expand_limits(y=0)+
#     facet_wrap(group~.)+
#     theme(legend.position = "bottom")
# 
#   #number of new participants per year
#   p_df %>%
#     dplyr::mutate(is_woman_15_50 = factor(as.numeric(sex=="woman" & age>=15 & age<=50))) %>%
#     group_by(year_start,is_woman_15_50) %>%
#     dplyr::summarise(n_new = length(unique(idpers)),.groups="drop") %>%
#     ggplot(aes(x=year_start,y=n_new,fill=is_woman_15_50))+
#     geom_bar(stat="identity",position = "stack")+
#     scale_y_continuous(name="Number of new participants")+
#     scale_fill_manual(name="Participants",values=c("gray","blue"),breaks = c(0,1),labels=c("All","Woman 15-50"))
# 
#   #mean
#   df = p_df %>%
#     filter(year_start %in% c(2010:2024),sex=="woman",age_start %in% 15:50) %>%
#     dplyr::mutate(dplyr::across(everything(), as.character)) %>%
#     dplyr::mutate(row_id = dplyr::row_number()) %>%
#     tidyr::pivot_longer(cols = -c(year,row_id), names_to = "var", values_to = "value") %>%
#     left_join(variables_df,by="var") %>%
#     filter(is_reg_var==1,!is.na(value)) %>%
#     dplyr::mutate(value=as.numeric(value)) %>%
#     group_by(year,var,def,group) %>%
#     dplyr::summarise(mean = mean(value),
#                      n_values = n(),.groups="drop") %>%
#     dplyr::mutate(year = ymd(paste0(year,"-01-01"))) %>%
#     group_by(year,group) %>%
#     dplyr::mutate(n_values_min = min(n_values),
#                   n_values_max = max(n_values),
#                   ymax=max(mean)*1.1) %>% ungroup() %>%
#     group_by(group) %>%
#     dplyr::mutate(ymax=max(mean)*1.1) %>% ungroup()
# 
#   plots <- df %>%
#     filter(n_values>=10) %>%
#     dplyr::mutate(def = str_wrap(def, width = 20)) %>%
#     split(.$group) %>%
#     map(~ ggplot(.x, aes(x = year, y = mean)) +
#           geom_point(aes(col = def)) + geom_line(aes(col = def)) +
#           geom_text(aes(y = 0, label = n_values_min),
#                     vjust = 0, show.legend = FALSE) +
#           geom_text(aes(y = ymax, label = n_values_max),
#                     vjust = 0, show.legend = FALSE) +
#           expand_limits(y = 0) +
#           expand_limits(y = max(.x$ymax*1.05, na.rm = TRUE))+
#           ggtitle(unique(.x$group)) +
#           theme(legend.position = "bottom"))
#   cowplot::plot_grid(plotlist = plots, ncol = 2)
# 
# 
# 
#   p_df %>%
#     filter(year_start %in% c(2006:2015),sex=="woman",age_start %in% 15:50) %>%
#     dplyr::select(idhous,idpers,year,year_start,plingu,age,ctn_abbr,pp21) %>%
#     filter(!is.na(pp21),!is.na(plingu)) %>%
#     group_by(year,plingu) %>%
#     dplyr::summarise(mean = mean(pp21),
#                      n_values = n()) %>% ungroup() %>%
#     dplyr::mutate(year = ymd(paste0(year,"-01-01"))) %>%
#     ggplot(aes(x = year, y = mean)) +
#     geom_point(aes(col = plingu)) + geom_line(aes(col = plingu)) +
#     geom_text(aes(y = mean, label = n_values),
#               vjust = 0, show.legend = FALSE) +
#     expand_limits(y = 0)
# 
# 
#   p_df %>%
#     filter(year_start %in% c(2010:2015),sex=="woman",age_start %in% 15:50) %>%
#     dplyr::select(idhous,idpers,year,year_start,plingu,age,ctn_abbr,pp21) %>%
#     left_join(canton_df %>% dplyr::select(ctn_abbr=ctn,NUTS2_name),by="ctn_abbr") %>%
#     filter(!is.na(pp21),!is.na(ctn_abbr)) %>%
#     group_by(year,NUTS2_name) %>%
#     dplyr::summarise(mean = mean(pp21),
#                      n_values = n()) %>% ungroup() %>%
#     dplyr::mutate(year = ymd(paste0(year,"-01-01"))) %>%
#     ggplot(aes(x = year, y = mean)) +
#     geom_point(aes(col = NUTS2_name)) + geom_line(aes(col = NUTS2_name)) +
#     geom_text(aes(y = mean, label = n_values),
#               vjust = 0, show.legend = FALSE) +
#     scale_y_continuous(name="Gender: Personnally penalized")+
#     expand_limits(y = 0)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#   p_df %>%
#     dplyr::summarise(dplyr::across(everything(),
#                                    list(#n= function(x) length(x),
#                                         p_duplicate = function(x) mean(grepl("duplicate",x)),
#                                         n_nna = ~sum(!is.na(.)),
#                                         prop_nna = ~mean(!is.na(.))))) |>
#     tidyr::pivot_longer(everything(), names_to = c("variable", "outcome"),names_pattern = "^([^_]+)_(.+)$" ) |>
#     mutate(variable = factor(variable,levels=variables_p_df$var)) %>%
#     ggplot(aes(x=variable,y=value))+
#     geom_point()+
#     facet_grid(outcome~.,scale="free_y")+
#     expand_limits(y=0)
# 
# 
#   p_df |>
#     dplyr::summarise(dplyr::across(where(is.numeric),
#                                    list(n= function(x) length(x),
#                                         n_na = ~sum(is.na(.)),
#                                         prop_na = ~mean(is.na(.)),
#                                         mean = ~mean(., na.rm = TRUE),
#                                         median = ~median(., na.rm = TRUE)))) |>
#     tidyr::pivot_longer(everything(), names_to = c("variable", "outcome"),names_pattern = "^([^_]+)_(.+)$" ) |>
#     tidyr::pivot_wider(  names_from = variable, values_from = value )
# 
# 
# 
#   data_p$pd81 %>% class()
# 
# 
# 
#   data1 %>% colnames()
#   data1$age
# 
#   data1$pc81
# 
#   vars <- c("age","sex","civsta","cohast","hab_ch",
#             "nat_1_","nat_2_","nat_3_","reg_1_","reg_2_","reg_3_","ownkid",
#             "pl26t","pd36","pd40","pd41","pd45","pd46","pd50","pd51","pd55","pd56","pd60","pd61","pd65","pd66","pd70","pd71")
# 
#    vars <- c("pd80","pd81","pd82",
#             "pd160","pd161","pd162","pd163","pd164",
#             "pi58a",
#             "pf64","pd90","pd91","pd92","pd93","pd94","pd95",
#             "pp10","pp13","pp20","pp21","pp22","pp23",
#             "pp63","pp71",
#             "pc126")
#   data1[,vars] %>% summary()
# 
#   data1 |> dplyr::mutate(pd80 = dplyr::na_if(pd80, -3))
# 
#   data1 %>% colnames()
# 
#   data1[, vars]
# 
#   c("idpers", "year", "idint", "filter")
# 
#   |>
#     dplyr::mutate(pd80 = dplyr::na_if(pd80, -3)) %>%
#     dplyr::summarise(dplyr::across(where(is.numeric),
#                                    list(n= function(x) length(x),
#                                         n_na = ~sum(is.na(.)),
#                                         prop_na = ~mean(is.na(.)),
#                                         mean = ~mean(., na.rm = TRUE),
#                                         median = ~median(., na.rm = TRUE)))) |>
#     tidyr::pivot_longer(everything(), names_to = c("variable", "outcome"),names_pattern = "^([^_]+)_(.+)$" ) |>
#     tidyr::pivot_wider(  names_from = variable, values_from = value )
# 
#   data1[,"pd80"] %>% sum(is.na(.))
# }
