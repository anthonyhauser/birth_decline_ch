tau = 5#in minutes
beta = 2 #exponent

function(tau=5, beta=2, lim=0){
  #0) load duration
  duration_df = load_duration_mobility_mun()
  
  #1) Load population by municipality
  pop_df <- read.csv("data/pop/STATPOP_municipality_2020.csv",
                     sep = ",",
                     fileEncoding = "Latin1",
                     stringsAsFactors = FALSE) %>% 
    as_tibble()
  colnames(pop_df) = c("year","reg_name","resident","citizenship","sex","n")
  
  #the number corresponds the population at the 31 Dec of the year for a given age, we thus need to add 1 to the year and to the age to get population of the next age for age +1
  pop_mun_df = pop %>% 
    dplyr::select(-c(year,resident,citizenship,sex)) %>%  #remove variables that are not informative as only one value included
    dplyr::mutate(reg_agg_level = case_when(
      reg_name == "Suisse"           ~ "national",
      str_starts(reg_name, "- ")      ~ "canton",
      str_starts(reg_name, ">> ")     ~ "district",
      str_starts(reg_name, "......")  ~ "municipality",
      TRUE                            ~ NA),
      reg_name = reg_name %>%
        str_remove("^(- |>> |\\.\\.\\.\\.+)") %>%
        str_remove("^\\d+\\s+")) %>% 
    filter(reg_name!="Sans indication") %>% 
    filter(reg_agg_level=="municipality") %>% dplyr::select(-reg_agg_level) %>% 
    dplyr::mutate(n=as.numeric(n))
  
  #2) add population size and reg_id
  duration_df2 = duration_df %>% 
    #add population size for each municipality
    left_join(pop_mun_df %>% dplyr::select(src_municipality_name = reg_name,src_pop = n),by="src_municipality_name") %>% 
    left_join(pop_mun_df %>% dplyr::select(dst_municipality_name = reg_name,dst_pop = n),by="dst_municipality_name") %>% 
    #remove mun with missing pop (due to municipality grouping from 2020)
    filter(!is.na(src_pop), !is.na(dst_pop)) %>% 
    #add reg_id
    left_join(municipality_region_df %>% dplyr::select(src_municipality_name=municipality_name,src_reg_id=reg_id),by="src_municipality_name") %>% 
    left_join(municipality_region_df %>% dplyr::select(dst_municipality_name=municipality_name,dst_reg_id=reg_id),by="dst_municipality_name")
  
  #3) summarise by reg_id
  cm_mun_df = duration_df2 %>% 
    #calculate kernel for each pair of municipality
    dplyr::mutate(k = src_pop * dst_pop /(1+duration_min/tau)^beta) %>% 
    #normalize to 1 to have probability of visit for each src and dest municipality
    group_by(src_municipality_name) %>%
    dplyr::mutate(prob = k/sum(k)) %>% ungroup() 
  cm_region_df = cm_mun_df %>% 
    #calculate probability aggregated by src region for each src municipality
    group_by(src_municipality_name, src_reg_id,dst_reg_id) %>%
    dplyr::summarise(prob = sum(prob),
                     src_pop = src_pop[1],.groups="drop") %>% 
    #weighted average of p by src region
    group_by(src_reg_id, dst_reg_id) %>% 
    dplyr::summarise(p = sum(prob * src_pop)/sum(src_pop),.groups="drop") %>% 
    #add region name
    left_join(municipality_region_df %>% dplyr::select(src_reg_name=reg_name ,src_reg_id=reg_id) %>% distinct(),by="src_reg_id") %>% 
    left_join(municipality_region_df %>% dplyr::select(dst_reg_name=reg_name ,dst_reg_id=reg_id) %>% distinct(),by="dst_reg_id") %>% 
    dplyr::rename(part_name = src_reg_name, part_id = src_reg_id,
                  cnt_name = dst_reg_name, cnt_id = dst_reg_id,value=p)
  
  #assign 0 to elements with small values (in both m[i,j] and m[j,i] to guarantee reciprocity) to create sparse matrix (if not needed, set lim=0)
  cm_region_df = cm_region_df %>% 
    left_join(cm_region_df %>% dplyr::select(t_value = value, part_name,cnt_name),by=c("part_name"="cnt_name","cnt_name"="part_name")) %>% 
    dplyr::mutate(value_nonsparse=value,
                  value = if_else(value<lim & t_value<lim, 0, value)) %>% dplyr::select(-t_value)
  
  
  if(FALSE){
    cm_mun_df %>% filter(src_municipality_name=="Moutier") %>% arrange(-prob)
    cm_region_df %>% filter(part_id==cnt_id) %>% 
      ggplot(aes(x=part_name,y=value)) +geom_point()+
      scale_y_continuous(limits = c(0,1),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title = element_blank())
    
    
    ggplot(cm_region_df, aes(x = part_name, y = cnt_name, fill = value)) +
      geom_tile() +
      geom_text(data = cm_region_df %>%  filter(value > 0),
                aes(label = scales::percent(value, accuracy = 1)),
                color = "black",
                size = 3) +
      scale_fill_gradient(name = "Proportion of contacts",low = "white", high = "orange",label=scales::percent,limits=c(0,1)) +
      theme_bw(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title = element_blank())
  }
  
  
  return(reg_cm_df)
}