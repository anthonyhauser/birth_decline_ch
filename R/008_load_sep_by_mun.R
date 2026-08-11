#Objective: assign a SEP for each mun_id
#Steps : 1) load SEP data for each individuals, load coordinate data for each municipality
#2) Assign a mun_id to each individual in  SEP data
#3) Combine
#4) Summarise SEP level by municipality

load_sep_by_mun = function(){
  #1) Load data
  #SEP data (individual level)
  sep_df = readRDS("data/sep_data/pop_reg_sep_df.RDS") 
  sep_df = sep_df %>% dplyr::select(plz,e_lv95,n_lv95,age,sex,ssep3,ssep3_d) %>% as.tibble()
  
  #Municipality data: coordinate for each municipality (some mun_id are present in multiple rows as some villages form the same municipality)
  plz_munid_df = read.csv("data/sep_data/PLZO_CSV_WGS84.csv",sep=";") %>% 
    dplyr::select(plz=PLZ,mun_id=BFS.Nr,E,N) %>% distinct() %>% 
    st_as_sf(coords = c("E", "N"), crs = 4326) %>%  # WGS84 lon/lat
    st_transform(crs = 2056) %>%                    # LV95
    dplyr::mutate(e_lv95_village = st_coordinates(.)[,1],
                  n_lv95_village = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
  #2) Assign a mun_id to each individual in  SEP data
  #join municipality (mun_id) data to SEP individual data using plz (i.e., for each unique plz, bind a mun_id)
  plz_munid_df2 = sep_df %>% dplyr::select(plz) %>% distinct() %>% 
    left_join(plz_munid_df,by="plz") %>% 
    arrange(mun_id) %>% as.tibble()
  #Problem: Some plz have multiple mun_id (plz shared over small municipalities), some plz are not present in municipality data
  if(FALSE){
    #Issue 1: some plz maps with multiple mun_id
    plz_munid_df2 %>% arrange(plz) %>% 
      group_by(plz) %>% dplyr::mutate(n=length(unique(mun_id))) %>% filter(n>1) 
    #Issue 2: some plz does not map with mun_id
    plz_munid_df2 %>% filter(is.na(mun_id))
  }
  #Issue 1: 
  #plz number with multiple mun_id
  plz_mult_munid = plz_munid_df2 %>% arrange(plz) %>% 
    group_by(plz) %>% dplyr::summarise(n=length(unique(mun_id))) %>% filter(n>1) %>% pull(plz) %>% unique()
  
  #data of plz that uniquely match with mun_id (or that don't match see Issue 2)
  sep_df1 = sep_df %>% filter(!(plz %in% plz_mult_munid))
  sep_df2 = sep_df %>% filter(plz %in% plz_mult_munid)
  #data of plz that match with multiple mun_id: take the mun_id with the shortest distance
  matching_sep_df2 = sep_df2 %>%
    dplyr::select(plz,e_lv95,n_lv95) %>% distinct() %>% 
    #for each SEP coordinate whose plz matches with multiple mun_id matches: select the mun_id with shortest distance
    left_join(plz_munid_df,by="plz",relationship = "many-to-many") %>% 
    dplyr::mutate(dist = sqrt((e_lv95 - e_lv95_village)^2 + (n_lv95 - n_lv95_village)^2)) %>% 
    dplyr::select(-c(e_lv95_village,n_lv95_village)) %>% 
    group_by(plz,e_lv95,n_lv95) %>% 
    slice_min(dist) %>% ungroup()
  
  #sep_df2 =  sep_df %>% filter(plz %in% plz_mult_munid) 
  
  #Issue 2: find mun_id manually
  plz_munid_missing_df <- data.frame(plz = c(3000, 8000, 6000, 2500, 4000, 7446, 1200),
                                     mun_id = c(351, 261, 1061, 371, 2701, 3681, 6621))
  matching_sep_df1 = rbind(plz_munid_df2 %>% filter(!(plz %in% plz_mult_munid),!is.na(mun_id)) %>% dplyr::select(plz,mun_id) %>% distinct(),
                           plz_munid_missing_df) 
  
  #3) Combine
  sep_df3 = rbind(sep_df1 %>%
                    left_join(matching_sep_df1,by="plz"),
                  sep_df2 %>% 
                    left_join(matching_sep_df2 %>% dplyr::select(-dist),by=c("plz","e_lv95","n_lv95")))
  
  #4) Summarise SEP level by municipality
  sep_df4 = sep_df3 %>% 
    group_by(mun_id) %>% 
    dplyr::summarise(ssep3_d_mean = mean(ssep3_d),.groups = "drop")
  
  if(FALSE){
    sep_df4 %>% 
      left_join(mun_sf %>% dplyr::mutate(mun_id=as.numeric(mun_id)), by = c("mun_id")) %>%
      st_as_sf() %>%
      ggplot(aes(fill = ssep3_d_mean)) +
      geom_sf(color = "black") +
      scale_fill_gradient2(
        name = "SEP",
        low = "red",        # negative
        mid = "white",      # zero
        high = "green",     # positive
        midpoint = 5)+
      theme( legend.position = "bottom",
             legend.direction = "horizontal")
    
  }
  
  saveRDS(sep_df4,"savepoint/sep_df4.RDS")
  return(sep_df4)
}
