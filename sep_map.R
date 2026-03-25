sep_df = readRDS("data/sep_data/pop_reg_sep_df.RDS") 
sep_df = sep_df %>% dplyr::select(plz,e_lv95,n_lv95,age,sex,ssep3,ssep3_d) %>% as.tibble()

plz_munid_df =read.csv("data/sep_data/PLZO_CSV_WGS84.csv",sep=";") %>% 
  dplyr::select(plz=PLZ,mun_id=BFS.Nr,E,N) %>% distinct() %>% 
  st_as_sf(coords = c("E", "N"), crs = 4326) %>%  # WGS84 lon/lat
  st_transform(crs = 2056) %>%                    # LV95
  dplyr::mutate(e_lv95_village = st_coordinates(.)[,1],
                n_lv95_village = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

#for each unique plz, bind a mun_id
plz_munid_df2 = sep_df %>% dplyr::select(plz) %>% distinct() %>% 
  left_join(plz_munid_df,by="plz") %>% 
  arrange(mun_id) %>% as.tibble()
if(FALSE){
  #Issue 1: some plz maps with multiple mun_id
  plz_munid_df2 %>% arrange(plz) %>% 
    group_by(plz) %>% dplyr::mutate(n=length(unique(mun_id))) %>% filter(n>1) 
  #Issue 2: some plz does not map with mun_id
  plz_munid_df2 %>% filter(is.na(mun_id))
}

plz_mult_munid = plz_munid_df2 %>% arrange(plz) %>% 
  group_by(plz) %>% dplyr::summarise(n=length(unique(mun_id))) %>% filter(n>1) %>% pull(plz) %>% unique()

sep_df1 = sep_df %>% filter(!(plz %in% plz_mult_munid))
sep_df2 = sep_df %>% filter(plz %in% plz_mult_munid) %>% dplyr::select(plz,e_lv95,n_lv95) %>% distinct() %>% 
  left_join(plz_munid_df,by="plz",relationship = "many-to-many") %>% 
  dplyr::mutate(dist = sqrt((e_lv95 - e_lv95_village)^2 + (n_lv95 - n_lv95_village)^2)) %>% 
  group_by(plz,e_lv95,n_lv95) %>% 
  slice_min(dist) %>% ungroup()
sep_df2 =  sep_df %>% filter(plz %in% plz_mult_munid) 

#Issue 2: find mun_id
plz_munid_missing_df <- data.frame(plz = c(3000, 8000, 6000, 2500, 4000, 7446, 1200),
                                   mun_id = c(351, 261, 1061, 371, 2701, 3681, 6621),
                                   e_lv95_village=NA,n_lv95_village=NA)

plz_munid_df2 = rbind(plz_munid_df2 %>% filter(!is.na(mun_id)),
                      plz_munid_missing_df)

sep_df3 = rbind(sep_df1 %>%  left_join(plz_munid_df2 %>% dplyr::select(plz,mun_id) %>% distinct(),by="plz"),
      sep_df %>% filter(plz %in% plz_mult_munid) %>% 
        left_join(sep_df2 %>% dplyr::select(plz,e_lv95,n_lv95,mun_id),by=c("plz","e_lv95","n_lv95")))

sep_df3 =sep_df3 %>% 
  group_by(mun_id) %>% 
  dplyr::summarise(ssep3_d_mean = mean(ssep3_d),.groups = "drop")

saveRDS(sep_df3,"savepoint/sep_df3.RDS")


sep_df3 = readRDS("savepoint/sep_df3.RDS")


sep_df3 %>% 
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
