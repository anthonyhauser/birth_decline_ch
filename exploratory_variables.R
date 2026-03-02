
excess_birth_adj_mun_draw_df %>% 
  left_join(rural_urban_df %>% dplyr::select(mun_id,urban_rural_type2) %>% 
              dplyr::mutate(urban_rural_type2 =  dense_rank(urban_rural_type2)),
                            by="mun_id") %>% 
  group_by(draw) %>% 
  dplyr::summarise(cor = cor(adj_n_exc,urban_rural_type2))



excess_birth_adj_mun_df %>% 
  left_join(rural_urban_df %>% dplyr::select(mun_id,urban_rural_type2) %>% 
              dplyr::mutate(urban_rural_type2 =  dense_rank(urban_rural_type2)),
            by="mun_id") %>% #filter(urban_rural_type2==1) %>% View()
  ggplot(aes(x=urban_rural_type2,y=rel_exc_mean))+
  geom_point()

  
  
  
excess_birth_adj_mun_df %>% 
  dplyr::mutate(rel_exc_mean = case_when(rel_exc_lwb >0 | rel_exc_upb<0 ~ rel_exc_mean ,
                                         TRUE ~ 0)) %>% 
  left_join(mun_df %>% dplyr::select(mun_id,dist_id) %>% distinct(), by="mun_id") %>% 
  #left_join(regions_sf %>% dplyr::mutate(dist_id=as.numeric(dist_id)), by = c("dist_id")) %>%
  left_join(mun_sf %>% dplyr::mutate(mun_id=as.numeric(mun_id)), by = c("mun_id")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = rel_exc_mean),color = "black",size = 0.1) +
  geom_sf(data=lake_sf,fill = "lightblue",color=NA,alpha=0.5) +
  scale_fill_gradient2(
    name = "Relative excess birth",
    low = "red",        # negative
    mid = "white",      # zero
    high = "green",     # positive
    midpoint = 0,
    labels = scales::percent_format(accuracy = 1)
  )+
  theme( legend.position = "bottom",
         legend.direction = "horizontal")

  
rural_urban_df %>% 
  left_join(mun_sf %>% dplyr::mutate(mun_id=as.numeric(mun_id)), by = c("mun_id")) %>%
  dplyr::mutate(urban_rural_type2 =  dense_rank(urban_rural_type2)) %>% 
  dplyr::mutate(urban_rural_type2 = case_when(urban_rural_type2==1 ~ "1",
                                              urban_rural_type2==2 ~ "2",
                                              urban_rural_type2==3 ~ "3",
                                              TRUE ~ ">3")) %>% 
  st_as_sf() %>%
  ggplot(aes(fill = urban_rural_type2)) +
  geom_sf(color = "black") +
  scale_fill_manual(values=c("white","darkred","red","violet"))+
  theme( legend.position = "bottom",
         legend.direction = "horizontal")


pop_dens_df %>% 
  left_join(mun_sf %>% dplyr::mutate(mun_id=as.numeric(mun_id)), by = c("mun_id")) %>%
  st_as_sf() %>%
  ggplot(aes(fill = pop_dens)) +
  geom_sf(color = "black") +
  scale_fill_gradient2(name = "Relative excess birth",
                      low = "green",        # negative
                      mid = "white",      # zero
                      high = "red",     # positive
                      scale="log",
                      midpoint = pop_dens_df$pop_dens %>% mean())+
  theme( legend.position = "bottom",
         legend.direction = "horizontal")


excess_birth_adj_mun_df %>% 
  left_join(pop_dens_df %>% dplyr::select(mun_id,pop_dens),by="mun_id") %>% 
  mutate(pop_dens_ntile = ntile(pop_dens, 20)) %>% 
  group_by(pop_dens_ntile) %>% 
  dplyr::summarise(mean(rel_exc_mean))

excess_birth_year_adj_mun_df %>% filter(year==2024) %>% 
  left_join(pop_dens_df %>% dplyr::select(mun_id,pop_dens),by="mun_id") %>% 
  mutate(pop_dens_ntile = ntile(pop_dens, 20)) %>% 
  group_by(pop_dens_ntile) %>% 
  dplyr::summarise(mean(rel_exc_mean))

excess_birth_year_adj_mun_df %>% filter(year==2024) %>% 
  left_join(pop_dens_df %>% dplyr::select(mun_id,pop_dens),by="mun_id") %>% 
  filter(pop_dens>5000)

excess_birth_adj_mun_df %>% 
  left_join(pop_dens_df %>% dplyr::select(mun_id,pop_dens),by="mun_id") %>% 
  mutate(pop_dens_ntile = ntile(pop_dens, 20)) %>% 
  filter(pop_dens_ntile==20) %>% View()

excess_birth_adj_mun_df %>% 
  left_join(pop_dens_df %>% dplyr::select(mun_id,pop_dens),by="mun_id") %>% 
  mutate(pop_dens_decile = ntile(pop_dens, 10)) %>% 
  ggplot(aes(x=pop_dens_decile,y=rel_exc_mean))+
  geom_point()











excess_birth_year_reg_df %>% 
  filter(year==2024) %>% 
  left_join(rural_urban_dist_df) %>% 
  dplyr::mutate(urban_rural_df)
