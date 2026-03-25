
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
