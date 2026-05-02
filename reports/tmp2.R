new_pop_mun_df = pop_mun_df %>% 
  dplyr::mutate(mun_name = if_else(mun_id %in% mun_ids_to_agg, "Appenzell (aggregated)",mun_name),
                mun_id = if_else(mun_id %in% mun_ids_to_agg, 31010,mun_id))

excess_birth_year_mun_draw_df = readRDS(paste0(code_root_path,"results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS")) %>% 
  dplyr::mutate(childless = FALSE)
if(filter_parity!="all"){
  excess_birth_year_mun_draw_df = rbind(excess_birth_year_mun_draw_df,
                                        readRDS(paste0(code_root_path,"results/",save.date,"_",mod_name,"_childless","_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS")) %>% 
                                          dplyr::mutate(childless = TRUE))
}


df_var = excess_birth_year_mun_draw_df %>% dplyr::select(mun_id,mun_name) %>% distinct() %>% 
  left_join(new_pop_mun_df %>% filter(year==2024,month==1) %>% 
              group_by(mun_id) %>% dplyr::summarise(n_pop = sum(n),.groups="drop"), by=c("mun_id")) %>% 
  left_join(rural_urban_df %>% dplyr::select(mun_id,urban_rural_type2),by="mun_id") %>% 
  left_join(pop_dens_df %>% pivot_wider(id_cols = mun_id,names_from = "variable",values_from = "value"),by="mun_id") %>% 
  left_join(sep_df3 %>% dplyr::select(mun_id,sep=ssep3_d_mean),by="mun_id")


make_pop_ntile <- function(data, var, n = 5) {
  total_pop = sum(data$n_pop)
  data %>%
    arrange({{ var }}) %>%
    dplyr::mutate(cum_pop = cumsum(n_pop),
                  group = cut(cum_pop,breaks = seq(0, total_pop, length.out = n+1),labels = FALSE,  include.lowest = TRUE )) %>% 
    dplyr::select(-cum_pop) %>% 
    arrange(mun_id) %>%
    pull(group)
}

vars <- c("n_pop", "pop_dens_building")

for (v in vars) {
  df_var[[paste0(v, "_ntile")]] <-make_pop_ntile(df_var, !!rlang::sym(v), 10)
}
df_var$urban_rural_type2_ntile = dense_rank(df_var$urban_rural_type2)

if(FALSE){
  #check that the number of pop is more or less the same between groups (except for urban_rural_type2)
  df_var %>% group_by(n_pop_ntile) %>% 
    dplyr::summarise(n_pop = sum(n_pop),
                     n=n())
}

#aggregate excess by group
excess_df = list()
for(v in paste0(c(vars), "_ntile")){
  for(w in unique(excess_birth_year_mun_draw_df$childless)){
    excess_df[[paste0(v,"_",w)]] = get_excess_est(var_group=c("ntile"),
                                                  excess_birth_year_mun_draw_df %>% 
                                                    filter(year %in% 2017:2024,
                                                           childless==w) %>% 
                                                    left_join(df_var %>% dplyr::select(mun_id,ntile = !!v),by="mun_id")) %>% 
      dplyr::mutate(explanatory_var=!!v,
                    childless = !!w)
  }
}
excess_df = rbindlist(excess_df)

#plot
plot_df = excess_df %>% 
  pivot_longer(cols = c(n_exp_mean, n_exp_lwb, n_exp_upb,
                        n_exc_mean, n_exc_lwb, n_exc_upb,
                        rel_exc_mean, rel_exc_lwb, rel_exc_upb),
               names_to = c("var", "stat"),
               names_pattern = "(.*)_(mean|lwb|upb)",
               values_to = "value" ) %>% 
  pivot_wider(names_from = "stat",values_from = "value") %>% 
  dplyr::mutate(var = factor(var,levels=c("n_exp","n_exc","rel_exc"),labels=c("Expected birth","Excess birth","Relative excess")))

saveRDS(excess_df,file="excess_df_parity.RDS")

readRDS(paste0(code_root_path,"excess_df_parity.RDS")) %>% 
  ggplot(aes(x = ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
  geom_ribbon(aes(fill=childless),alpha = 0.2) +
  geom_line(aes(col=childless)) +
  geom_point(aes(col=childless)) +
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_y_continuous(name="Relative excess birth",labels = scales::percent)+
  facet_wrap(explanatory_var~.,ncol=2)











mod_name=paste0("mod8_","first")
mod_name=paste0("mod8_","second")
mod_name=paste0("mod8_","swiss")
mod_name=paste0("mod8_","non-swiss")

save.date="20260309"
filter_parity="all"




new_pop_mun_df = pop_mun_df %>% 
  dplyr::mutate(mun_name = if_else(mun_id %in% mun_ids_to_agg, "Appenzell (aggregated)",mun_name),
                mun_id = if_else(mun_id %in% mun_ids_to_agg, 31010,mun_id))

excess_birth_year_mun_draw_df = readRDS(paste0(code_root_path,"results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS")) %>% 
  dplyr::mutate(childless = FALSE)
if(filter_parity!="all"){
  excess_birth_year_mun_draw_df = rbind(excess_birth_year_mun_draw_df,
                                        readRDS(paste0(code_root_path,"results/",save.date,"_",mod_name,"_childless","_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS")) %>% 
                                          dplyr::mutate(childless = TRUE))
}


df_var = excess_birth_year_mun_draw_df %>% dplyr::select(mun_id,mun_name) %>% distinct() %>% 
  left_join(new_pop_mun_df %>% filter(year==2024,month==1) %>% 
              group_by(mun_id) %>% dplyr::summarise(n_pop = sum(n),.groups="drop"), by=c("mun_id")) %>% 
  left_join(rural_urban_df %>% dplyr::select(mun_id,urban_rural_type2),by="mun_id") %>% 
  left_join(pop_dens_df %>% pivot_wider(id_cols = mun_id,names_from = "variable",values_from = "value"),by="mun_id") %>% 
  left_join(sep_df3 %>% dplyr::select(mun_id,sep=ssep3_d_mean),by="mun_id")


make_pop_ntile <- function(data, var, n = 5) {
  total_pop = sum(data$n_pop)
  data %>%
    arrange({{ var }}) %>%
    dplyr::mutate(cum_pop = cumsum(n_pop),
                  group = cut(cum_pop,breaks = seq(0, total_pop, length.out = n+1),labels = FALSE,  include.lowest = TRUE )) %>% 
    dplyr::select(-cum_pop) %>% 
    arrange(mun_id) %>%
    pull(group)
}

 vars <- c("n_pop", "pop_dens_building")

for (v in vars) {
  df_var[[paste0(v, "_ntile")]] <-make_pop_ntile(df_var, !!rlang::sym(v), 10)
}
df_var$urban_rural_type2_ntile = dense_rank(df_var$urban_rural_type2)

if(FALSE){
  #check that the number of pop is more or less the same between groups (except for urban_rural_type2)
  df_var %>% group_by(n_pop_ntile) %>% 
    dplyr::summarise(n_pop = sum(n_pop),
                     n=n())
}

#aggregate excess by group
excess_df = list()
for(v in paste0(c(vars), "_ntile")){
  for(w in unique(excess_birth_year_mun_draw_df$childless)){
    excess_df[[paste0(v,"_",w)]] = get_excess_est(var_group=c("ntile"),
                                                  excess_birth_year_mun_draw_df %>% 
                                                    filter(year %in% 2017:2024,
                                                           childless==w) %>% 
                                                    left_join(df_var %>% dplyr::select(mun_id,ntile = !!v),by="mun_id")) %>% 
      dplyr::mutate(explanatory_var=!!v,
                    childless = !!w)
  }
}
excess_df = rbindlist(excess_df)

#plot
plot_df = excess_df %>% 
  pivot_longer(cols = c(n_exp_mean, n_exp_lwb, n_exp_upb,
                        n_exc_mean, n_exc_lwb, n_exc_upb,
                        rel_exc_mean, rel_exc_lwb, rel_exc_upb),
               names_to = c("var", "stat"),
               names_pattern = "(.*)_(mean|lwb|upb)",
               values_to = "value" ) %>% 
  pivot_wider(names_from = "stat",values_from = "value") %>% 
  dplyr::mutate(var = factor(var,levels=c("n_exp","n_exc","rel_exc"),labels=c("Expected birth","Excess birth","Relative excess")))

saveRDS(excess_df,file=paste0("excess_df_",mod_name,".RDS"))

readRDS(paste0(code_root_path,paste0("excess_df_",mod_name,".RDS"))) %>% 
  ggplot(aes(x = ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
  geom_ribbon(aes(fill=childless),alpha = 0.2) +
  geom_line(aes(col=childless)) +
  geom_point(aes(col=childless)) +
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_y_continuous(name="Relative excess birth",labels = scales::percent)+
  facet_wrap(explanatory_var~.,ncol=2)

