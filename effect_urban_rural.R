#excess mortality by district
#join with rural/urban 
excess_birth_year_reg_df  = pred_n_birth_reg_draw_df %>% 
  left_join(rural_urban_dist_df %>% dplyr::select(dist_id,urban_rural_type2),by="dist_id") %>% 
  dplyr::mutate(n_exc = n_birth - n_pred,
                urban_rural_id = as.numeric(urban_rural_type2<12)) %>% 
  filter(year %in% 2024)

#excess
var_group=c("urban_rural_id") #var_group=c("mun_id","mun_name")
excess_df = get_excess_est(var_group, excess_birth_year_reg_df)
excess_df
excess_df = get_excess_est(var_group, excess_birth_year_reg_df %>% 
                 dplyr::mutate(urban_rural_id = floor(urban_rural_type2)))
excess_df

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
plot_df %>% 
  ggplot(aes(x = urban_rural_id, y = mean))+
  geom_ribbon(aes(ymin = lwb,ymax=upb),alpha = 0.2) +
  geom_line() +
  geom_hline(data = subset(plot_df, var %in% c("Excess birth", "Relative excess")),
             aes(yintercept = 0),
             linetype = 2) +
  geom_line(data = subset(plot_df,var %in% c("Expected birth")),
            aes(y=n_birth),col="darkred",
            linetype = 2 ) +
  facet_grid(var~.,scale="free_y")


################################################################################################################################################
#municipality
#pop data
pop_mun_df2 = pop_mun_df %>% filter(month==1) %>% 
  group_by(mun_id,year) %>% dplyr::summarise(n_pop=sum(n),.groups="drop")

total_pop = sum(pop_mun_df2 %>% filter(year==2024) %>% pull(n_pop), na.rm = TRUE)
pop_mun_df2 = pop_mun_df2 %>% filter(year==2024) %>% 
  arrange(n_pop) %>% 
  dplyr::mutate(cum_pop = cumsum(n_pop),
                n_pop_ntile = cut(cum_pop,breaks = seq(0, total_pop, length.out = 11),labels = FALSE,  include.lowest = TRUE )) %>% 
  dplyr::select(-cum_pop)

pop_mun_df2 %>% 
  group_by(n_pop_ntile) %>% 
  dplyr::summarise(n_mun =n(),
                   min_pop =  min(n_pop),
                   max_pop =  max(n_pop),
                   n_pop=sum(n_pop))

#join with rural/urban 
excess_birth_year_reg_df = excess_birth_year_mun_draw_df %>% 
  left_join(pop_mun_df2 %>% dplyr::select(-year), by=c("mun_id")) %>% 
  left_join(rural_urban_df %>% dplyr::select(mun_id,urban_rural_type2),by="mun_id") %>% 
  left_join(pop_dens_df %>% pivot_wider(id_cols = mun_id,names_from = "variable",values_from = "value"),by="mun_id") %>% 
  filter(year %in% 2020:2024) 

#################################################################################
df = excess_birth_year_reg_df %>% 
  group_by(year,mun_id,mun_name,
           n_pop, urban_rural_type2, pop_dens, pop_dens_building, pop_dens_tot, prop_building, prop_individual_houses, prop_infrastructure) %>% 
  dplyr::summarise(n_pred_mean = mean(n_pred),
                   n_exc_mean = mean(n_exc),
                   rel_exc_mean = n_exc_mean/n_pred_mean, .groups="drop") %>% 
  filter(year==2024) %>% 
  filter(n_pop>500)

library(rpart)

vars = c("n_pop","pop_dens","pop_dens_building","pop_dens_tot",
         "prop_building","prop_individual_houses",
         "prop_infrastructure","urban_rural_type2")

results = lapply(vars, function(v) {
  
  formula = as.formula(
    paste0("rel_exc_mean ~ ",
           ifelse(v == "urban_rural_type2",
                  "factor(urban_rural_type2)", v))
  )
  
  fit = rpart(formula, data = df, cp = 0.001)
  
  data.frame(
    variable = v,
    nsplit = fit$cptable[which.min(fit$cptable[,"xerror"]),"nsplit"],
    xerror = min(fit$cptable[,"xerror"])
  )
  
}) %>% bind_rows() %>% arrange(xerror)

results

results = lapply(vars, function(v) {
  
  df_tmp = df %>% 
    mutate(group = ntile(.data[[v]], 20))
  
  fit = lm(rel_exc_mean ~ factor(group), data = df_tmp)
  
  data.frame(
    variable = v,
    r2 = summary(fit)$r.squared
  )
  
}) %>% bind_rows() %>% arrange(desc(r2))

results



excess_birth_year_reg_df %>% 
  filter(year==202,n_pop>200)  %>% 
  group_by(mun_id,n_pop,urban_rural_type2,pop_dens) %>% 
  dplyr::summarise(rel_exc_mean = mean(n_exc)/mean(n_pred)) %>% 
  ggplot(aes(x=log(n_pop),y=rel_exc_mean,col=log(pop_dens)))+
  geom_point()

excess_birth_year_reg_df %>% 
  dplyr::select(mun_id,mun_name,pop_dens,urban_rural_type2,n_pop) %>% distinct() %>% View()

#excess
var_group=c("urban_rural_id") #var_group=c("mun_id","mun_name")

dens_var = c("n_pop","pop_dens","pop_dens_building","pop_dens_tot",
             "prop_building","prop_individual_houses",
             "prop_infrastructure","urban_rural_type2")

df = excess_birth_year_mun_draw_df %>% 
  left_join(pop_dens_df %>% variable
              dplyr::select(mun_id,variable,value),by="mun_id")

###################################################################################################################################################

pop_dens_df %>% filter(mun_id==31010)


df_var = excess_birth_year_mun_draw_df %>% dplyr::select(mun_id,mun_name) %>% distinct() %>% 
  left_join(pop_mun_df2 %>% filter(year==2024,month==1) %>% 
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

vars <- setdiff(names(df_var), c("urban_rural_type2", "mun_id", "mun_name"))

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
for(v in paste0(c("urban_rural_type2",vars), "_ntile")){
  excess_df[[v]] = get_excess_est(var_group=c("ntile"),
                                  excess_birth_year_mun_draw_df %>% 
                                    filter(year %in% 2017:2024) %>% 
                                    left_join(df_var %>% dplyr::select(mun_id,ntile = !!v),by="mun_id")) %>% 
    dplyr::mutate(explanatory_var=!!v)
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
excess_df %>% 
  filter(explanatory_var %in% c("n_pop_ntile","pop_dens_building_ntile", "prop_building_ntile", "prop_individual_houses_ntile","sep_ntile","urban_rural_type2_ntile")) %>% 
  ggplot(aes(x = ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
  geom_ribbon(aes(),alpha = 0.2) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_y_continuous(name="Relative excess birth",labels = scales::percent)+
  facet_wrap(explanatory_var~.,ncol=2)



#plot
plot_df = excess_df %>% 
  filter(explanatory_var=="n_pop_ntile") %>% 
  pivot_longer(cols = c(n_exp_mean, n_exp_lwb, n_exp_upb,
                        n_exc_mean, n_exc_lwb, n_exc_upb,
                        rel_exc_mean, rel_exc_lwb, rel_exc_upb),
               names_to = c("var", "stat"),
               names_pattern = "(.*)_(mean|lwb|upb)",
               values_to = "value" ) %>% 
  pivot_wider(names_from = "stat",values_from = "value") %>% 
  dplyr::mutate(var = factor(var,levels=c("n_exp","n_exc","rel_exc"),labels=c("Expected birth","Excess birth","Relative excess")))
plot_df %>% 
  ggplot(aes(x = ntile, y = mean))+
  geom_ribbon(aes(ymin = lwb,ymax=upb),alpha = 0.2) +
  geom_line() +
  geom_point() +
  geom_hline(data = subset(plot_df, var %in% c("Excess birth", "Relative excess")),
             aes(yintercept = 0),
             linetype = 2) +
  geom_line(data = subset(plot_df,var %in% c("Expected birth")),
            aes(y=n_birth),col="darkred",
            linetype = 2 ) +
  facet_grid(var~explanatory_var,scale="free_y")









