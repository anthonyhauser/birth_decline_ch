excess_birth_year_reg_df  = pred_n_birth_reg_draw_df %>% 
  left_join(rural_urban_dist_df %>% dplyr::select(dist_id,urban_rural_type2),by="dist_id")
excess_df = excess_birth_year_reg_df %>% filter(year %in% 2024) %>% 
  dplyr::mutate(urban_rural_id = as.numeric(urban_rural_type2<12)) %>% 
  #sum over age and month
  group_by(urban_rural_id,draw) %>% 
  dplyr::summarise(n_pop = sum(n_pop),
                   n_birth = sum(n_birth),
                   n_pred = sum(n_pred),.groups="drop_last") %>% 
  dplyr::summarise(n_pop = n_pop[1],
                   n_birth = n_birth[1],
                   n_exp_mean = mean(n_pred),
                   n_exp_lwb = quantile(n_pred,probs=0.025),
                   n_exp_upb = quantile(n_pred,probs=0.9725),.groups="drop") %>% 
  dplyr::mutate(n_exc_mean = n_birth - n_exp_mean,
                n_exc_lwb = n_birth- n_exp_lwb,
                n_exc_upb = n_birth-n_exp_upb,
                rel_exc_mean = (n_birth - n_exp_mean)/n_exp_mean,
                rel_exc_lwb = (n_birth- n_exp_lwb)/n_exp_mean,
                rel_exc_upb = (n_birth-n_exp_upb)/n_exp_mean)


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

