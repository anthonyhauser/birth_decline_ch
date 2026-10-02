source("R/000_setup.R")

res_path = "results/2025/"
save.date="20260625"
save.date2="20260625"
mod_name = "mod8"
seed_id = 1
year_suffix       = "_2025"
ntile_year_suffix = "_2017_2025"
mod_name_full     = paste0(mod_name,             year_suffix)
mod_name_swiss    = paste0(mod_name, "_swiss",     year_suffix)
mod_name_nonswiss = paste0(mod_name, "_non-swiss", year_suffix)
mod_name_first    = paste0(mod_name, "_first",     year_suffix)
mod_name_second   = paste0(mod_name, "_second",    year_suffix)

################################################################################
#-------------------------------------------------------------------------------
#Slope
slope_excess_birth_adj2_mun_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,"_","slope_excess_birth_year_adj2_mun_df",".RDS"))
slope_excess_birth_adj2_mun_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_swiss,"_","seedid",seed_id,"_","slope_excess_birth_year_adj2_mun_df",".RDS"))
slope_excess_birth_mun_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,"_","slope_excess_birth_year_mun_df",".RDS"))
fig3 = slope_excess_birth_adj2_mun_df %>% 
  #left_join(new_mun_df %>% dplyr::select(mun_id,dist_id) %>% distinct(), by="mun_id") %>% 
  left_join(new_mun_sf %>% dplyr::mutate(mun_id=as.numeric(mun_id)), by = c("mun_id")) %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = slope_rel_exc_mean),color=NA)+
  # geom_sf(aes(fill = if_else(slope_rel_exc_lwb > 0 | slope_rel_exc_upb < 0,
  #                            slope_rel_exc_mean, NA_real_)), color = NA) +  # communes sans bordure
  geom_sf(data = regions_sf %>% mutate(dist_id = as.numeric(dist_id)), 
          fill = NA, color = "black", size = 0.3) +  # contours districts
  geom_sf(data = lake_sf, fill = "lightblue", color = NA, alpha = 0.5) +
  scale_fill_gradient2(
    name = "Change in relative excess birth",
    low = "red",
    mid = "lightyellow",
    high = "green",
    midpoint = 0,#median(slope_excess_birth_mun_df$slope_rel_exc_mean),
    labels = scales::percent_format(accuracy = 1),
    na.value = "white"
    #limits=c(-0.4,0.4)
  ) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
fig3

#-------------------------------------------------------------------------------
#ICF
load("savepoint/cleaned2025_df.RData")

birth_df1 = birth_agg_df %>% 
  filter(mother_age %in% 15:50,year>=2000) %>% 
  dplyr::rename(age=mother_age) %>% 
  group_by(year,age) %>% 
  dplyr::summarise(n=sum(n), .groups="drop")


pop_df1 = pop_df %>% 
  filter(age %in% 15:50,year>=2000,month==1) %>% 
  group_by(year,age) %>% 
  dplyr::summarise(n=sum(n), .groups="drop")

fig1 = birth_df1 %>% 
  rename(n_birth = n) %>% 
  left_join(pop_df1 %>% rename(n_pop = n), by = c("year", "age")) %>% 
  group_by(year) %>% 
  dplyr::summarise(icf = sum(n_birth / n_pop), .groups = "drop") %>% 
  dplyr::mutate(year = dmy(paste0("01-01-", year))) %>% 
  ggplot(aes(x = year, y = icf)) + 
  geom_line(linewidth = 1) +  # Correction : suppression de la virgule en trop
  geom_point() + 
  scale_x_date( name = "",
    breaks = as.Date(paste0(seq(2000, 2025, by = 5), "-01-01")), # Correction : breaks au format Date
    date_labels = "%Y") + 
  scale_y_continuous(name = "Indice conjoncturel de fécondité (ICF)",limits=c(1,1.8)) + 
  theme(legend.position = "bottom")

ggsave(filename = paste0(code_root_path,"slides/images/icf_switzerland_2020_2025.png"),
       plot = fig1,
       width = 7, height = 4,units = "in", dpi = 600)

#-------------------------------------------------------------------------------
#Correction for proportion of women susceptible of having children by municipality
#load data 
pop_birth_mun_df2 = readRDS(paste0(code_root_path,"savepoint/","p_childless_df.RDS"))

# Communes > 10'000
mun_col <- pop_birth_mun_df2 %>% 
filter(n_pop_2024 > 20000) %>% 
distinct(mun_name, n_pop_2024) %>% 
arrange(n_pop_2024)

# Palette rose → rouge
pal <- col_numeric( palette = c("violet", "darkred"),
                    domain = range(mun_col$n_pop_2024) )

mun_col <- mun_col %>% 
  mutate(colour = pal(n_pop_2024))

fig2 = pop_birth_mun_df2 %>% 
  ggplot(aes(x = age, y = p_childless_pos2, group = mun_name)) +
  geom_line(data = ~ filter(.x, n_pop_2024 <= 20000),
            colour = "lightgray",
            alpha = 0.1 ) +
  geom_line( data = ~ filter(.x, n_pop_2024 > 20000),
             aes(colour = mun_name),
             alpha = 1) +
  scale_x_continuous(name="Age")+
  scale_y_continuous(labels=scales::percent)+
  labs(colour = "Commune",y="Proportion de femmes sans enfant")
fig2

ggsave(filename = paste0(code_root_path,"slides/images/prop_childless.png"),
       plot = fig2,
       width = 8, height = 4,units = "in", dpi = 600)


################################################################################
#Figure 1
gamma_month_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,"_","gammamonth.RDS")) %>% 
  dplyr::mutate(month_abb = factor(month_id, levels = 1:12, labels = month.abb))
excess_birth_year_month_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,"_","excess_birth_year_month_df",".RDS"))
birth_prob_by_age_df=  readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_seedid",seed_id,"_birthprob.RDS"))
gp_rel_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_seedid",seed_id,"_gp_rel.RDS"))

p1 = excess_birth_year_month_df %>% 
  ggplot(aes(x=date))+
  geom_ribbon(aes(ymin=n_exp_lwb, ymax=n_exp_upb),alpha=0.2)+
  geom_line(aes(y=n_exp_mean)) +
  geom_point(aes(y=n_birth), alpha=0.6,col="darkred")+
  scale_x_date(name="", expand = c(0.01, 0.01),limits=dmy(c("01-01-2000",paste0("01-01-",last_year+1))),
               breaks = seq(as.Date("2000-01-01"), as.Date(paste0(last_year, "-01-01")),by = "5 years"),
               date_labels = "%Y")+
  scale_y_continuous(name="Monthly births")

p2 = gamma_month_df %>% 
  dplyr::mutate(month_abb = factor(month_id, levels = 1:12, labels = month.abb)) %>% 
  mutate(days = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
         adjustment = 31 / days,
         across(c(est, lwb, upb), ~ (.x+1) * adjustment -1)) %>% 
  ggplot(aes(x = month_id,
             y = est,ymin = lwb,ymax = upb)) +
  geom_ribbon(fill = "blue", alpha = 0.2) +
  geom_line(col = "blue", group = 1) +
  scale_x_continuous(name="",breaks=1:12,labels=gamma_month_df$month_abb)+
  scale_y_continuous(name = "Relative effect on birth (vs Jan)",labels=scales::percent)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#fertility
p3 = birth_prob_by_age_df %>% 
  filter(year %in% c(2000, 2010, last_year)) %>%
  ggplot(aes(x = mother_age, y = est, ymin = lwb, ymax = upb)) +
  geom_ribbon(aes(fill = factor(year)), alpha = 0.2) +
  geom_line(aes(col = factor(year))) +
  scale_x_continuous(name = "Maternal age") +
  scale_y_continuous( name = "Probability of having a child\n(by month)",
    labels = scales::percent) +
  scale_color_discrete(name = "") +
  scale_fill_discrete(name = "") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1))


#calculate peak age
df <- birth_prob_by_age_df %>% filter(year==2000) %>%  arrange(mother_age)
idx <- which.max(df$est)
y1 <- df$est[idx - 1]
y2 <- df$est[idx]
y3 <- df$est[idx + 1]
x0 <- df$mother_age[idx]
delta <- (y1 - y3) / (2 * (y1 - 2 * y2 + y3))
peak_age <- x0 + delta

p4 = gp_rel_df %>% 
  dplyr::mutate(est = est + peak_age,
                lwb = lwb + peak_age,
                upb = upb + peak_age) %>% 
  ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="black",alpha=0.2)+
  geom_line(col="black")+
  scale_y_continuous(name="Age of fertility peak")+
  scale_x_continuous(name="Calendar year")+
  theme(plot.margin = margin(5.5, 9, 5.5, 5.5))

fig1 = cowplot::plot_grid(p1,
                          cowplot::plot_grid(p2,p3,p4,rel_widths = c(1,1,1),ncol=3,labels = c("B.","C.","D")),
                          rel_heights = c(1.4,1),labels = c("A.",""),
                          nrow = 2)
fig1
ggsave(filename = paste0(code_root_path,"slides/images/fig1.png"),
       plot = fig1,
       width = 10, height = 8,units = "in", dpi = 600)



################################################################################
#Figure 2a
#load data
excess_birth_year_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,"_","excess_birth_year_df",".RDS"))
excess_birth_ctzreg_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,"_","excess_birth_ctzreg_df",".RDS"))
excess_birth_year_ctzreg_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,"_","excess_birth_year_ctzreg_df",".RDS"))


#national, year
plot_df1 = excess_birth_year_df %>% 
  pivot_longer(cols = c(n_exp_mean, n_exp_lwb, n_exp_upb,
                        n_exc_mean, n_exc_lwb, n_exc_upb,
                        rel_exc_mean, rel_exc_lwb, rel_exc_upb),
               names_to = c("var", "stat"),
               names_pattern = "(.*)_(mean|lwb|upb)",
               values_to = "value" ) %>% 
  pivot_wider(names_from = "stat",values_from = "value") %>% 
  dplyr::mutate(var = factor(var,levels=c("n_exp","n_exc","rel_exc"),labels=c("Expected birth","Excess birth","Relative excess")))

p1 = plot_df1 %>% 
  filter(var %in% c("Expected birth")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = lwb,ymax=upb),alpha = 0.2) +
  geom_line(aes(y = mean)) +
  geom_point(aes(y=n_birth),col="darkred" ) +
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Annual birth")

p2 = plot_df1 %>% 
  filter(var %in% c("Relative excess")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = lwb,ymax=upb),alpha = 0.2) +
  geom_line(aes(y = mean))+
  geom_hline( aes(yintercept = 0),    linetype = 2) +
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Relative excess birth",labels=scales::percent)

#national, year, month
plot_df2 = excess_birth_year_month_df %>% 
  pivot_longer(cols = c(n_exp_mean, n_exp_lwb, n_exp_upb,
                        n_exc_mean, n_exc_lwb, n_exc_upb,
                        rel_exc_mean, rel_exc_lwb, rel_exc_upb),
               names_to = c("var", "stat"),
               names_pattern = "(.*)_(mean|lwb|upb)",
               values_to = "value" ) %>% 
  pivot_wider(names_from = "stat",values_from = "value") %>% 
  dplyr::mutate(var = factor(var,levels=c("n_exp","n_exc","rel_exc"),labels=c("Expected birth","Excess birth","Relative excess")))
p3 = plot_df2 %>% 
  filter(year>=2020, var=="Relative excess") %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lwb,ymax=upb),alpha = 0.2) +
  geom_line(aes(y = mean))+
  geom_hline( aes(yintercept = 0),    linetype = 2) +
  scale_x_date(name="")+
  scale_y_continuous(name="Relative excess birth",labels=scales::percent)


#-------------------------------------------------------------------------------
#Figure 2b
rel_pop_detctz_df = pop_detctz_df %>% 
  left_join(ctz_map,by="ctz_name") %>% 
  group_by(ctz_region,year) %>% 
  dplyr::summarise(n=sum(n),.groups="drop_last") %>% 
  dplyr::summarise(n=mean(n),.groups="drop") %>% 
  dplyr::mutate(p=n/sum(n))
region_levels <- excess_birth_year_ctzreg_df %>%
  left_join(rel_pop_detctz_df, by="ctz_region") %>%
  distinct(ctz_region, p) %>%
  arrange(-p) %>%
  mutate(ctz_region2 = paste0(ctz_region, " (", round(p*100, 1), "%)")) %>%
  pull(ctz_region2)

n=15
cols <- scales::hue_pal()(n)
eu_colors <- cols[c(1,3,5)] #scales::seq_gradient_pal("darkblue", "#ABD9E9")(seq(0,1,length.out=3))
rest_colors <- cols[c(7,8,9,10,14,15)]# scales::seq_gradient_pal("#D7191C", "#FDAE61")(seq(0,1,length.out=6))

world_reg_name = c("Switzerland", "Western Europe", "Eastern Europe",
                   "South America", "North America", "Central America",
                   "Africa", "Asia", "Oceania")

rel_pop_detctz_df <- rel_pop_detctz_df %>%
  mutate(ctz_region2 = paste0(ctz_region, " (", round(p*100, 1), "%)"),
         ctz_region2 = factor(ctz_region2, levels = region_levels)) %>%
  #sort so that colors are in right orders
  mutate(ctz_region = factor(ctz_region, levels = world_reg_name),
         ctz_region3 = gsub(" ", "\n", ctz_region),
         ctz_region4 = gsub(" ", "\n", ctz_region2)) %>% 
  arrange(ctz_region)
rel_pop_detctz_df$fill_col = c(eu_colors,rest_colors)

p4 = excess_birth_ctzreg_df %>%
  left_join(rel_pop_detctz_df,by="ctz_region") %>% 
  ggplot(aes(x=ctz_region4,y=rel_exc_mean,ymin=rel_exc_lwb,ymax=rel_exc_upb,col=ctz_region4))+
  geom_hline(yintercept = 0, lty=2)+
  geom_pointrange()+
  scale_x_discrete(name="",
                   limits = rel_pop_detctz_df$ctz_region4)+
  scale_y_continuous(name = "Relative excess birth",breaks=c(-0.2,0,0.2,0.4,0.6,0.8,1),labels = scales::percent)+
  scale_color_manual( name = "Region",
                      values = rel_pop_detctz_df$fill_col,
                      breaks = rel_pop_detctz_df$ctz_region4) +
  theme(legend.position = "none")
p5 = excess_birth_year_ctzreg_df %>%
  left_join(rel_pop_detctz_df, by="ctz_region") %>%
  mutate(region_group = if_else(ctz_region %in% c("Switzerland", "Western Europe", "Eastern Europe"),
                                "Europe", "Other")) %>% 
  filter(p>0.01) %>% 
  ggplot(aes(x=year, y=rel_exc_mean, ymin=rel_exc_lwb, ymax=rel_exc_upb)) +
  geom_ribbon(aes(fill=ctz_region), alpha=0.1) +
  geom_line(aes(col=ctz_region)) +
  facet_wrap(region_group~.,nrow=2,scales="free_y") +
  geom_hline(yintercept = 0, lty=2) +
  scale_y_continuous(name = "Relative excess birth",breaks=c(-0.2,0,0.2,0.4,0.6,0.8,1),labels = scales::percent)+
  scale_x_continuous(name="",breaks=c(2011,2015,2020,last_year))+
  scale_color_manual( name = "",
                      values = rel_pop_detctz_df$fill_col,
                      breaks = rel_pop_detctz_df$ctz_region)+
  scale_fill_manual( name = "",
                     values = rel_pop_detctz_df$fill_col,
                     breaks = rel_pop_detctz_df$ctz_region)+
  theme(legend.position = "bottom",
        legend.box.margin = margin(t = -18))


#plot
fig2a = cowplot::plot_grid(p1,p2,p3,nrow=3,labels=c("A.","B.","C."))

fig2b = cowplot::plot_grid(p4, p5+theme(legend.position = "none"), get_legend2(p5),
                           nrow=3,labels=c("A.","B.",""),rel_heights = c(1,1.8,0.2))


ggsave(filename = paste0(code_root_path,"slides/images/fig2a.pdf"),
       plot = fig2a,
       width = 8, height = 8, units = "in")

ggsave(filename = paste0(code_root_path,"slides/images/fig2b.pdf"),
       plot = fig2b,
       width = 8, height = 8, units = "in")

################################################################################
#Figure 5
subgroup_col = c("black", "darkred", "darkorange", "darkgreen","darkblue")
subgroup_names = c("all", "swiss","non-swiss","first","second")
subgroup_names2 = c("All", "Swiss","Non-Swiss","1st births","2nd+ births")

#load data
birth_prob_by_age_swiss_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_swiss,"_seedid",seed_id,"_birthprob.RDS"))
birth_prob_by_age_nonswiss_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_nonswiss,"_seedid",seed_id,"_birthprob.RDS"))
birth_prob_by_age_first_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_first,"_seedid",seed_id,"_birthprob.RDS"))
birth_prob_by_age_second_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_second,"_seedid",seed_id,"_birthprob.RDS"))

excess_birth_year_swiss_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_swiss,"_","seedid",seed_id,"_","excess_birth_year_df",".RDS"))
excess_birth_year_nonswiss_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_nonswiss,"_","seedid",seed_id,"_","excess_birth_year_df",".RDS"))
excess_birth_year_first_df = readRDS(paste0(code_root_path,res_path,save.date2,"_",mod_name_first,"_","seedid",seed_id,"_","excess_birth_year_df",".RDS"))
excess_birth_year_second_df = readRDS(paste0(code_root_path,res_path,save.date2,"_",mod_name_second,"_","seedid",seed_id,"_","excess_birth_year_df",".RDS"))

excess_by_ntiles_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,ntile_year_suffix,"_","excess_birth_ntiles_df",".RDS"))
excess_by_ntiles_swiss_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_swiss,"_","seedid",seed_id,ntile_year_suffix,"_","excess_birth_ntiles_df",".RDS"))
excess_by_ntiles_nonswiss_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_nonswiss,"_","seedid",seed_id,ntile_year_suffix,"_","excess_birth_ntiles_df",".RDS"))
excess_by_ntiles_first_df = readRDS(paste0(code_root_path,res_path,save.date2,"_",mod_name_first,"_","seedid",seed_id,ntile_year_suffix,"_","excess_birth_ntiles_df",".RDS"))
excess_by_ntiles_second_df = readRDS(paste0(code_root_path,res_path,save.date2,"_",mod_name_second,"_","seedid",seed_id,ntile_year_suffix,"_","excess_birth_ntiles_df",".RDS"))


#birth prob
birth_prob_by_age_subgroup_df = rbind(birth_prob_by_age_df %>% filter(year==2025) %>% dplyr::mutate(subgroup="all"),
                                      birth_prob_by_age_swiss_df %>% filter(year==2025) %>% dplyr::mutate(subgroup="swiss"),
                                      birth_prob_by_age_nonswiss_df %>% filter(year==2025) %>% dplyr::mutate(subgroup="non-swiss"),
                                      birth_prob_by_age_first_df %>% filter(year==2025) %>% dplyr::mutate(subgroup="first"),
                                      birth_prob_by_age_second_df %>% filter(year==2025) %>% dplyr::mutate(subgroup="second"))
birth_prob_by_age_subgroup_df = birth_prob_by_age_subgroup_df %>% 
  left_join(birth_prob_by_age_subgroup_df %>% 
              filter(subgroup=="all") %>% 
              dplyr::select(mother_age,est_all = est),by="mother_age")

#excess
excess_birth_year_subgroup_df = rbind(excess_birth_year_df %>% dplyr::mutate(subgroup="all"),
                                      excess_birth_year_swiss_df  %>% dplyr::mutate(subgroup="swiss"),
                                      excess_birth_year_nonswiss_df  %>% dplyr::mutate(subgroup="non-swiss"),
                                      excess_birth_year_first_df  %>% dplyr::mutate(subgroup="first"),
                                      excess_birth_year_second_df  %>% dplyr::mutate(subgroup="second"))
excess_birth_year_subgroup_df = excess_birth_year_subgroup_df %>% 
  left_join(excess_birth_year_subgroup_df %>% 
              filter(subgroup=="all") %>% 
              dplyr::select(year,rel_exc_mean_all = rel_exc_mean),by="year")


#excess
excess_by_ntiles_subgroup_df = rbind(excess_by_ntiles_df %>% dplyr::mutate(subgroup="all"),
                                     excess_by_ntiles_swiss_df  %>% dplyr::mutate(subgroup="swiss"),
                                     excess_by_ntiles_nonswiss_df  %>% dplyr::mutate(subgroup="non-swiss"),
                                     excess_by_ntiles_first_df %>% filter(childless==TRUE)  %>% dplyr::mutate(subgroup="first"),
                                     excess_by_ntiles_second_df  %>% filter(childless==TRUE)  %>% dplyr::mutate(subgroup="second"))
excess_by_ntiles_subgroup_df = excess_by_ntiles_subgroup_df %>% 
  left_join(excess_by_ntiles_subgroup_df %>% 
              filter(subgroup=="all") %>% 
              dplyr::select(ntile,explanatory_var,rel_exc_mean_all = rel_exc_mean),by=c("ntile","explanatory_var"))

#nationality
p1 = birth_prob_by_age_subgroup_df %>% 
  filter(subgroup %in% c("all","swiss","non-swiss")) %>% 
  ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(subgroup)),alpha=0.2)+
  geom_line(aes(col=factor(subgroup)))+
  scale_x_continuous(name="Maternal age")+
  scale_y_continuous(name="Probability of having a child\n(by month)", labels = scales::percent)+
  scale_color_manual(name = "",
                     values = subgroup_col,
                     labels = subgroup_names2,
                     breaks =subgroup_names)+
  scale_fill_manual(name = "",
                    values = subgroup_col,
                    labels = subgroup_names2,
                    breaks =subgroup_names)+
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1))

p2 = excess_birth_year_subgroup_df %>% 
  filter(subgroup %in% c("swiss","non-swiss")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = rel_exc_lwb ,ymax=rel_exc_upb ,fill=subgroup),alpha = 0.2) +
  geom_line(aes(y = rel_exc_mean , col=subgroup))+
  geom_line(aes(y = rel_exc_mean_all))+ #linetype=3, linewidth = 2)+
  geom_hline( aes(yintercept = 0),    linetype = 2) +
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Relative excess birth",labels=scales::percent)+
  scale_color_manual(name = "",
                     values = subgroup_col,
                     breaks =subgroup_names)+
  scale_fill_manual(name = "",
                    values = subgroup_col,
                    breaks =subgroup_names)+
  theme(legend.position = "none")

p3 = excess_by_ntiles_subgroup_df %>% 
  filter(subgroup %in% c("swiss","non-swiss")) %>% 
  left_join(data.frame(explanatory_var  = c("pop_dens_building_ntile"),
                       explanatory_var2 =  c("Population density")),by="explanatory_var") %>% 
  filter(!is.na(explanatory_var2)) %>% 
  dplyr::mutate(explanatory_var2 = factor(explanatory_var2, levels = c("Population density"))) %>% 
  ggplot(aes(x = ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
  geom_ribbon(aes(fill=subgroup),alpha = 0.2) +
  geom_line(aes(col=subgroup)) +
  geom_point(aes(col=subgroup)) +
  geom_line(aes(y = rel_exc_mean_all))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_x_continuous(name="Population decile",breaks=c(1:10))+
  scale_y_continuous(name="Relative excess birth",labels = scales::percent)+
  facet_wrap(explanatory_var2~.,ncol=2)+
  scale_color_manual(name = "",
                     values = subgroup_col,
                     breaks =subgroup_names)+
  scale_fill_manual(name = "",
                    values = subgroup_col,
                    breaks =subgroup_names)+
  theme(legend.position = "none")

fig5a = cowplot::plot_grid(p1,p2,p3,
                   labels = c("A.","B.","C."),nrow=3,rel_heights = c(1,1,1))

ggsave(filename = paste0(code_root_path,"slides/images/fig5a.png"),
       plot = fig5a,
       width = 6, height = 8,units = "in", dpi = 600)

#parity
p4 = birth_prob_by_age_subgroup_df %>% 
  filter(subgroup %in% c("all","first","second")) %>% 
  ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(subgroup)),alpha=0.2)+
  geom_line(aes(col=factor(subgroup)))+
  scale_x_continuous(name="Maternal age")+
  scale_y_continuous(name="Probability of having a child\n(by month)", labels = scales::percent)+
  scale_color_manual(name = "",
                     values = subgroup_col,
                     labels = subgroup_names2,
                     breaks =subgroup_names)+
  scale_fill_manual(name = "",
                    values = subgroup_col,
                    labels = subgroup_names2,
                    breaks =subgroup_names)+
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1))

p5 = excess_birth_year_subgroup_df %>% 
  filter(subgroup %in% c("first","second")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = rel_exc_lwb ,ymax=rel_exc_upb ,fill=subgroup),alpha = 0.2) +
  geom_line(aes(y = rel_exc_mean , col=subgroup))+
  geom_line(aes(y = rel_exc_mean_all))+
  geom_hline( aes(yintercept = 0),    linetype = 2) +
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Relative excess birth",labels=scales::percent)+
  scale_color_manual(name = "",
                     values = subgroup_col,
                     breaks =subgroup_names)+
  scale_fill_manual(name = "",
                    values = subgroup_col,
                    breaks =subgroup_names)+
  theme(legend.position = "none")


p6 = excess_by_ntiles_subgroup_df %>% 
  filter(subgroup %in% c("first","second")) %>% 
  left_join(data.frame(explanatory_var  = c("pop_dens_building_ntile"),
                       explanatory_var2 =  c("Population density")),by="explanatory_var") %>% 
  filter(!is.na(explanatory_var2)) %>% 
  dplyr::mutate(explanatory_var2 = factor(explanatory_var2, levels = c("Population density"))) %>% 
  ggplot(aes(x = ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
  geom_ribbon(aes(fill=subgroup),alpha = 0.2) +
  geom_line(aes(col=subgroup)) +
  geom_point(aes(col=subgroup)) +
  geom_line(aes(y = rel_exc_mean_all))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_x_continuous(name="Population decile",breaks=c(1:10))+
  scale_y_continuous(name="Relative excess birth",labels = scales::percent)+
  facet_wrap(explanatory_var2~.,ncol=2)+
  scale_color_manual(name = "",
                     values = subgroup_col,
                     breaks =subgroup_names)+
  scale_fill_manual(name = "",
                    values = subgroup_col,
                    breaks =subgroup_names)+
  theme(legend.position = "none")

fig5b = cowplot::plot_grid(p4,p5,p6,
                           labels = c("A.","B.","C."),nrow=3,rel_heights = c(1,1,1))

ggsave(filename = paste0(code_root_path,"slides/images/fig5b.png"),
       plot = fig5b,
       width = 6, height = 8,units = "in", dpi = 600)





#plot
excess_by_ntiles_df = readRDS(paste0(code_root_path,res_path,save.date,"_",mod_name_full,"_","seedid",seed_id,ntile_year_suffix,"_","excess_birth_ntiles_df",".RDS"))
fig4 = excess_by_ntiles_df %>% 
  left_join(explanatory_var_df %>% 
              dplyr::mutate(explanatory_var = paste0(explanatory_var,"_ntile")),by="explanatory_var") %>% 
  filter(explanatory_var %in% c("pop_dens_building_ntile")) %>% 
  dplyr::mutate(explanatory_var2 = factor(explanatory_var2, levels = explanatory_var_df$explanatory_var2)) %>% 
  ggplot(aes(x = ntile, y = rel_exc_mean, ymin = rel_exc_lwb, ymax = rel_exc_upb))+
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_x_continuous(name="Population decile",breaks=c(1:10))+
  scale_y_continuous(name="Relative excess birth",labels = scales::percent)+
  facet_wrap(explanatory_var2~.,ncol=2)
fig4 
ggsave(filename = paste0(code_root_path,"slides/images/fig4_population_density.pdf"),
       plot = fig4,
       width = 7, height = 4.5,units = "in", dpi = 600)
