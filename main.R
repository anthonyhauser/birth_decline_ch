source("R/000_setup.R")

#load data
#birth data, 1987_2024
birth_1987_2024 = load_birth_data()
mun_df = load_municipality_data()

#population data
pop_df = load_pop_year_age_ctn_ctz() #population ctn 1987-2024
pop_mun_df = load_pop_year_age_mun_ctz() #population mun 2011-2025

#rural/urban, votation
rural_urban_df = load_rural_urban_data()
vote_df = load_vote_data()

mun_df %>% filter(mun_name=="Moutier")
pop_mun_df %>% filter(reg_name=="Moutier")

rural_urban_df %>% filter(mun_name=="Moutier")
vote_df %>% filter(reg_name=="Moutier")

#population municipality
#agegp, country ctz, 1970-2000
pop_ctz_df = load_pop_10year_agegp_mun_ctz()

#assign a current mun_id from mother_municipality as well as its corresponding district and canton
birth_df = birth_1987_2024 %>% 
  filter(live_birth==1,mother_permanent==1) %>% 
  left_join(mun_df %>% dplyr::select(mother_ctn_abbr = ctn_abbr, mother_ctn_id = ctn_id,
                                       mother_dist_name = dist_name, mother_dist_id = dist_id,
                                       mother_mun_id = mun_id, mother_mun_name = mun_name,
                                       mother_municipality = hist_mun_id),by="mother_municipality")
if(FALSE){
  #check Swiss mother (i.e., mother_municipality 8100), with missing mun_id: 1 row
  birth_df %>% 
    filter(is.na(mother_mun_id),mother_municipality==8100)
}

birth_agg_df = birth_df %>% 
  filter(!is.na(mother_mun_id)) %>% #remove foreigners (and 1 row with missing mun_id)
  group_by(year,month,mother_age,ctn_abbr=mother_ctn_abbr) %>% #we could add mother_citizenship (but needs to be binary),other_ctn_id,mother_dist_name
  dplyr::summarise(n = n(),.groups="drop")

#save for cluster
saveRDS(birth_agg_df,file="cluster/cluster_data/birth_agg_df.RDS")
saveRDS(pop_df,file="cluster/cluster_data/pop_agg_df.RDS")


birth_df = birth_agg_df

###############################################################################################################################################################

#non-parameteric

res1 =cmstan_fit_mod1_nonparam(birth_agg_df, pop_df,stan_years = 2000:2024,
                                    dist = "negbin",#"normal"
                                    cut_age_group_year_gp = NULL, #c(28,35)
                                    seed_id=123)

res2 =cmstan_fit_mod1_nonparam(birth_agg_df, pop_df,stan_years = 2000:2024,
                               dist = "negbin",#"normal"
                               cut_age_group_year_gp = c(28,35),
                               seed_id=123)


cowplot::plot_grid(
  #GP age
  res1$gp_age_df %>% 
    ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
    geom_ribbon(fill="blue",alpha=0.2)+
    geom_line(col="blue"),
  #GP year
  res1$gp_year_df %>% 
    ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
    geom_ribbon(aes(fill=factor(group_year_id)),alpha=0.2)+
    geom_line(aes(col=factor(group_year_id)))+
    theme(legend.position = "bottom"))

cowplot::plot_grid(
  #GP age
  res2$gp_age_df %>% 
    ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
    geom_ribbon(fill="blue",alpha=0.2)+
    geom_line(col="blue"),
  #GP year
  res2$gp_year_df %>% 
    ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
    geom_ribbon(aes(fill=factor(group_year_id)),alpha=0.2)+
    geom_line(aes(col=factor(group_year_id)))+
    theme(legend.position = "bottom"))

res3 = cmdstan_fit_mod1_param(birth_df, pop_df, stan_years = 2000:2024,
                                         mod_name = "mod1_param_1expgp_2fixed.stan",
                                         save_draw = FALSE, save.date=NULL,
                                         seed_id=123)
res4 = cmdstan_fit_mod1_param(birth_df, pop_df, stan_years = 2000:2024,
                                         mod_name = "mod1_param_2expgp_1fixed.stan",
                                         save_draw = FALSE, save.date=NULL,
                                         seed_id=123)

res6 = cmdstan_fit_mod1_param(birth_df, pop_df, stan_years = 2000:2024,
                              mod_name = "mod1_rw1.stan",
                              save_draw = FALSE, save.date=NULL,
                              seed_id=123)



#parametric function
res3$birth_prob_by_age_df %>% 
  filter(year %in% c(2000,2010,2021,2024)) %>% 
  ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
  geom_line(aes(col=factor(year)))
#GP
res3$gp_df %>% 
  ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue")+
  facet_grid(group_id ~.)


#parametric function
res4$birth_prob_by_age_df %>% 
  filter(year %in% c(2000,2010,2021,2024)) %>% 
  ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
  geom_line(aes(col=factor(year)))
#GP
res4$gp_df %>% 
  ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue")+
  facet_grid(group_id ~.)

res5 = cmdstan_fit_mod2(birth_df, pop_df, stan_years = 2000:2024,
                            mod_name = "mod2_1expgp_1periodic.stan",
                            save_draw = FALSE, save.date,
                            seed_id=123)

#parametric function
res5$birth_prob_by_age_df %>% 
  filter(year %in% c(2000,2010,2021,2024),month==1) %>% 
  ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
  geom_line(aes(col=factor(year)))

#GP
cowplot::plot_grid(
res5$gp_year_df %>% 
  ggplot(aes(x=date,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue"),#facet_grid(group_id ~.)
res5$gp_month_df %>%
  filter(year<=2001) %>% 
  ggplot(aes(x=date,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue"))

save(res1,res2,res3,res4,res5,res6,file="results/res.RData")


load("results/res.RData")


save.date = "20260122"
paste0("results/",save.date,"/res_mod3_1expgp_1periodic.RData")




load(paste0("results/",save.date,"/res_mod3_1expgp_1periodic.RData"))
res$cmdstan_diag
res$birth_prob_by_age_df %>% 
  filter(year %in% c(2000,2010,2021,2024),month==1) %>% 
  ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
  geom_line(aes(col=factor(year)))

res$gp_df %>% 
ggplot(aes(x=date,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue")

res$par_df %>% 
  filter(grepl("beta_reg",variable)) %>% 
  tidyr::extract(variable,into=c("variable","reg_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
  as_tibble() %>% 
  dplyr::select(reg_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  dplyr::mutate(reg_id = as.numeric(reg_id)) %>% 
  left_join(stan_df %>% dplyr::select(ctn_abbr,reg_id) %>% distinct(),by="reg_id") %>% 
  ggplot(aes(x=ctn_abbr, y=est))+
  geom_hline(yintercept = 0,lty=4,alpha=0.5)+
  geom_point()+
  geom_segment(aes(xend = ctn_abbr, y = lwb, yend = upb))

#https://www.swisstopo.admin.ch/en/landscape-model-swissboundaries3d
ctn_sf <- st_read("data/boundary_data/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp") %>% 
  dplyr::select(ctn_name = NAME, geometry) %>% 
  left_join(ctn_map,by=c("ctn_name"="region"))

res$par_df %>% 
  filter(grepl("beta_reg",variable)) %>% 
  tidyr::extract(variable,into=c("variable","reg_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
  as_tibble() %>% 
  dplyr::select(reg_id,est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  dplyr::mutate(reg_id = as.numeric(reg_id)) %>% 
  left_join(stan_df %>% dplyr::select(ctn_abbr,reg_id) %>% distinct(),by="reg_id") %>% 
  left_join(ctn_sf, by = c("ctn_abbr")) %>%
  st_as_sf() %>%
  ggplot(aes(fill = est)) +
  geom_sf(color = "black") +
  scale_fill_gradient(name = "Relative fertility",
                      low = "red",
                      high = "green" ) +
  theme( legend.position = "bottom",
         legend.direction = "horizontal" )



#main parameters
param_res$par_df
#parametric function
param_res$birth_prob_by_age_df %>% 
  filter(year %in% c(2000,2010,2021,2024)) %>% 
  ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
  geom_line(aes(col=factor(year)))
#GP
param_res$gp_df %>% 
  ggplot(aes(x=year,y=est,ymin=lwb,ymax=upb))+
  geom_ribbon(fill="blue",alpha=0.2)+
  geom_line(col="blue")+
  facet_grid(group_id ~.)

################################################################################################################################################################
#stan list


###############################################################################################################################################################






###################################################################################################################################################
###################################################################################################################################################
birth_df = birth_1987_2024 %>% 
  filter(live_birth==1,mother_permanent==1) %>% 
  dplyr::select(year,month,mother_age,birth_loc,birth_state,mother_municipality,mother_citizenship,parity) %>% 
  dplyr::mutate(mother_birth_year = year-mother_age)

#Total number of births---------------------------------------------------------
#by year: decrease from 2016, exception in 2021
birth_df %>% 
  group_by(year) %>% 
  dplyr::summarise(n=n()) %>% 
  ggplot(aes(x=year,y=n)) + geom_point()+geom_line()+
  expand_limits(y = 0)

birth_df %>% 
  filter(year %in% c(1990,2000,2010,2020,2024)) %>% 
  group_by(month,year) %>% 
  dplyr::summarise(n=n()) %>% 
  ggplot(aes(x=month,y=n,col=factor(year))) + geom_point()+geom_line()

#number of births by mother citizenship
birth_df %>% 
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  group_by(year,mother_citizenship) %>% dplyr::summarise(n=n()) %>% 
  ggplot(aes(x=year,y=n,col=factor(mother_citizenship)))+geom_point()+geom_line()+
  expand_limits(y = 0)

###################################################################################################################################################
#Mother age---------------------------------------------------------------------
#Overall mean mother age: stable increase
birth_df %>% 
  group_by(year) %>% 
  dplyr::summarise(mean = mean(mother_age)) %>% 
  ggplot(aes(x=year,y=mean))+ geom_point()+geom_line()+
  expand_limits(y = 0)

#by citizenship: higher increase in non-Swiss
birth_df %>% 
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  group_by(year,mother_citizenship) %>% 
  dplyr::summarise(mean = mean(mother_age)) %>% 
  ggplot(aes(x=year,y=mean,col=factor(mother_citizenship)))+geom_point()+geom_line()+
  expand_limits(y = 0)

#Distribution by calendar year: shift
birth_df |>
  filter(year %in% c(1990, 2000, 2010, 2020, 2024)) |>
  ggplot(aes(x = mother_age,
             y = after_stat(density),
             color = factor(year))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Proportion", color = "Year")

#by citizenship: higher increase in non-Swiss
birth_df |>
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2024)) |>
  ggplot(aes(x = mother_age,
             y = after_stat(density),
             color = factor(mother_citizenship))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Proportion", color = "Swiss")+
  facet_grid(year~.)

#By mother year of birth
#distribution by year of mother birth (censored): increase in mother age btw mother born in 1960, 70 and 80, but not clear change btw 80 and 90
birth_df |>
  filter(mother_birth_year %in% c(1960, 1970, 1980,1985)) |>
  ggplot(aes(x = mother_age, color = factor(mother_birth_year))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Count", color = "Birth year")

#By mother year of birth, accounting for right-censoring
#when inspecting by mother birth year and correctly accounting for right-censoring, we see that age of the mother is increasing with birth year
years = c(1970,1975,1980,1985,1990,1995)
birth_df %>% 
  filter(mother_birth_year %in% years,mother_age>(1987-min(years)),mother_age<(2024-max(years))) %>% 
  ggplot(aes(x = mother_age,y=after_stat(density), color = factor(mother_birth_year))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Dist", color = "Birth year")

#Swiss vs non-Swiss
#non-Swiss have lower mother age
birth_df |>
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  filter(mother_birth_year %in% c(1960, 1970, 1980,1990,1995)) |>
  ggplot(aes(x = mother_age, y=after_stat(density),color = factor(mother_citizenship))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Count", color = "Year")+
  facet_grid(mother_birth_year~.)
#increase more clear in swiss (especially in early birth year, 1970-1980) but also present in non-swiss
years = c(1970,1975,1980,1985,1990,1995) #1995 can be removed to explore difference in older
birth_df %>% 
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  filter(mother_birth_year %in% years,mother_age>(1987-min(years)),mother_age<(2024-max(years))) %>% 
  ggplot(aes(x = mother_age,y=after_stat(density), color = factor(mother_birth_year))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Dist", color = "Birth year")+
  facet_grid(mother_citizenship~.)

#Filtering on first child
#difficult to look at difference by parity because restrict the dataset as parity available only from 2005
years = c(1982,1983,1988,1990) #years = c(1980,1985,1990,1995)
birth_df %>% 
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  filter(parity==1,mother_birth_year %in% years,mother_age>(2004-min(years)),mother_age<(2024-max(years))) %>% 
  ggplot(aes(x = mother_age,y=after_stat(density), color = factor(mother_birth_year))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Dist", color = "Birth year")+facet_grid(mother_citizenship~.)
#same but cumulative distribution (seems to be a small effect)
birth_df |>
  mutate(mother_citizenship = as.numeric(mother_citizenship == 8100)) |>
  filter(parity == 1,
    mother_birth_year %in% years,
    mother_age > (2004 - min(years)),
    mother_age < (2024 - max(years))) |>
  group_by(mother_birth_year) %>% #,mother_citizenship) %>% 
  do({d <- density(.$mother_age, from = min(.$mother_age), to = max(.$mother_age))
    tibble(mother_age = d$x,
      cdf = cumsum(d$y) / sum(d$y),
      mother_birth_year = .$mother_birth_year[1]) }) |>
  ggplot(aes(x = mother_age, y = cdf, color = factor(mother_birth_year))) +
  geom_line(linewidth = 1) +
  labs(x = "Mother age", y = "Smoothed cumulative proportion", color = "Birth year")#+facet_grid(mother_citizenship~.)


#Conclusion:
#- Stable increase in mother age from 1987 to 2024 (higher increase in non-Swiss)
#- Increase visible between generations (could have been only due to decrease in number of birth in the young generation, leading to higher and higher contribution of the old generation)
#- Increase visible for both Swiss and non-Swiss
#- Increase mostly visible among mother born btw 1970-80 (for Swiss mothers) and a bit later for non-Swiss mothers
#- Seems that the effect holds when filtering on first child (i.e., increase in the age at first child)


#Parity-------------------------------------------------------------------------
#Parity distribution over years: no changes
birth_df %>% 
  filter(year %in% c(2005, 2010, 2020, 2024),parity<=5) %>% 
  ggplot(aes(x = parity, y=after_stat(density),color = factor(year))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Parity", y = "Count", color = "Year")
#Proportion of children of rank 2, 3, 4
birth_df %>% 
  filter(year>=2005) %>% 
  group_by(year,parity) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>% 
  dplyr::mutate(p=n/sum(n)) %>% ungroup() %>% 
  filter(parity>=2,parity<=4) %>% 
  ggplot(aes(x=year,y=p,col=factor(parity)))+
  geom_line()+geom_point()+
  expand_limits(y = 0)

#Mother age distribution according to parity
#by year
birth_df %>% 
  filter(year %in% c(2005, 2010, 2020, 2024),parity<=3) %>% 
  ggplot(aes(x = mother_age, y=after_stat(density),color = factor(parity))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Dist", color = "Year")+
  facet_grid(year~.)
#by mother birth year
years = c(1980,1985)
birth_df %>% 
  filter(mother_birth_year %in% years,mother_age>(2005-min(years)),mother_age<(2024-max(years)),parity<=3) %>% 
  ggplot(aes(x = mother_age, y=after_stat(density),color = factor(parity))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Mother age", y = "Dist", color = "Parity")+
  facet_grid(mother_birth_year~.)

#Parity distribution over citizenship by year: higher parity for Swiss (at least in 2005-2010)
birth_df %>% 
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  filter(year %in% c(2005, 2010, 2020, 2024),parity<=5) %>% 
  ggplot(aes(x = parity, y=after_stat(density),color = factor(mother_citizenship))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = "Parity", y = "Dist", color = "Citizenship")+
  facet_grid(year~.)
#by year of mother birth: small differences
years = c(1980,1985)
birth_df %>% 
  dplyr::mutate(mother_citizenship = as.numeric(mother_citizenship==8100)) %>%
  filter(mother_birth_year %in% years,mother_age>(2005-min(years)),mother_age<(2024-max(years)),parity<=3) %>% 
  ggplot(aes(x = parity, y=after_stat(density),color = factor(mother_citizenship))) +
  geom_freqpoly(binwidth = 1, linewidth = 1) +
  labs(x = " Parity", y = "Dist", color = "Citizenship")+
  facet_grid(mother_birth_year~.)





###################################################################################################################################################

birth_1987_2024$mother_municipality %>% unique() %>% sort()

setdiff(birth_1987_2024$mother_municipality %>% unique() %>% sort(),
        final_mun_df$hist_mun_id %>% unique() %>% sort()) %>% sort()


birth_1987_2024$mother_citizenship %>% unique() %>% sort()

birth_1987_2024 %>% filter()


#municipality data: link number with municipality name, https://www.agvchapp.bfs.admin.ch/fr
final_mun_df = load_municipality_data()

mun_df %>% filter(mun_name=="Moutier")