source("R/000_setup.R")

#load data
#birth data, 1987_2024
birth_1987_2024 = load_birth_data()
mun_df = load_municipality_data()
pop_df = load_pop_year_age_nat_ctn()

#assign a current mun_id from mother_municipality as well as its corresponding district and canton
birth_df = birth_1987_2024 %>% 
  left_join(mun_df %>% dplyr::select(mother_ctn_abbr=ctn_abbr, mother_ctn_id = ctn_id,
                                       mother_dist_name = dist_name, mother_dist_id = dist_id,
                                       mother_mun_id = mun_id, mother_mun_name = mun_name,
                                       mother_municipality = hist_mun_id),by="mother_municipality")

if(FALSE){
  #check Swiss mother (i.e., mother_municipality 8100), with missing mun_id
  birth_df %>% 
    filter(is.na(mother_mun_id),mother_municipality==8100)
}




stan_years = 2000:2024
birth_mod_df
pop_mod_df


#data
birth_mod_df = birth_df %>% 
  filter(!is.na(mother_mun_id),mother_age %in% 15:50) %>% 
  group_by(year,mother_age) %>% 
  dplyr::summarise(n_birth=n(),.groups="drop")

pop_mod_df = pop_df %>% 
  filter(month==7) %>% 
  group_by(year,mother_age=age) %>% 
  dplyr::summarise(n_pop=sum(n),.groups="drop")

stan_df = pop_mod_df %>% 
  left_join(birth_mod_df,by=c("year","mother_age")) %>% 
  filter(year %in% stan_years) %>% 
  dplyr::mutate(n_birth = replace_na(n_birth,0),
                year_id = year-min(year) + 1,
                age_id = mother_age - min(mother_age) + 1)

#stan list
stan_data = list(N = dim(stan_df)[1],
                 N_year = length(unique(stan_df$year)),
                 N_age = length(unique(stan_df$mother_age)),
                 year_id = stan_df$year_id,
                 age_id = stan_df$age_id,
                 n_pop = stan_df$n_pop,
                 n_birth = stan_df$n_birth,
                 
                 x = sort(unique(stan_df$year_id)),
                 y = sort(unique(stan_df$age_id)),
                 
                 M_year = 20, 
                 c_year = 5,
                 M_age = 20, 
                 c_age = 5,
                 
                 p_intercept = c(-4,2),
                 p_lambda_year = c(0.5, 0.5),#c(0,0.4)
                 p_alpha_year = c(0,0.5),
                 p_lambda_age = c(0.5, 0.5),
                 p_alpha_age = c(0,0.5))


mod <- cmdstan_model("stan/mod1.stan")
fit <- mod$sample(data = stan_data,
                         chains = 4,
                         parallel_chains = 4,
                         iter_sampling = 200,
                         iter_warmup = 200,
                         refresh = 10)
                 


fit$summary() %>% arrange(-rhat)


fit$summary(variables = c("intercept","lambda_year","alpha_year","lambda_age","alpha_age"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% View()
fit$summary(variables = c("f_age"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% View()

chains = fit$time()$chains %>% filter(!is.na(total)) %>% pull(chain_id) %>% .[1:2]
stan_diag =  data.frame(time = fit$time()$chains %>% filter(!is.na(total)) %>% pull(total) %>% max(),
                        num_successful_chains =  fit$diagnostic_summary()$num_divergent %>% length(),
                        num_divergent = fit$diagnostic_summary()$num_divergent %>% sum(),#fit$sampler_diagnostics()
                        num_max_treedepth = fit$diagnostic_summary()$num_max_treedepth %>% sum(),
                        ebfmi = fit$diagnostic_summary()$ebfmi %>% min(),
                        rhat = fit$summary() %>% filter(!is.na(rhat)) %>% pull(rhat) %>% max()) %>% 
  dplyr::mutate(is.stan.ok = num_successful_chains>=4 & num_divergent==0 & ebfmi>=0.3 & rhat<1.1)
print(stan_diag)



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