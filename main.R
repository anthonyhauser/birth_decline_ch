source("R/000_setup.R")

#load data----------------------------------------------------------------------
#load raw data, clean and save cleaned data in savepoint
if(FALSE){
  #municipality data
  mun_df = load_municipality_data()
  new_mun_df = rbind(mun_df %>% filter(!(mun_id %in% mun_ids_to_agg)),
                     mun_df %>% filter(mun_id==3101) %>% 
                       dplyr::mutate(mun_id=31010,mun_name="Appenzell (aggregated)"))
  
  #birth data
  birth_df = load_birth_data(mun_df = new_mun_df, rerun=FALSE,last_year=last_year) #automatically saved
  birth_agg_df = birth_df %>% #by canton, as the minimum aggregation level for fit (it is even too detailled as we only run nationally)
    group_by(year,month,mother_age,ctn_abbr=mother_ctn_abbr,citizenship=mother_citizenship2) %>% 
    dplyr::summarise(n = n(),.groups="drop")
  birth_agg_first_df = birth_df %>% 
    filter(year>=2005,!is.na(parity),parity==1) %>% 
    group_by(year,month,mother_age,ctn_abbr=mother_ctn_abbr,citizenship=mother_citizenship2) %>% 
    dplyr::summarise(n = n(),.groups="drop")
  birth_agg_second_df = birth_df %>% 
    filter(year>=2005,!is.na(parity),parity>1) %>% 
    group_by(year,month,mother_age,ctn_abbr=mother_ctn_abbr,citizenship=mother_citizenship2) %>% 
    dplyr::summarise(n = n(),.groups="drop")
  
  #population data
  pop_df = load_pop_year_age_ctn_ctz(last_year = last_year) #population ctn 1987-2024 or 1987-2025
  pop_mun_df_list = load_pop_year_age_mun_ctz(mun_df = mun_df) #population mun 2011-2025, list of df, one for each aggregation level (national, canton, district, municipality)
  pop_mun_df = pop_mun_df_list[["municipality"]]#reg_id is historical region id (e.g., for municipality levels it corresponds to hist_mun_id)
  pop_detctz_df = load_pop_year_age_ctn_detctz()
  
  new_pop_mun_df = pop_mun_df %>% 
    dplyr::mutate(mun_name = if_else(mun_id %in% mun_ids_to_agg, "Appenzell (aggregated)",mun_name),
                  mun_id = if_else(mun_id %in% mun_ids_to_agg, 31010,mun_id))
  
  #rural/urban, votation
  sep_df3 = readRDS("savepoint/sep_df3.RDS")
  rural_urban_df = load_rural_urban_data(pop_mun_df = pop_mun_df)
  pop_dens_df = load_population_density_data(new_mun_df)
  vote_df_list = load_vote_data(new_mun_df,new_pop_mun_df)
  vote_mun_df = vote_df_list[["municipality"]]
  
  #load childcare institutions
  childcare_mun_df = load_childcare_institutions_data(new_pop_mun_df, new_mun_df)
  childcare_institutions_df = childcare_mun_df$childcare_institutions_df
  childcare_institutions_adj_df = childcare_mun_df$childcare_institutions_adj_df
  
  #District-level data----------------------------------------------------------
  #population
  pop_dist_df = pop_mun_df %>% 
    group_by(dist_name,dist_id, year, month, age, citizenship) %>% 
    dplyr::summarise(n=sum(n),.groups="drop") 
  pop_ctz_df = pop_mun_df %>% 
    group_by(citizenship, year, month, age) %>% 
    dplyr::summarise(n=sum(n),.groups="drop") 
  #votation
  vote_dist_df = vote_mun_df %>% 
    group_by(vote_object,dist_name,dist_id) %>% 
    dplyr::summarise(yes_prop = sum(yes_prop * n_pop)/sum(n_pop),.groups="drop") 
  #rural/urban
  rural_urban_dist_df = rural_urban_df %>% 
    group_by(dist_name,dist_id) %>% 
    dplyr::summarise(urban_rural_type1   = sum(urban_rural_type1 * n_pop)/sum(n_pop),
                     urban_rural_type2   = sum(urban_rural_type2 * n_pop)/sum(n_pop),.groups="drop") 
  
  save(birth_agg_df, birth_agg_first_df, birth_agg_second_df,
       pop_df, pop_mun_df, new_pop_mun_df, pop_dist_df, pop_ctz_df, pop_detctz_df,
       new_mun_df,
       sep_df3,
       rural_urban_df, rural_urban_dist_df,
       pop_dens_df,
       vote_mun_df, vote_dist_df,
       childcare_institutions_df, childcare_institutions_adj_df,
       file=ifelse(last_year==2024,"savepoint/cleaned_df.RData","savepoint/cleaned2025_df.RData"))
}

#load cleaned, aggregated data
load(ifelse(last_year==2024,"savepoint/cleaned_df.RData","savepoint/cleaned2025_df.RData"))
#group four mun_id from Appenzel district because birth are reported jointly by Appenzell municipality for some years
new_birth_df = birth_df %>% 
  dplyr::mutate(mother_mun_name = if_else(mother_mun_id %in% mun_ids_to_agg, "Appenzell (aggregated)",mother_mun_name),
                mother_mun_id = if_else(mother_mun_id %in% mun_ids_to_agg, 31010,mother_mun_id))

#run model----------------------------------------------------------------------
#stan model

#main analysis
save.date="20260622"#"20260309"
mod8_res = cmstan_fit_mod5(pop_df, birth_agg_df,
                           mod_name ="mod8",
                           stan_years=  2000:last_year,
                           effect_on_age_shift = "cal_year",
                           save_draw = TRUE, save.date,
                           seed_id = 1)
pop_df
birth_agg_df
mod_name ="mod8"
stan_years=  2000:last_year
effect_on_age_shift = "cal_year"
save_draw = TRUE
save.date
seed_id = 1

#parity
mod8_res = cmstan_fit_mod5(pop_df, birth_agg_first_df,
                           mod_name ="mod8",
                           stan_years=  2005:last_year,
                           effect_on_age_shift = "cal_year",
                           save_draw = TRUE, save.date,
                           filter_parity="first",
                           seed_id = 1)
mod8_res = cmstan_fit_mod5(pop_df, birth_agg_second_df,
                           mod_name ="mod8",
                           stan_years=  2005:last_year,
                           effect_on_age_shift = "cal_year",
                           save_draw = TRUE, save.date,
                           filter_parity="second",
                           seed_id = 1)

#citizenship
mod8_res = cmstan_fit_mod5(pop_df, birth_agg_df,
                           mod_name ="mod8",
                           stan_years=  2005:last_year,
                           effect_on_age_shift = "cal_year",
                           save_draw = TRUE, save.date,
                           filter_parity="äll",
                           filter_ctz = "swiss",
                           seed_id = 1)
mod8_res = cmstan_fit_mod5(pop_df, birth_agg_df,
                           mod_name ="mod8",
                           stan_years=  2005:last_year,
                           effect_on_age_shift = "cal_year",
                           save_draw = TRUE, save.date,
                           filter_parity="all",
                           filter_ctz = "non-swiss",
                           seed_id = 1)

mod8_res$stan_diag_df

#finer-level excess draws-------------------------------------------------------
#post-processing: estimates by district and municipality
filter_ctz =  c("swiss","non-swiss") #c("swiss","non-swiss") #c("swiss","non-swiss"), "swiss", "non-swiss"
filter_parity="all" #"all", "first", "second"
effect_on_age_shift = "cal_year"
seed_id=1 # seed_id = 1
mod_name = "mod8" #mod_name = "mod5_birthyear"
mod_name = if_else(effect_on_age_shift=="cal_year",mod_name,paste0(mod_name,"_birthyear")) #used as text to name saved results
mod_name = paste0(mod_name,ifelse(length(filter_ctz)==2,"",paste0("_",filter_ctz))) #add whether we filter on citizenship or not
mod_name = if_else(filter_parity=="all",mod_name,paste0(mod_name,"_",filter_parity)) #used as text to name saved results
mod_name = if_else(last_year!=2025,mod_name,paste0(mod_name,"_",last_year))
use.p_childless_v = if (filter_parity != "all") c(FALSE, TRUE) else FALSE
#save.date = if (filter_parity != "all") "20260320" else "20260309"

#load stan fit and stan_df
fit <- readRDS(paste0(code_root_path, "results/cmdstan_draw/", save.date, "_", mod_name,"_seedid",seed_id, ".RDS"))
stan_df = readRDS(paste0("results/",mod_name,"_standf.RDS"))

#use multinomial to distribute prediction over regions (draws over regions, month, year, age) and citizenship (swiss and non-swiss)
list_pred_n_birth_draw_list = get_pred_birth_draw_by_dist(fit, #cmdstanr fit
                                                          stan_df,
                                                          pop_dist_df,#pop by district
                                                          birth_df, #birth_df (individual level)
                                                          n_draw_subset = 100,
                                                          save.date,
                                                          mod_name,
                                                          seed_id)
pred_n_birth_draw_df = list_pred_n_birth_draw_list[["pred_n_birth_draw_df"]]
pred_n_birth_reg_draw_df = list_pred_n_birth_draw_list[["pred_n_birth_reg_draw_df"]]
pred_n_birth_ctz_draw_df = list_pred_n_birth_draw_list[["pred_n_birth_ctz_draw_df"]]


pred_n_birth_draw_df %>% 
  filter(year==2024, month==1) %>% 
  group_by(draw) %>% 
  dplyr::summarise(n_excess = sum(n_birth-n_pred)/sum(n_birth))


pred_n_birth_reg_draw_df %>% 
  filter(year==2024, month==1) %>% 
  group_by(draw) %>% 
  dplyr::summarise(n_excess = sum(n_birth-n_pred)/sum(n_birth))

#use multinomial to distribute prediction over municipality (slightly different from district, as, because they are more strata, we sum over month and age)
for(use.p_childless in use.p_childless_v){
  list_pred_n_birth_mun_draw_list = get_pred_birth_draw_by_mun(fit, #cmdstanr fit
                                                               stan_df,
                                                               new_pop_mun_df,#pop by district
                                                               new_birth_df, #birth_df (individual level)
                                                               new_mun_sf = new_mun_sf,
                                                               n_draw_subset = 100,
                                                               save.date,
                                                               mod_name,
                                                               use.p_childless,
                                                               seed_id)
  excess_birth_year_mun_draw_df = list_pred_n_birth_mun_draw_list[["excess_birth_year_mun_draw_df"]]
  excess_birth_year_adj_mun_draw_df = list_pred_n_birth_mun_draw_list[["excess_birth_year_adj_mun_draw_df"]]
  excess_birth_year_adj2_mun_draw_df = list_pred_n_birth_mun_draw_list[["excess_birth_year_adj2_mun_draw_df"]]
}

excess_birth_year_mun_draw_df %>% 
  filter(year==2011) %>% 
  group_by(draw) %>% 
  dplyr::summarise(n_excess = sum(n_birth-n_pred)/sum(n_birth))


pred_n_birth_reg_draw_df %>% 
  filter(year==2024, month==1) %>% 
  group_by(draw) %>% 
  dplyr::summarise(n_excess = sum(n_birth-n_pred)/sum(n_birth))

#use multinomial to distribute over ctz_reg and ctn_abbr
list_pred_n_birth_ctzreg_draw_list = get_pred_birth_draw_by_ctzreg(fit, #cmdstanr fit
                                                                   stan_df,
                                                                   pop_detctz_df,#pop by year ctn_abbr and detctz
                                                                   new_birth_df, #birth_df (individual level)
                                                                   n_draw_subset = 100,
                                                                   save.date,
                                                                   mod_name,
                                                                   seed_id)
excess_birth_year_ctz_draw_df = list_pred_n_birth_ctzreg_draw_list[["excess_birth_year_ctz_draw_df"]]


excess_birth_year_ctz_draw_df %>% 
  filter(year==2011) %>% 
  group_by(draw) %>% 
  dplyr::summarise(n_excess = sum(n_birth-n_pred)/sum(n_birth))


#excess estimates with uncertainty----------------------------------------------
#load draws
if(FALSE){
  #national, district, national/citizenship (by age, month and year)
  pred_n_birth_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_draw_df",".RDS"))
  pred_n_birth_reg_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_reg_draw_df",".RDS"))
  pred_n_birth_ctz_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_ctz_draw_df",".RDS"))
  #municipality (by year)
  use.p_childless=TRUE
  excess_birth_year_mun_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS"))
  excess_birth_year_adj_mun_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_adj_mun_draw_df",".RDS"))
  excess_birth_year_adj2_mun_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_adj2_mun_draw_df",".RDS"))
  
  #ctz region (by year and ctn_abbr)
  excess_birth_year_ctz_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_ctn_ctzreg_draw_df",".RDS"))
}

#Summarise excess birth 
#1) nationally (level at which model was fitted)
pred_n_birth_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_draw_df",".RDS"))
excess_birth_nat_res = summarise_excess_birth_nat(pred_n_birth_draw_df,
                                                  save.date, mod_name, seed_id)
#2) by region (using multinomial distribution to distribute over regions)
pred_n_birth_reg_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_reg_draw_df",".RDS"))
excess_birth_reg_res = summarise_excess_birth_reg(pred_n_birth_reg_draw_df,
                                                  save.date, mod_name, seed_id)
#3) by citizenship (2 levels, swiss, non-swiss), using multinomial
#only if not already restricted to swiss or non-swiss
if(length(filter_ctz)==2){
  pred_n_birth_ctz_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_ctz_draw_df",".RDS"))
  excess_birth_ctz_res = summarise_excess_birth_ctz(pred_n_birth_ctz_draw_df,
                                                    save.date, mod_name, seed_id) 
}
#4) municipality level, using multinomial
for(use.p_childless in use.p_childless_v){
  excess_birth_year_mun_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS"))
  excess_birth_year_adj_mun_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_adj_mun_draw_df",".RDS"))
  excess_birth_year_adj2_mun_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_adj2_mun_draw_df",".RDS"))
  excess_birth_mun = summarise_excess_birth_mun(excess_birth_year_adj_mun_draw_df,
                                                          excess_birth_year_adj2_mun_draw_df,
                                                          excess_birth_year_mun_draw_df,
                                                          save.date,  paste0(mod_name,ifelse(use.p_childless,"_childless","")), seed_id)
}
#5) ctz region, using multinomial
#needs to do it for all
excess_birth_year_ctz_draw_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_ctn_ctzreg_draw_df",".RDS"))
excess_birth_ctzreg = summarise_excess_birth_ctzreg(excess_birth_year_ctz_draw_df)


################################################################################
#excess birth by ntiles---------------------------------------------------------
ctz_name =  c("swiss","non-swiss","") #c("swiss","non-swiss") #c("swiss","non-swiss"), "swiss", "non-swiss"
filter_parity=c("all", "first", "second")

mod_df = expand.grid(ctz_name = ctz_name,
                     filter_parity = filter_parity,stringsAsFactors = FALSE) %>% 
  filter(ctz_name=="" | filter_parity=="all") #%>% .[3,]

mod_df <- mod_df %>%
  pmap(function(ctz_name, filter_parity) {
    print("---")
    filter_ctz = if (ctz_name == "") c("swiss","non-swiss") else ctz_name
    print(filter_ctz)
    effect_on_age_shift <- "cal_year"
    seed_id <- 1
    mod_name0 <- "mod8"
    mod_name <- if_else(effect_on_age_shift == "cal_year", mod_name0,
                        paste0(mod_name0, "_birthyear"))
    mod_name <- paste0(mod_name,
                       ifelse(length(filter_ctz) == 2, "", paste0("_", filter_ctz)))
    mod_name <- if_else(filter_parity == "all", mod_name,
                        paste0(mod_name, "_", filter_parity))
    save.date <- if (filter_parity != "all") "20260320" else "20260309"
    
    data.frame(ctz_name = ctz_name,
               filter_parity = filter_parity,
               mod_name = mod_name,
               save.date = save.date,
               seed_id = seed_id)
  }) %>% rbindlist()
    
results <- mod_df %>%
  pmap(function(ctz_name,filter_parity, mod_name, save.date, seed_id) {
    use.p_childless_v <- if (filter_parity != "all") c(FALSE, TRUE) else FALSE
    
    excess_by_ntiles(save.date, mod_name, seed_id,
                     use.p_childless_v,
                     new_pop_mun_df, rural_urban_df, pop_dens_df, sep_df3, childcare_institutions_df, vote_mun_df)
  })


################################################################################
#report

mod_df[3,] %>%
  pmap(function(ctz_name,filter_parity, mod_name, save.date, seed_id) {
    quarto::quarto_render(input = "reports/report1_pres.qmd",
                          output_file = paste0("report1_pres_",save.date,"_",mod_name,"_","seedid",seed_id,".html"),
                          execute_params = list(save.date = save.date,
                                                mod_name = mod_name,
                                                seed_id = seed_id))
  })

mod_name = "mod8_swiss"
seed_id=1
save.date="20260309"


p_childless_df =  get_prob_childless_by_mun(new_birth_df, new_pop_mun_df, filter_ctz, p_mun_changes = 0.06)
age_childless=30
p_childless_pos_mid = p_childless_df %>% filter(age==age_childless) %>% 
  left_join(new_pop_mun_df %>% dplyr::filter(year==2024,reg_agg_level=="municipality",age==35,month==1) %>%
              group_by(mun_id) %>% dplyr::summarise(n=sum(n),.groups="drop"),by="mun_id") %>% 
  dplyr::summarise(p_childless_pos=sum(n*p_childless_pos2)/sum(n)) %>% pull(p_childless_pos)

p_childless_df %>% filter(age==age_childless) %>% 
  left_join(new_mun_df %>% dplyr::select(mun_id,dist_id) %>% distinct(), by="mun_id") %>% 
  left_join(new_mun_sf %>% dplyr::mutate(mun_id=as.numeric(mun_id)), by = c("mun_id")) %>% 
  st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = 1-p_childless_pos2 ), color = NA) +  # communes sans bordure
    geom_sf(data = regions_sf %>% mutate(dist_id = as.numeric(dist_id)), 
            fill = NA, color = "black", size = 0.3) +  # contours districts
    geom_sf(data = lake_sf, fill = "lightblue", color = NA, alpha = 0.5) +
    scale_fill_gradient2(
      name = paste0("Proportion of women with child at ",age_childless),
      low = "red",
      mid = "lightyellow",
      high = "green",
      midpoint=1-p_childless_pos_mid,
      labels = scales::percent_format(accuracy = 1),
      #limits=c(-0.4,0.4)
    ) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")



excess_birth_adj2_mun_childless_df

excess_birth_adj2_mun_childless_df = readRDS(paste0(code_root_path,"results/","20260320","_","mod8_first","_childless","_","seedid",1,"_","excess_birth_adj2_mun_df",".RDS"))
excess_birth_adj2_mun_df = readRDS(paste0(code_root_path,"results/","20260320","_","mod8_first","_","seedid",1,"_","excess_birth_adj2_mun_df",".RDS"))

excess_birth_adj2_mun_childless_df %>% arrange(-rel_exc_mean)
excess_birth_adj2_mun_childless_df %>% filter(mun_id==5755)
excess_birth_adj2_mun_df %>% filter(mun_id==5755)

p_childless_df %>% 
  ggplot(aes(x=age,y=p_childless_pos2,group=mun_id))+
  geom_line(alpha=0.1)+
  geom_line(data=p_childless_df %>% filter(mun_id==5755),col="violet",linewidth = 2)



#-------------------------------------------------------------------------------
#check it's the same
pop_dist_df %>%group_by(year,month,age) %>% dplyr::summarise(n=sum(n)) %>% filter(year==2011,month==1,age==15)
pop_df %>%group_by(year,month,age) %>% dplyr::summarise(n=sum(n)) %>% filter(year==2011,month==1,age==15)

mun_df$mun_id %>% unique() %>% length()
mun_df$dist_id %>% unique() %>% length()
mun_df$hist_mun_id %>% unique() %>% length()

#---------------------------------------------------------------------
#correlation between excess and socio-demographic variables
#excess by district

excess_birth_res = summarise_excess_birth (pred_n_birth_reg_draw_df,pred_n_birth_draw_df)


excess_birth_year_reg_df %>% 
  dplyr::select(dist_id,year,rel_exc_mean) %>% 
  left_join(vote_dist_df,by="dist_id", relationship = "many-to-many") %>% 
  group_by(year,vote_object) %>% 
  dplyr::summarise(corr = cor(rel_exc_mean,yes_prop)) %>% 
  ggplot(aes(x=year,y=corr,col=vote_object)) +
  geom_point(position = position_dodge(width=1))

excess_birth_year_reg_df %>% 
  dplyr::select(dist_id,year,rel_exc_mean) %>% 
  left_join(vote_dist_df,by="dist_id", relationship = "many-to-many") %>% 
  filter(year %in% 2021:2024,vote_object %in% c("womens_suffrage","paternity_leave")) %>% 
  ggplot(aes(x=rel_exc_mean,y=yes_prop))+geom_point()+
  facet_grid(year~vote_object)

excess_birth_year_reg_df %>% 
  dplyr::select(dist_id,year,rel_exc_mean) %>% 
  left_join(rural_urban_dist_df,by="dist_id", relationship = "many-to-many") %>% 
  filter(year %in% 2021:2024) %>% 
  ggplot(aes(x=rel_exc_mean,y=urban_rural_type2))+geom_point()+
  facet_grid(year~.)

excess_birth_year_reg_df %>% 
  dplyr::select(dist_id,year,rel_exc_mean) %>% 
  left_join(rural_urban_dist_df,by="dist_id", relationship = "many-to-many") %>% 
  filter(year %in% 2024) %>% 
  arrange(urban_rural_type2)



model <- excess_birth_year_reg_df %>%
  dplyr::select(dist_id, year, rel_exc_mean) %>%
  left_join(rural_urban_dist_df %>% dplyr::select(-dist_name),
            by = "dist_id") %>%
  left_join(
    vote_dist_df %>%
      pivot_wider(names_from = vote_object,
                  values_from = yes_prop),
    by = "dist_id"
  ) %>%
  lm(rel_exc_mean*100 ~ factor(year) +
       urban_rural_type2 +
       family_tax_education_support +paternity_leave + reduce_childcare_tax + womens_suffrage,
     data = .)

summary(model)

model <- excess_birth_year_reg_df %>% 
  dplyr::select(dist_id, year, rel_exc_mean) %>%
  left_join(rural_urban_dist_df %>% dplyr::select(-dist_name) %>% 
            dplyr::mutate(urban_rural_type2_bin = as.numeric(urban_rural_type2>15)),
            by = "dist_id") %>%
  left_join(
    vote_dist_df %>%
      pivot_wider(names_from = vote_object,
                  values_from = yes_prop),
    by = "dist_id"
  ) %>%
  lm(rel_exc_mean ~ factor(year)+
       urban_rural_type2_bin,
      #family_tax_education_support +paternity_leave + reduce_childcare_tax + womens_suffrage,
     data = .)

summary(model)


rural_urban_dist_df %>% 
  dplyr::mutate(urban_rural_type2_bin = as.numeric(urban_rural_type2>15)) %>% 
  arrange(urban_rural_type2)
  

#District-level data------------------------------------------------------------
#plots












###############################################################################################################################################################

#population municipality
#agegp, country ctz, 1970-2000
pop_ctz_df = load_pop_10year_agegp_mun_ctz()


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

birth_df$mother_municipality %>% unique() %>% sort()

setdiff(birth_df$mother_municipality %>% unique() %>% sort(),
        final_mun_df$hist_mun_id %>% unique() %>% sort()) %>% sort()


birth_df$mother_citizenship %>% unique() %>% sort()

birth_df %>% filter()


#municipality data: link number with municipality name, https://www.agvchapp.bfs.admin.ch/fr
final_mun_df = load_municipality_data()

mun_df %>% filter(mun_name=="Moutier")