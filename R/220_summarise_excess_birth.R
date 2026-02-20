summarise_excess_birth_nat = function(pred_n_birth_draw_df,
                                  save.date,
                                  mod_name,
                                  seed_id){
  #national, by year--------------------------------------------------------------
  excess_birth_year_df = pred_n_birth_draw_df %>% 
    #sum over age and month
    group_by(year,draw) %>% 
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
  #age
  excess_birth_year_age_df = pred_n_birth_draw_df %>% 
    #sum over age and month
    group_by(year,age,draw) %>% 
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
  
  saveRDS(excess_birth_year_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_df",".RDS"))
  saveRDS(excess_birth_year_age_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_age_df",".RDS"))
  
  #national, by year and month----------------------------------------------------
  excess_birth_year_month_df = pred_n_birth_draw_df %>% 
    #sum over age and month
    group_by(year,month,draw) %>% 
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
                  rel_exc_upb = (n_birth-n_exp_upb)/n_exp_mean) %>% 
    dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
  
  #age
  excess_birth_year_month_age_df = pred_n_birth_draw_df %>% 
    #sum over age and month
    group_by(year,month,age,draw) %>% 
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
                  rel_exc_upb = (n_birth-n_exp_upb)/n_exp_mean) %>% 
    dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
  
  saveRDS(excess_birth_year_month_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_df",".RDS"))
  saveRDS(excess_birth_year_month_age_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_age_df",".RDS"))
  
 
  return(list(excess_birth_year_df = excess_birth_year_df,
              excess_birth_year_age_df = excess_birth_year_age_df,
              excess_birth_year_month_df = excess_birth_year_month_df,
              excess_birth_year_month_age_df = excess_birth_year_month_age_df))
}

summarise_excess_birth_reg = function(pred_n_birth_reg_draw_df,
                                  save.date,
                                  mod_name,
                                  seed_id){
  
  #by region and year-------------------------------------------------------------
  excess_birth_year_reg_df  = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(dist_name,dist_id,year,draw) %>% 
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
  #age
  excess_birth_year_age_reg_df  = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(dist_name,dist_id,year,age,draw) %>% 
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
  
  saveRDS(excess_birth_year_reg_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_reg_df",".RDS"))
  saveRDS(excess_birth_year_age_reg_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_age_reg_df",".RDS"))
  
  #by region, by year and month---------------------------------------------------
  excess_birth_year_month_reg_df  = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(dist_name,dist_id,year,month,draw) %>% 
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
                  rel_exc_upb = (n_birth-n_exp_upb)/n_exp_mean) %>% 
    dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
  
  saveRDS(excess_birth_year_month_reg_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_reg_df",".RDS"))
  
  #age (takes 8 minutes)
  print("computing: excess_birth_year_month_age_reg_df")
  setDT(pred_n_birth_reg_draw_df)
  
  excess_birth_year_month_age_reg_df <- 
    pred_n_birth_reg_draw_df[,# summarise (mean and quantiles)
                              .(n_pop       = n_pop[1],
                                n_birth     = n_birth[1],
                                n_exp_mean  = mean(n_pred),
                                n_exp_lwb   = quantile(n_pred, 0.025),
                                n_exp_upb   = quantile(n_pred, 0.9725)),by = .(dist_name, dist_id, year, month, age)][,
                              # excess mortality (mutate)
                              `:=`( n_exc_mean   = n_birth - n_exp_mean,
                                    n_exc_lwb    = n_birth - n_exp_lwb,
                                    n_exc_upb    = n_birth - n_exp_upb,
                                    rel_exc_mean = (n_birth - n_exp_mean) / n_exp_mean,
                                    rel_exc_lwb  = (n_birth - n_exp_lwb)  / n_exp_mean,
                                    rel_exc_upb  = (n_birth - n_exp_upb)  / n_exp_mean,
                                    date         = as.IDate(sprintf("%d-%02d-01", year, month)))]
  
  # excess_birth_year_month_age_reg_df  = pred_n_birth_reg_draw_df %>% 
  #   #sum over age and month
  #   group_by(dist_name,dist_id,year,month,age,draw) %>% 
  #   dplyr::summarise(n_pop = sum(n_pop),
  #                    n_birth = sum(n_birth),
  #                    n_pred = sum(n_pred),.groups="drop_last") %>% 
  #   dplyr::summarise(n_pop = n_pop[1],
  #                    n_birth = n_birth[1],
  #                    n_exp_mean = mean(n_pred),
  #                    n_exp_lwb = quantile(n_pred,probs=0.025),
  #                    n_exp_upb = quantile(n_pred,probs=0.9725),.groups="drop") %>% 
  #   dplyr::mutate(n_exc_mean = n_birth - n_exp_mean,
  #                 n_exc_lwb = n_birth- n_exp_lwb,
  #                 n_exc_upb = n_birth-n_exp_upb,
  #                 rel_exc_mean = (n_birth - n_exp_mean)/n_exp_mean,
  #                 rel_exc_lwb = (n_birth- n_exp_lwb)/n_exp_mean,
  #                 rel_exc_upb = (n_birth-n_exp_upb)/n_exp_mean) %>% 
  #   dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
  
  saveRDS(excess_birth_year_month_age_reg_df, paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_age_reg_df",".RDS"))
  
  if(FALSE){
    save.date="20260214"
    mod_name = "mod5"
    seed_id=8
    excess_birth_year_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_df",".RDS"))
    excess_birth_year_age_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_age_df",".RDS"))
    excess_birth_year_month_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_df",".RDS"))
    excess_birth_year_month_age_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_age_df",".RDS"))
    excess_birth_year_reg_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_reg_df",".RDS"))
    excess_birth_year_age_reg_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_age_reg_df",".RDS"))
    excess_birth_year_month_reg_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_reg_df",".RDS"))
    excess_birth_year_month_age_reg_df = readRDS(paste0("results/",save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_month_age_reg_df",".RDS"))
  }
  
  return(list(excess_birth_year_reg_df = excess_birth_year_reg_df,
              excess_birth_year_age_reg_df = excess_birth_year_age_reg_df,
              excess_birth_year_month_reg_df = excess_birth_year_month_reg_df,
              excess_birth_year_month_age_reg_df = excess_birth_year_month_age_reg_df))
}
