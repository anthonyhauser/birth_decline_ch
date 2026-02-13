get_pred_birth_draw_by_mun0 = function(fit, log_mean_n_birth_n_pop, n_draw_subset = 100, pop_dist_df){
  
  if(FALSE){
    fit = fit5_month
    log_mean_n_birth_n_pop = stan_month_data$log_mean_n_birth_n_pop
    n_draw_subset = 100
    pop_dist_df
  }

  #number of draws
  n_draws = fit$num_chains() * fit$metadata()$iter_sampling
  
  #draws
  f_age_df = fit$draws("f_age",format = "df") %>%
    pivot_longer( cols = starts_with("f_age["),names_to = "var",values_to = "value") %>% 
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    tidyr::extract(var,into=c("var","age_id","year_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(age_id = as.numeric(age_id),
                  year_id = as.numeric(year_id)) %>% 
    dplyr::select(chain,iter,draw,age_id,year_id,f_age=value)
    
  gamma_month_df <- fit$draws("gamma_month",format = "df") %>%
    pivot_longer( cols = starts_with("gamma_month["),names_to = "var",values_to = "value") %>% 
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    tidyr::extract(var,into=c("var","month_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(month_id = as.numeric(month_id)) %>% 
    dplyr::select(chain,iter,draw,month_id,gamma_month=value)
  
  inv_sigma_df <- fit$draws("inv_sigma",format = "df") %>%
    pivot_longer( cols = starts_with("inv_sigma["),names_to = "var",values_to = "value") %>% 
    dplyr::rename(chain=.chain,iter=.iteration,draw=.draw) %>% 
    tidyr::extract(var,into=c("var","sigma_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(sigma_id = as.numeric(sigma_id)) %>% 
    dplyr::select(chain,iter,draw,sigma_id,inv_sigma=value)
  
  log_mu_df = f_age_df %>% 
    left_join(gamma_month_df, by=c("chain","iter","draw"), relationship = "many-to-many") %>% 
    left_join(inv_sigma_df, by=c("chain","iter","draw"), relationship = "many-to-many") %>%
    dplyr::mutate(log_mu = f_age + gamma_month + log_mean_n_birth_n_pop,
                  mu = exp(log_mu)) %>% 
    left_join(stan_df %>% dplyr::select(year,year_id=year_id1) %>% distinct(),by="year_id") %>% 
    left_join(stan_df %>% dplyr::select(mother_age,age_id) %>% distinct(),by="age_id")
  
  #subset
  subset_draw_ids <- sample(seq_len(n_draws), n_draw_subset, replace = FALSE) %>% sort()
  subset_log_mu_df = log_mu_df %>% 
    filter(draw %in% subset_draw_ids)
    
  #prediction numbers at the national level (i.e., level at which data is fitted)
  pred_n_birth_draw_df = subset_log_mu_df %>% dplyr::rename(month=month_id, age=mother_age) %>% 
    inner_join(pop_dist_df %>%group_by(year,month,age) %>%
                dplyr::summarise(n_pop=sum(n),.groups="drop"), by=c("year"="year","month"="month","age"="age")) %>% 
    dplyr::mutate(n_exp_nat = exp(log_mu) * n_pop,
                  n_pred_nat = rnbinom(nrow(.), size = 1/inv_sigma, mu = n_exp_nat))
  
  if(FALSE){
    #check that this gives the posterior distribution by age (and year) of the probability of having a child in a month
    log_mu_df %>% 
      filter(year %in% c(2000,2010,2024),month_id==1) %>% 
      group_by(year,mother_age) %>% 
      dplyr::summarise(est = mean(mu),
                       lwb = quantile(mu,probs=c(0.025)),
                       upb = quantile(mu,probs=c(0.975))) %>% 
      ggplot(aes(x=mother_age,y=est,ymin=lwb,ymax=upb))+
      geom_ribbon(aes(fill=factor(year)),alpha=0.2)+
      geom_line(aes(col=factor(year)))
  }
  
  pred_n_birth_reg_draw_df = pred_n_birth_draw_df %>% 
    dplyr::select(year,month,age,draw,n_pred_nat) %>% 

    group_by(draw,year, month, age) %>%
    dplyr::mutate(n_pred = {p <- n_pop / sum(n_pop)
                                       as.vector(rmultinom(1, size = n_pred_nat[1], prob = p))}) %>%
    ungroup()
  
  if(FALSE){
    #check dimension
    dim(pred_n_birth_reg_draw_df)
    144 * 14 * 36 * 12 * 100 #dist year age month draw
    #check that sum of pred is national pred
    pred_n_birth_reg_draw_df %>% 
      group_by(draw,year, month, age) %>%
      dplyr::summarise(n_pred_nat=n_pred_nat[1],
                       n_pred_region=sum(n_pred))
  }
  
  return(pred_n_birth_reg_draw_df)
}


if(FALSE){
  birth_year_month_age_reg_df = birth_1987_2024 %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year, month,age=mother_age,dist_id=mother_dist_id,dist_name = mother_dist_name) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  birth_year_reg_df = birth_1987_2024 %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year, dist_id=mother_dist_id,dist_name = mother_dist_name) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  
  birth_year_df = birth_1987_2024 %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  birth_month_year_df = birth_1987_2024 %>% 
    filter(year %in% 2011:2024) %>% 
    group_by(year,month) %>% 
    dplyr::summarise(n_birth=n(),.groups="drop")
  
  #dist, year
  excess_birth_year_df = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(year,draw) %>% 
    dplyr::summarise(n_pop = sum(n_pop),
                     n_pred = sum(n_pred),.groups="drop_last") %>% 
    dplyr::summarise(n_pop = n_pop[1],
                     n_exp_mean = mean(n_pred),
                     n_exp_lwb = quantile(n_pred,probs=0.025),
                     n_exp_upb = quantile(n_pred,probs=0.9725),.groups="drop") %>% 
    left_join(birth_year_df %>% dplyr::select(year,n_birth),
              by=c("year")) %>% 
    dplyr::mutate(n_exc_mean = n_birth - n_exp_mean,
                  n_exc_lwb = n_birth- n_exp_lwb,
                  n_exc_upb = n_birth-n_exp_upb)
  
  
  excess_birth_year_df %>% 
    ggplot(aes(x = year, y = n_exc_mean/n_exp_mean, ymin = n_exc_lwb/n_exp_mean, ymax = n_exc_upb/n_exp_mean)) +
    geom_hline(yintercept=0,lty=2)+
    geom_ribbon(alpha = 0.2) +
    geom_line() 
  
  
  #dist, year
  excess_birth_year_month_df = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(year,month,draw) %>% 
    dplyr::summarise(n_pop = sum(n_pop),
                     n_pred = sum(n_pred),.groups="drop_last") %>% 
    dplyr::summarise(n_pop = n_pop[1],
                     n_exp_mean = mean(n_pred),
                     n_exp_lwb = quantile(n_pred,probs=0.025),
                     n_exp_upb = quantile(n_pred,probs=0.9725),.groups="drop") %>% 
    left_join(birth_month_year_df %>% dplyr::select(year,,month,n_birth),
              by=c("year","month")) %>% 
    dplyr::mutate(n_exc_mean = n_birth - n_exp_mean,
                  n_exc_lwb = n_birth- n_exp_lwb,
                  n_exc_upb = n_birth-n_exp_upb) %>% 
    mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
  
  excess_birth_year_month_df %>%
    filter(year>=2020) %>% 
    ggplot(aes(x = date, y = n_exc_mean/n_exp_mean, ymin = n_exc_lwb/n_exp_mean, ymax = n_exc_upb/n_exp_mean)) +
    geom_hline(yintercept=0,lty=2)+
    geom_ribbon(alpha = 0.2) +
    geom_line() 
  
  excess_birth_year_reg_df = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(dist_name,dist_id,year,draw) %>% 
    dplyr::summarise(n_pop = sum(n_pop),
                     n_pred = sum(n_pred),.groups="drop_last") %>% 
    dplyr::summarise(n_pop = n_pop[1],
                     n_exp_mean = mean(n_pred),
                     n_exp_lwb = quantile(n_pred,probs=0.025),
                     n_exp_upb = quantile(n_pred,probs=0.9725),.groups="drop") %>% 
    left_join(birth_year_reg_df %>% dplyr::select(year,dist_id,n_birth),
              by=c("year","dist_id")) %>% 
    dplyr::mutate(n_exc_mean = n_birth - n_exp_mean,
                  n_exc_lwb = n_birth- n_exp_lwb,
                  n_exc_upb = n_birth-n_exp_upb)
  #plots
  #load region location
  regions_sf <- st_read("data/boundary_data/Boundaries_K4_District_20260101.shp") #source: https://www.agvchapp.bfs.admin.ch/fr/boundaries?SnapshotDate=01.01.2018&Unit=BAE2018
  regions_sf = regions_sf[,c(1,3,7)]
  names(regions_sf)[c(1,2)] <- c("dist_name2", "dist_id")
  
  
  excess_birth_year_reg_df %>% 
    filter(year %in% 2017:2024) %>% 
    dplyr::mutate(rel_exc_mean = n_exc_mean/n_exp_mean) %>% 
    left_join(regions_sf %>% dplyr::mutate(dist_id=as.numeric(dist_id)), by = c("dist_id")) %>%
    st_as_sf() %>%
    ggplot(aes(fill = rel_exc_mean)) +
    geom_sf(color = "black") +
    scale_fill_gradient2(
      name = "Relative excess birth",
      low = "red",        # negative
      mid = "white",      # zero
      high = "green",     # positive
      midpoint = 0,
      labels = scales::percent_format(accuracy = 1)
    )+
    theme( legend.position = "bottom",
           legend.direction = "horizontal")+
    facet_wrap(year~.,ncol=2)
  
  
  if(FALSE){
    #check that dist_name corresponds (it should as both datasets used mun_df)
    pred_n_birth_reg_df %>% 
      left_join(birth_reg_df %>% dplyr::select(year,dist_id,dist_name2=dist_name,n_birth),
                by=c("year","dist_id")) %>% 
      filter(dist_name!=dist_name2)
  }
  
  
  
  
  ################################################################################
  #yearly estimates by region
  pred_n_birth_reg_df = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(dist_name,dist_id,year,draw) %>% 
    dplyr::summarise(n_pop = sum(n_pop),
                     n_pred = sum(n_pred),.groups="drop_last") %>% 
    dplyr::summarise(n_pop = n_pop[1],
                     n_exp_mean = mean(n_pred),
                     n_exp_lwb = quantile(n_pred,probs=0.025),
                     n_exp_upb = quantile(n_pred,probs=0.9725),.groups="drop")
  #yearly estimate nationally
  pred_n_birth_df = pred_n_birth_reg_draw_df %>% 
    #sum over age and month
    group_by(year,draw) %>% 
    dplyr::summarise(n_pop = sum(n_pop),
                     n_pred = sum(n_pred),.groups="drop_last") %>% 
    dplyr::summarise(n_pop = n_pop[1],
                     n_exp_mean = mean(n_pred),
                     n_exp_lwb = quantile(n_pred,probs=0.025),
                     n_exp_upb = quantile(n_pred,probs=0.9725),.groups="drop")
  
  if(FALSE){
    #national
    pred_n_birth_df %>% 
      ggplot(aes(x = year, y = n_exp_mean, ymin = n_exp_lwb, ymax = n_exp_upb)) +
      geom_ribbon( alpha = 0.2) +geom_line()
    pred_n_birth_df %>% 
      ggplot(aes(x = year, y = n_exp_mean/n_pop, ymin = n_exp_lwb/n_pop, ymax = n_exp_upb/n_pop)) +
      geom_ribbon( alpha = 0.2) +geom_line()
    
    #district
    df = pred_n_birth_reg_df %>% 
      filter(dist_id %in% sample(unique(pred_n_birth_reg_df$dist_id), 6)) %>% 
      left_join(pred_n_birth_df %>% dplyr::select(year,n_pop_nat=n_pop,n_exp_mean_nat=n_exp_mean),by="year")
    df %>% 
      ggplot(aes(x = year, y = n_exp_mean, ymin = n_exp_lwb, ymax = n_exp_upb)) +
      geom_ribbon(aes(fill=dist_name), alpha = 0.2) +
      geom_line(aes(col=dist_name)) +
      facet_grid(dist_name~.,scale="free_y")
    df %>% 
      ggplot(aes(x = year, y = n_exp_mean/n_pop, ymin = n_exp_lwb/n_pop, ymax = n_exp_upb/n_pop)) +
      geom_ribbon(aes(fill=dist_name), alpha = 0.2) +
      geom_line(aes(col=dist_name)) +
      geom_line(aes(y=n_exp_mean_nat/n_pop_nat),lty=2)+
      facet_grid(dist_name~.)
    pred_n_birth_reg_df %>% 
      ggplot(aes(x = year, y = n_exp_mean/n_pop)) +
      geom_line(aes(col=dist_name)) +
      expand_limits(y=0)+theme(legend.position = "none")
  } 
}
