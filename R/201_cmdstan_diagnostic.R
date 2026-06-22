cmdstan_diagnostic = function(fit,var=NULL){
  if(is.null(var)){
    fit_summary=fit$summary()
  }else{
    fit_summary=fit$summary(variables = var)
  }

  chains = fit$time()$chains %>% filter(!is.na(total)) %>% pull(chain_id) %>% .[1:2]
  stan_diag =  data.frame(time = fit$time()$chains %>% filter(!is.na(total)) %>% pull(total) %>% max(),
                          num_successful_chains =  fit$diagnostic_summary()$num_divergent %>% length(),
                          num_divergent = fit$diagnostic_summary()$num_divergent %>% sum(),#fit$sampler_diagnostics()
                          num_max_treedepth = fit$diagnostic_summary()$num_max_treedepth %>% sum(),
                          ebfmi = fit$diagnostic_summary()$ebfmi %>% min(),
                          rhat = fit_summary %>% filter(!is.na(rhat)) %>% pull(rhat) %>% max(),
                          ess_tail = fit_summary$ess_tail %>% min(.,na.rm = TRUE),
                          ess_bulk = fit_summary$ess_bulk %>% min(.,na.rm = TRUE)) %>% 
    dplyr::mutate(is.stan.ok = num_successful_chains>=4 & num_divergent==0 & ebfmi>=0.3 & rhat<1.1)
  return(stan_diag)
}

rstan_diagnostic = function(fit, var=NULL){
  if(is.null(var)){
    fit_summary = rstan::summary(fit)$summary %>% as.data.frame() %>% dplyr::mutate(variable=rownames(.))
  } else {
    fit_summary = rstan::summary(fit, pars=var)$summary %>% as.data.frame() %>% dplyr::mutate(variable=rownames(.))
  }

  sp = rstan::get_sampler_params(fit, inc_warmup=FALSE)
  num_divergent     = sum(sapply(sp, function(x) sum(x[,"divergent__"])))
  max_treedepth_set = fit@stan_args[[1]]$control$max_treedepth
  if(is.null(max_treedepth_set)) max_treedepth_set = 10
  num_max_treedepth = sum(sapply(sp, function(x) sum(x[,"treedepth__"] >= max_treedepth_set)))
  ebfmi_vals        = sapply(sp, function(x){ e = x[,"energy__"]; mean(diff(e)^2)/var(e) })

  draws_arr = posterior::as_draws_array(fit)
  ess_tail_val = posterior::ess_tail(draws_arr) %>% min(na.rm=TRUE)
  ess_bulk_val = posterior::ess_bulk(draws_arr) %>% min(na.rm=TRUE)

  stan_diag = data.frame(
    time                = rstan::get_elapsed_time(fit) %>% rowSums() %>% max(),
    num_successful_chains = length(sp),
    num_divergent         = num_divergent,
    num_max_treedepth     = num_max_treedepth,
    ebfmi                 = min(ebfmi_vals),
    rhat                  = fit_summary %>% dplyr::filter(!is.na(Rhat)) %>% dplyr::pull(Rhat) %>% max(),
    ess_tail              = ess_tail_val,
    ess_bulk              = ess_bulk_val
  ) %>%
    dplyr::mutate(is.stan.ok = num_successful_chains>=4 & num_divergent==0 & ebfmi>=0.3 & rhat<1.1)
  return(stan_diag)
}

cmdstan_est_by_chain = function(fit, var, chains=1:4){
  d=fit$draws(variables = var)
  n_iter_per_chain = d[,1,1] %>% length()
  
  draws = as.data.frame(ftable(d)) %>% #as.data.frame(ftable(d[,,grepl(var,dimnames(d)[[3]])])) %>% 
    dplyr::mutate(chain = as.numeric(as.character(chain)),
                  iteration = as.numeric(as.character(iteration)),
                  iter=iteration+n_iter_per_chain*(chain-1)) %>% 
    dplyr::filter(chain %in% chains) %>% 
    dplyr::select(chain,iter,var=variable,values=Freq) 
  
  return(draws %>% 
           group_by(var,chain) %>% 
           dplyr::summarise( mean   = mean(values),
                             median = median(values),
                             q2.5   = quantile(values, 0.025),
                             q97.5  = quantile(values, 0.975)))
}
