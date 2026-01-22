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
                          ess_tail = fit_summary$ess_tail %>% min(),
                          ess_bulk = fit_summary$ess_bulk %>% min()) %>% 
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
