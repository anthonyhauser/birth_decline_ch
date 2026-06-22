##########################################
#load arguments from ubelix array
# args9="20231129"
# for(args1 in c(3)){
#   for(args2 in c(2)){
#     for(args3 in c(3)){
#       for(args4 in c(1,2,3)){
#         for(args5 in c(1)){
#  args=c(args1,args2,args3,args4,args5,NA,NA,NA,args9)

##########################################
#Setting up paths
is.sim.cluster=TRUE
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster")[[1]][1],"/")
setwd(code_root_path) # cmdstanr needs to run from project root to find compiled executables
source(paste0(code_root_path,"R/000_setup.R"))
code_root_path = paste0(code_root_path,"/") #add "/" because when running setup it save code_root_path without

args_all=(commandArgs(TRUE)) #args_all=c(c(3,1,rep(1,6)),"20271111")
args=as.numeric(unlist(args_all[1:8]))#args[9] should be simulation date
save.date=as.character(args_all[[9]])
l=args[1]
test_year = (2015:2023)[args[2]]

print("print args")
print(args)
print(save.date)

#########################################################################################
#load data
rundate = save.date
load(paste0(code_root_path,"/savepoint/ssd_apk_2019",if_else(use.gynecology,"","_nogyn"),".RData"))
ssd_mice_df = ssd2_df %>% dplyr::mutate(mice_id=1)

# Create results/cv_stan/rundate folder if it doesn't exist
cv_stan_path <- paste0(code_root_path,"/results/cv_stan/",rundate)
if(!dir.exists(cv_stan_path)) {
  dir.create(cv_stan_path, recursive = TRUE)
  cat("Folder created:", cv_stan_path, "\n")
}

mod_ids=c(1,9:13)#1:13
dist_ids=3:4
save_results=TRUE
use.gynecology=FALSE
seed_id = 1
region = c("CH01","CH02","CH03","CH04","CH05","CH06","CH07")

gp_id_df = data.frame(rho_age  = c(1, 1, 1, 1, 1, 0.5),
                      rho_year = c(1, 2, 0.5, 1, 2, 1),
                      alpha_gp = c(0.5, 0.5, 0.5, 1, 1, 0.5)) %>%
  dplyr::mutate(gp_id = row_number())

#define dist_id for mod_id (mod1-6: all, mod7-8: only 3)
dist_by_mod_df <- tibble::tibble(mod_id = mod_ids) %>%
  tidyr::crossing(dist_id = 1:4) %>%
  tidyr::crossing(gp_id = 0:(dim(gp_id_df)[1])) %>%
  dplyr::filter((mod_id %in% 1:6 & dist_id %in% c(4)) |
                  (mod_id %in% 7:8 & dist_id == 3) |
                  (mod_id %in% 9 & dist_id == 1) |
                  (mod_id %in% 10:13 & dist_id == 4)) %>%
  dplyr::filter((mod_id %in% c(1:9,13) & gp_id==0) |
                  (mod_id %in% 10:12 & gp_id>0)) %>%
  #dplyr::filter(dist_id %in% dist_ids) %>%
  dplyr::mutate(mod=paste0("loo_mod",mod_id,"_",dist_id,"_",gp_id))

# gp_id_df = data.frame(gp_id=1:4,
#                       rho = rep(c(1,2),each=2),
#                       alpha_gp = rep(0.5,2))



################################################################################
#check years
min_year = unlist(lapply(list(detapk_agg_df$year,apk_agg_df$year,ssd_mice_df$year),min))
max_year = unlist(lapply(list(detapk_agg_df$year,apk_agg_df$year,ssd_mice_df$year),max))
print(paste0("Years: ", max(min_year),"-",min(max_year)))
if(test_year>min(max_year)){
  stop("The year to predict is outside of the data year.")
}
detapk_agg_df = detapk_agg_df %>%
  filter(year>=max(min_year),year<=test_year)
apk_agg_df = apk_agg_df %>%
  filter(year>=max(min_year),year<=test_year)
ssd_mice_df = ssd_mice_df %>%
  filter(year>=max(min_year),year<=test_year)

#swiss data
#Load df -----------------------------------------------------------------------
N_age_group = length(unique(ssd_mice_df$age_group))
N_year = length(unique(ssd_mice_df$year))
N_reg = length(unique(ssd_mice_df$reg))
ssd_df = ssd_mice_df %>% #by age, sex, reg and year
  dplyr::mutate(age_id = as.numeric(age_group),
                sex_id = as.numeric(factor(sex)),
                age_sex_id = age_id + (sex_id -1)* N_age_group,
                reg_id = as.numeric(factor(reg,levels=region)),
                year_id = 1 + (as.numeric(year)-min(year)),
                age_year_id = age_id + (year_id -1)* N_age_group,
                year_reg_id = year_id + (reg_id -1) * N_year) %>%
  #aggregate over spec
  group_by(age_sex_id,age_id,sex_id,reg_id,year_id,age_group,sex,reg,year,year_reg_id,age_year_id,mice_id) %>%
  dplyr::summarise(n_vst=sum(n_vst),.groups="drop")
apk_agg_df = apk_agg_df %>%  #by reg and year (aggregated over doctors)
  dplyr::mutate(reg_id = as.numeric(factor(reg,levels=region)),
                year_id = 1 + (as.numeric(year)-min(year)),
                year_reg_id = year_id + (reg_id -1) * N_year)
detapk_agg_df = detapk_agg_df %>% #by age, sex, reg and year, filter from 2019 as not done yet unlike other datasets (because meth1 used 5-year data to estimate age-sex distribution)
  dplyr::mutate(age_id = as.numeric(age_group),
                sex_id = as.numeric(factor(sex)),
                age_sex_id = age_id + (sex_id -1)* N_age_group,
                reg_id = as.numeric(factor(reg,levels=region)),
                year_id = 1 + (as.numeric(year)-min(year)),
                year_reg_id = year_id + (reg_id -1) * N_year) %>%
  filter(!is.na(sex_id),!is.na(age_id))
#pop_df #not used at the moment

#Adapt -------------------------------------------------------------------------
#aggregate over chosen stratification (by default, age, sex, year and reg, but could be as fine as doctor-level if we want to adjust for doctor-level effect)
detapk_agg_df = detapk_agg_df %>%
  group_by(age_sex_id,reg_id,year_id,year_reg_id) %>%
  dplyr::summarise(n=sum(n_vst),.groups="drop") %>%
  left_join(ssd_df %>% dplyr::select(mice_id,age_sex_id,reg_id,year_id,n_ssd=n_vst),
            by=c("age_sex_id","year_id","reg_id"))

#aggregate over spec
apk_agg_df = apk_agg_df %>%
  group_by(reg_id,year_id) %>%
  dplyr::summarise(n_vst=sum(n_vst),.groups="drop")

#aggregate over age and sex (used in binomial to compare with apk_agg_df)
ssd_agg_df = ssd_df %>%
  group_by(reg_id,year_id,year_reg_id,mice_id) %>%
  dplyr::summarise(n=sum(n_vst),.groups="drop")

#Dummy variables for age, sex and for reg and year
age_sex_df = ssd_df %>% dplyr::select(age_id,sex_id,age_sex_id) %>% arrange(age_sex_id) %>% distinct()

#adapt sdd number using correction factor (14% + 7.5% of consultations not reported)
ssd_corr = 1/(1-0.14-0.075)
ssd_df$n_vst = ssd_df$n_vst * ssd_corr
ssd_agg_df$n = ssd_agg_df$n * ssd_corr

################################################################################
test_detapk_agg_df = detapk_agg_df %>% filter(year_id==test_year-max(min_year)+1)
test_apk_agg_df = apk_agg_df %>% filter(year_id==test_year-max(min_year)+1)
test_ssd_agg_df = ssd_agg_df %>% filter(year_id==test_year-max(min_year)+1)
test_ssd_df = ssd_df %>% filter(year_id==test_year-max(min_year)+1)

detapk_agg_df = detapk_agg_df %>% filter(year_id<test_year-max(min_year)+1)
apk_agg_df = apk_agg_df %>% filter(year_id<test_year-max(min_year)+1)
ssd_agg_df = ssd_agg_df %>% filter(year_id<test_year-max(min_year)+1)
ssd_df = ssd_df %>% filter(year_id<test_year-max(min_year)+1)

N_year = length(unique(detapk_agg_df$year_id))
age_year_df = cross_join(data.frame(age_id=1:N_age_group),data.frame(year_id=1:(N_year+1))) %>%
  dplyr::mutate( age_year_id = age_id + (year_id-1 )* N_age_group) %>%
  arrange(age_year_id)

################################################################################
#Stan model

#data: mice_id==1
data_list = list(N_age = length(unique(ssd_df$age_id)),
                 N_sex = length(unique(ssd_df$sex_id)),
                 N_age_sex =  length(unique(ssd_df$age_id)) *  length(unique(ssd_df$sex_id)),
                 N_reg = length(unique(ssd_df$reg_id)),
                 N_year = length(unique(ssd_df$year_id)),
                 N_year_reg = length(unique(ssd_df$year_reg_id)),
                 N_age_year = N_age_group * (N_year + 1),
                 C = dim(ssd_df %>% dplyr::select(age_sex_id) %>% distinct())[1],
                 N_ssd_agg = dim(ssd_agg_df %>% filter(mice_id==1))[1],
                 N_detapk = dim(detapk_agg_df %>% filter(age_sex_id==1,mice_id==1))[1],

                 n_ssd = ssd_df %>% filter(mice_id==1) %>%
                   dplyr::select(age_sex_id,reg_id,year_id,n_vst) %>%
                   arrange(age_sex_id,reg_id,year_id) %>%
                   pivot_wider(names_from=c("age_sex_id"),values_from = "n_vst") %>%
                   dplyr::select(-c("reg_id","year_id")) %>% as.matrix(),
                 n_ssd_agg = ssd_agg_df %>% filter(mice_id==1) %>% pull(n) %>% round(),

                 n_apk = apk_agg_df$n_vst,
                 n_detapk = detapk_agg_df %>% filter(mice_id==1) %>%
                   arrange(reg_id,year_id) %>% dplyr::select(-n_ssd,-year_reg_id) %>%
                   pivot_wider(names_from=c("age_sex_id"),values_from = "n") %>%
                   dplyr::select(-c("reg_id","year_id","mice_id")) %>% as.matrix(),
                 n_corr_ssd_agg = detapk_agg_df %>% filter(mice_id==1) %>% #corresponding ssd aggegrated counts
                   arrange(age_sex_id,reg_id,year_id) %>% dplyr::select(-n,-year_reg_id) %>%
                   pivot_wider(names_from=c("age_sex_id"),values_from = "n_ssd") %>%
                   dplyr::select(-c("reg_id","year_id","mice_id")) %>% as.matrix(),

                 n_detapk_agg = detapk_agg_df %>% filter(mice_id==1) %>%
                   group_by(reg_id,year_id) %>% dplyr::summarise(n=sum(n),.groups="drop") %>%
                   arrange(reg_id,year_id) %>% pull(n),

                 mat_year_reg_id = matrix( 1:(N_year * N_reg), nrow = N_year,  ncol = N_reg),
                 mat_age_year_id = matrix( 1:(N_age_group * (N_year+1)), nrow = N_age_group,  ncol = N_year+1),

                 age_id = age_sex_df$age_id,
                 sex_id = age_sex_df$sex_id,
                 age_sex_id = age_sex_df$age_sex_id,
                 x_gp = age_year_df %>% dplyr::select(age_id,year_id) %>%
                   dplyr::mutate(age_id = (age_id-1)/(N_age_group-1),
                                 year_id = (year_id)/(N_year)),
                 x_gp2 = (1:(N_year+1)-1)/(N_year),

                 ssd_agg_reg_id = ssd_agg_df %>% filter(mice_id==1) %>% pull(reg_id) %>% array(.,dim=N_reg*N_year),
                 ssd_agg_year_id =  ssd_agg_df %>% filter(mice_id==1) %>% pull(year_id),
                 ssd_agg_year_reg_id =  ssd_agg_df %>% filter(mice_id==1) %>% pull(year_reg_id),
                 detapk_reg_id = detapk_agg_df %>% filter(mice_id==1,age_sex_id==1) %>%
                   arrange(reg_id,year_id) %>% pull(reg_id),
                 detapk_year_id = detapk_agg_df %>% filter(mice_id==1,age_sex_id==1) %>%
                   arrange(reg_id,year_id) %>% pull(year_id),
                 detapk_year_reg_id = detapk_agg_df %>% filter(mice_id==1,age_sex_id==1) %>%
                   arrange(year_reg_id) %>% pull(year_reg_id),

                 #test data
                 N_ssd_agg_test = dim(test_ssd_agg_df %>% filter(mice_id==1))[1],
                 N_detapk_test = dim(test_detapk_agg_df %>% filter(age_sex_id==1,mice_id==1))[1],

                 test_year_id = test_year-max(min_year), #take the max for mod1

                 ssd_agg_reg_id_test = test_ssd_agg_df %>% filter(mice_id==1) %>% pull(reg_id) %>% array(.,dim=N_reg),
                 detapk_reg_id_test = test_detapk_agg_df %>% filter(mice_id==1,age_sex_id==1) %>%
                   arrange(reg_id,year_id) %>% pull(reg_id)  %>% array(.,dim=N_reg),

                 n_ssd_test = test_ssd_df %>% filter(mice_id==1) %>%
                   dplyr::select(age_sex_id,reg_id,year_id,n_vst) %>%
                   arrange(age_sex_id,reg_id,year_id) %>%
                   pivot_wider(names_from=c("age_sex_id"),values_from = "n_vst") %>%
                   dplyr::select(-c("reg_id","year_id")) %>% as.matrix(),
                 n_ssd_agg_test = test_ssd_agg_df %>% filter(mice_id==1) %>% pull(n) %>% round() %>% array(.,dim=N_reg),
                 n_corr_ssd_agg_test = test_detapk_agg_df %>% filter(mice_id==1) %>% #corresponding ssd aggegrated counts
                   arrange(age_sex_id,reg_id,year_id) %>% dplyr::select(-n,-year_reg_id) %>%
                   pivot_wider(names_from=c("age_sex_id"),values_from = "n_ssd") %>%
                   dplyr::select(-c("reg_id","year_id","mice_id")) %>% as.matrix(),

                 n_apk_test = test_apk_agg_df$n_vst %>% array(.,dim=N_reg),
                 n_detapk_test = test_detapk_agg_df %>% filter(mice_id==1) %>%
                   arrange(reg_id,year_id) %>% dplyr::select(-n_ssd,-year_reg_id) %>%
                   pivot_wider(names_from=c("age_sex_id"),values_from = "n") %>%
                   dplyr::select(-c("reg_id","year_id","mice_id")) %>% as.matrix(),

                 n_detapk_agg_test = test_detapk_agg_df %>% filter(mice_id==1) %>%
                   group_by(reg_id,year_id) %>% dplyr::summarise(n=sum(n),.groups="drop") %>%
                   arrange(reg_id,year_id) %>% pull(n) %>% array(.,dim=N_reg),

                 p_alpha = c(-4,2),
                 p_alpha_gp = c(0,2),
                 alpha_gp = 2,
                 p_b_sex = c(0,1),
                 p_b = c(-4,2),
                 p_b_year = 1,
                 p_sigma_reg=1,
                 p_sigma_year=0.5,
                 p_sigma_year_reg = 1,
                 p_phi=c(6,2),
                 p_rho = c(5,5), #p_phi=c(1/100,0)   #p_phi=c(1,1)


                 mu = -4,
                 rho_age  = 0.5,
                 rho_year  = 0.5)

#Run models

#model arguments
dist_by_mod_df_l = dist_by_mod_df[l,]
print(dist_by_mod_df_l)
i = dist_by_mod_df_l$mod_id
j = dist_by_mod_df_l$dist_id
k = dist_by_mod_df_l$gp_id
mod = paste0("loo_mod",i,"_",j,"_",k)

# skip if results already exist
standiag_path = paste0(code_root_path,"results/cv_stan/",rundate,"/",rundate,"_",mod,"_standiag",if_else(use.gynecology,"","_nogyn"),"_seedid1_testyear",test_year,".RDS")
if(file.exists(standiag_path)){
  cat("Results already exist for", mod, "test_year", test_year, "- skipping.\n")
  quit(save="no", status=0)
}

print(i)

data_list$dist_id = j
if(k>0){
  data_list$alpha_gp = gp_id_df$alpha_gp[k]
  data_list$rho_age  = gp_id_df$rho_age[k]
  data_list$rho_year = gp_id_df$rho_year[k]
}
mod_cmdstan <- cmdstan_model(paste0(code_root_path,"stan/loo_stan/loo_mod",i,".stan"))
print("Model loaded.")

if(FALSE){
  mod_rstan <- stan_model(paste0("stan/loo_stan/loo_mod",i,".stan"))
  data_list$dist_id = j
  fit_i <- sampling(mod_rstan, data = data_list,
                    #init = function() list(phi = structure(rnorm(1,data_list$p_phi[1],data_list$p_phi[2]),dim=c(1))),
                    control = list(adapt_delta=0.95),#0.99
                    seed = seed_id,
                    chains = 4,
                    cores = 4,
                    iter = 2000,
                    warmup = 1000,
                    refresh = 100,
                    save_warmup = TRUE)
  summary(fit_i,pars="lp__")$summary
}

#model
num_divergent=1
rhat=2
ess_bulk=0
ess_tail=0
n_iter = 0
while(n_iter<5 & (num_divergent>0 | rhat>1.1 | ess_bulk<50 | ess_tail<50)){
  n_iter = n_iter+1
  seed_id = n_iter
  fit_i <- mod_cmdstan$sample(data = data_list,
                              adapt_delta = 0.95,
                              seed = seed_id,
                              chains = 4,
                              parallel_chains = 4,
                              iter_warmup = 1000,
                              iter_sampling = 1000,
                              refresh = 100,
                              save_warmup = TRUE,
                              sig_figs = 18)

  # Diagnostics
  diags <- fit_i$diagnostic_summary()
  dr = fit_i$draws()
  diag_tbl <- posterior::summarize_draws(dr, "rhat", "ess_bulk", "ess_tail")

  num_divergent <- sum(diags$num_divergent)
  num_max_treedepth <- sum(diags$num_max_treedepth)
  rhat = max(diag_tbl[,"rhat"],na.rm=TRUE)
  ess_bulk = min(diag_tbl[,"ess_bulk"],na.rm=TRUE)
  ess_tail = min(diag_tbl[,"ess_tail"],na.rm=TRUE)

  # EBFMI
  ebfmi = min(diags$ebfmi)

  stan_diag_i = data.frame(num_successful_chains = 4,
                           num_divergent = num_divergent,
                           num_max_treedepth = num_max_treedepth,
                           ebfmi = ebfmi,
                           rhat = rhat,
                           ess_bulk = ess_bulk,
                           ess_tail = ess_tail,
                           n_iter = n_iter,
                           time = max(fit_i$time()$chains$total),
                           mod = mod,
                           mod_id = i,
                           dist_id = j,
                           seed_id = seed_id,
                           test_year = test_year)
  print(stan_diag_i)
}
#save fit and diagnostic
if(save_results){
  fit_i$save_object(file = paste0(code_root_path,"results/cv_stan/",rundate,"/",rundate,"_",mod,"_stanfit",if_else(use.gynecology,"","_nogyn"),"_seedid",seed_id,"_testyear",test_year,".RDS"))
  saveRDS(stan_diag_i, file = paste0(code_root_path,"results/cv_stan/",rundate,"/",rundate,"_",mod,"_standiag",if_else(use.gynecology,"","_nogyn"),"_seedid",seed_id,"_testyear",test_year,".RDS"))
}

#stan outcomes
cv_stan_outcomes_i = cv_stan_outcomes(fit_i,
                                      mod,
                                      seed_id,
                                      ssd_df, ssd_agg_df, apk_agg_df, detapk_agg_df,
                                      test_ssd_df, test_ssd_agg_df, test_apk_agg_df, test_detapk_agg_df,
                                      rundate = rundate, test_year)
print("Done.")

