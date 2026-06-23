##########################################
#Setting up paths
is.sim.cluster = TRUE
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster")[[1]][1],"/")
source(paste0(code_root_path,"R/000_setup.R"))

args_all  = commandArgs(TRUE)
job_id    = as.numeric(args_all[1])
save.date = as.character(args_all[2])
last_year = as.numeric(args_all[3])

print(paste("job_id:", job_id))
print(paste("save.date:", save.date))
print(paste("last_year:", last_year))

##########################################
#Job configurations (5 models)
# 1: all ctz,     all parity    (main)
# 2: all ctz,     first parity
# 3: all ctz,     second parity
# 4: swiss only,  all parity
# 5: non-swiss,   all parity

configs = list(
  list(filter_ctz = c("swiss","non-swiss"), filter_parity = "all",    stan_year_start = 2000),
  list(filter_ctz = c("swiss","non-swiss"), filter_parity = "first",  stan_year_start = 2005),
  list(filter_ctz = c("swiss","non-swiss"), filter_parity = "second", stan_year_start = 2005),
  list(filter_ctz = "swiss",               filter_parity = "all",    stan_year_start = 2000),
  list(filter_ctz = "non-swiss",           filter_parity = "all",    stan_year_start = 2000)
)

cfg = configs[[job_id]]

##########################################
#Reconstruct mod_name and res_path (same logic as cmstan_fit_mod5)
mod_name = "mod8"
mod_name = paste0(mod_name, ifelse(length(cfg$filter_ctz)==2, "", paste0("_", cfg$filter_ctz)))
mod_name = if_else(cfg$filter_parity=="all", mod_name, paste0(mod_name,"_",cfg$filter_parity))
mod_name = if_else(last_year!=2025, mod_name, paste0(mod_name,"_",last_year))
res_path = paste0(code_root_path, "results/", ifelse(last_year==2025, "2025/", ""))
seed_id  = 1

##########################################
#Load model data
load(ifelse(last_year==2024,
            paste0(code_root_path,"savepoint/cleaned_cluster_df.RData"),
            paste0(code_root_path,"savepoint/cleaned2025_cluster_df.RData")))

##########################################
#Load stan fit and stan_df
fit     = readRDS(paste0(code_root_path,"results/cmdstan_draw/",save.date,"_",mod_name,"_seedid",seed_id,".RDS"))
stan_df = readRDS(paste0(res_path,mod_name,"_standf.RDS"))

##########################################
#use.p_childless: TRUE only for parity models
use.p_childless_v = if(cfg$filter_parity != "all") c(FALSE, TRUE) else FALSE

##########################################
#1. Distribute over district (and ctz swiss/non-swiss for job 1)
list_pred_n_birth_draw_list = get_pred_birth_draw_by_dist(fit,
                                                          stan_df,
                                                          pop_dist_df,
                                                          new_birth_df,
                                                          n_draw_subset = 100,
                                                          save.date,
                                                          mod_name,
                                                          seed_id,
                                                          res_path = res_path)

##########################################
#2. Distribute over municipality
for(use.p_childless in use.p_childless_v){
  get_pred_birth_draw_by_mun(fit,
                             stan_df,
                             new_pop_mun_df,
                             new_birth_df,
                             new_mun_sf   = new_mun_sf,
                             n_draw_subset = 100,
                             save.date,
                             mod_name,
                             use.p_childless = use.p_childless,
                             seed_id,
                             res_path = res_path)
}

##########################################
#3. Distribute over ctz region x canton
get_pred_birth_draw_by_ctzreg(fit,
                               stan_df,
                               pop_detctz_df,
                               new_birth_df,
                               n_draw_subset = 100,
                               save.date,
                               mod_name,
                               seed_id,
                               res_path = res_path)
