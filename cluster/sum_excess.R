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
use.p_childless_v = if(cfg$filter_parity != "all") c(FALSE, TRUE) else FALSE

##########################################
#Load data
load(ifelse(last_year==2024,
            paste0(code_root_path,"savepoint/cleaned_cluster_df.RData"),
            paste0(code_root_path,"savepoint/cleaned2025_cluster_df.RData")))

##########################################
#Check stan diagnostics
standiag = readRDS(paste0(res_path, save.date,"_",mod_name,"_seedid",seed_id,"_standiag.RDS"))
if(!all(standiag$is.stan.ok)){
  stop("Results from stan model cannot be used due to pathological behaviours reported during the run, check diagnostic statistics.")
}

##########################################
#Summarise excess births (already saved, commented out temporarily)

#1) nationally
pred_n_birth_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_draw_df",".RDS"))
excess_birth_nat_res = summarise_excess_birth_nat(pred_n_birth_draw_df,
                                                  save.date, mod_name, seed_id, res_path)

#2) by district
pred_n_birth_reg_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_reg_draw_df",".RDS"))
excess_birth_reg_res = summarise_excess_birth_reg(pred_n_birth_reg_draw_df,
                                                  save.date, mod_name, seed_id, res_path)

#3) by citizenship (only if not already restricted to swiss or non-swiss)
if(length(cfg$filter_ctz)==2){
  pred_n_birth_ctz_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","pred_n_birth_ctz_draw_df",".RDS"))
  excess_birth_ctz_res = summarise_excess_birth_ctz(pred_n_birth_ctz_draw_df,
                                                    save.date, mod_name, seed_id, res_path)
}

#4) municipality level
for(use.p_childless in use.p_childless_v){
  excess_birth_year_mun_draw_df     = readRDS(paste0(res_path,save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_mun_draw_df",".RDS"))
  excess_birth_year_adj_mun_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_adj_mun_draw_df",".RDS"))
  excess_birth_year_adj2_mun_draw_df= readRDS(paste0(res_path,save.date,"_",mod_name,ifelse(use.p_childless,"_childless",""),"_","seedid",seed_id,"_","excess_birth_year_adj2_mun_draw_df",".RDS"))
  summarise_excess_birth_mun(excess_birth_year_adj_mun_draw_df,
                             excess_birth_year_adj2_mun_draw_df,
                             excess_birth_year_mun_draw_df,
                             save.date, paste0(mod_name,ifelse(use.p_childless,"_childless","")), seed_id, res_path)
}

#5) ctz region x canton
excess_birth_year_ctz_draw_df = readRDS(paste0(res_path,save.date,"_",mod_name,"_","seedid",seed_id,"_","excess_birth_year_ctn_ctzreg_draw_df",".RDS"))
excess_birth_ctzreg = summarise_excess_birth_ctzreg(excess_birth_year_ctz_draw_df, save.date, mod_name, seed_id, res_path)

##########################################
#Excess birth by ntiles
use.p_childless_v = if(cfg$filter_parity != "all") c(FALSE, TRUE) else FALSE
excess_by_ntiles(save.date, mod_name, seed_id,
                 use.p_childless_v,
                 new_pop_mun_df, rural_urban_df, pop_dens_df, sep_df3, childcare_institutions_df, vote_mun_df,
                 year_range = 2017:last_year,
                 res_path   = res_path)
