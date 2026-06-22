##########################################
#Setting up paths
is.sim.cluster = TRUE
quick_run = TRUE
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster")[[1]][1],"/")
source(paste0(code_root_path,"R/000_setup.R"))

args_all = commandArgs(TRUE)
job_id   = as.numeric(args_all[1])
save.date = as.character(args_all[2])

print(paste("job_id:", job_id))
print(paste("save.date:", save.date))

##########################################
#Job configurations (5 models)
# 1: all ctz,     all parity    (main)
# 2: all ctz,     first parity
# 3: all ctz,     second parity
# 4: swiss only,  all parity
# 5: non-swiss,   all parity

configs = list(
  list(filter_ctz = c("swiss","non-swiss"), filter_parity = "all",    stan_year_start = 2000, birth_data = "birth_agg_df.RDS"),
  list(filter_ctz = c("swiss","non-swiss"), filter_parity = "first",  stan_year_start = 2005, birth_data = "birth_agg_first_df.RDS"),
  list(filter_ctz = c("swiss","non-swiss"), filter_parity = "second", stan_year_start = 2005, birth_data = "birth_agg_second_df.RDS"),
  list(filter_ctz = "swiss",               filter_parity = "all",    stan_year_start = 2005, birth_data = "birth_agg_df.RDS"),
  list(filter_ctz = "non-swiss",           filter_parity = "all",    stan_year_start = 2005, birth_data = "birth_agg_df.RDS")
)

cfg = configs[[job_id]]

##########################################
#Load data
pop_df       = readRDS(paste0(code_root_path,"cluster/cluster_data/pop_agg_df.RDS"))
birth_agg_df = readRDS(paste0(code_root_path,"cluster/cluster_data/",cfg$birth_data))

##########################################
#Run model
last_year = max(birth_agg_df$year)

res = cmstan_fit_mod5(pop_df, birth_agg_df,
                      mod_name          = "mod8",
                      stan_years        = cfg$stan_year_start:last_year,
                      effect_on_age_shift = "cal_year",
                      save_draw         = TRUE,
                      save.date         = save.date,
                      filter_parity     = cfg$filter_parity,
                      filter_ctz        = cfg$filter_ctz,
                      seed_id           = 1,
                      quick_run         = quick_run)
