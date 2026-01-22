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
source(paste0(code_root_path,"R/000_setup.R"))

args_all=(commandArgs(TRUE)) #args_all=c(c(5,rep(1,7)),"20241218")
args=as.numeric(unlist(args_all[1:8]))#args[9] should be simulation date
save.date=as.character(args_all[[9]])

print("print args")
print(args)
print(save.date)#save.date="20260122"

# Check if the folder exists, and create it if it doesn't
folder_path <- paste0(code_root_path,"results/", save.date)
if (!dir.exists(folder_path)) {
  dir.create(folder_path)
  cat("Folder created:", folder_path, "\n")
} else {
  cat("Folder already exists:", folder_path, "\n")
}

#model_name
mod_name = "mod1_param_1expgp_2fixed.stan"
mod_name = "mod3_1expgp_1periodic.stan"

#load data
birth_agg_df = readRDS(paste0(code_root_path,"cluster/cluster_data/birth_agg_df.RDS"))
pop_df =  readRDS(paste0(code_root_path,"cluster/cluster_data/pop_agg_df.RDS"))

#run model
res = cmdstan_fit_mod3(birth_agg_df, pop_df, stan_years = 2000:2024,
                                 mod_name =  mod_name,
                                 save_draw = TRUE, save.date = save.date,
                                 seed_id=123)

# res = cmdstan_fit_mod1_param(birth_agg_df, pop_df, stan_years = 2000:2024,
#                              mod_name =  mod_name,
#                              save_draw = TRUE, save.date = save.date,
#                              seed_id=123)

#save results
save(res, file=paste0(code_root_path,"results/",save.date,"/","res_",sub("\\.stan$", "",mod_name),".RData"))




