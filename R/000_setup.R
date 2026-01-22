

library(pacman)
pacman::p_load(ISOweek, lubridate, data.table, tidyfast, tidyr, dplyr,purrr,ggplot2,stringr,zoo,ppcor,progress,
               xml2)

#cmdstanr
library(cmdstanr)
cmdstan_path()

print(getwd())
print(grepl("ahauser6",getwd()))

if(!grepl("ahauser6",getwd())){
  set_cmdstan_path("C:/TEMP/.cmdstan/cmdstan-2.36.0")
  library(tidyverse)
  library(flextable)
  library(officer)
  library(readxl)
  library(scales)
  library(patchwork)
}

if(FALSE){#check cmdstan
  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  mod <- cmdstan_model(file)
  mod$exe_file()
  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
  
  fit <- mod$sample(
    data = data_list,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 500 # print update every 500 iters
  )
  
}

theme_set(theme_bw())

#load R files
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster|/manuscript")[[1]][1],"/")
path_functions = list.files(pattern="[.]R$", path=paste0(code_root_path,"/R/"), full.names=TRUE)
path_functions = path_functions[!grepl("000",path_functions)]
print(path_functions)
sapply(path_functions, source)

#controls
controls=list(load.encrypted.data=FALSE)

#file directories
data_folder = "L:/UNISANTE_DESS/S_SUMAC/DIVERS_PROJETS/03_data/kaspar_staub_birth_data/"



