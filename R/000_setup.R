library(pacman)
pacman::p_load(ISOweek, lubridate, data.table, tidyfast, tidyr, dplyr,purrr,ggplot2,stringr,zoo,ppcor,progress,
               xml2,
               boot,janitor)

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
  library(sf)
  library(foreign)
  library(spdep)
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
code_root_path = paste0(strsplit(wd, split="reports|/cluster|/manuscript")[[1]][1],"/")
path_functions = list.files(pattern="[.]R$", path=paste0(code_root_path,"/R/"), full.names=TRUE)
path_functions = path_functions[!grepl("000",path_functions)]
print(path_functions)
sapply(path_functions, source)

print("----")
print(code_root_path)
print(path_functions)


#controls
controls=list(load.encrypted.data=FALSE)

#file directories
data_folder = "L:/UNISANTE_DESS/S_SUMAC/DIVERS_PROJETS/03_data/kaspar_staub_birth_data/"

#canton names
ctn_map <- tibble::tribble( ~ctn_abbr, ~region,
                            "CH",      "Suisse",
                            "ZH",      "Zürich",
                            "BE",      "Bern / Berne",
                            "LU",      "Luzern",
                            "UR",      "Uri",
                            "SZ",      "Schwyz",
                            "OW",      "Obwalden",
                            "NW",      "Nidwalden",
                            "GL",      "Glarus",
                            "ZG",      "Zug",
                            "FR",      "Fribourg / Freiburg",
                            "SO",      "Solothurn",
                            "BS",      "Basel-Stadt",
                            "BL",      "Basel-Landschaft",
                            "SH",      "Schaffhausen",
                            "AR",      "Appenzell Ausserrhoden",
                            "AI",      "Appenzell Innerrhoden",
                            "SG",      "St. Gallen",
                            "GR",      "Graubünden / Grigioni / Grischun",
                            "AG",      "Aargau",
                            "TG",      "Thurgau",
                            "TI",      "Ticino",
                            "VD",      "Vaud",
                            "VS",      "Valais / Wallis",
                            "NE",      "Neuchâtel",
                            "GE",      "Genève",
                            "JU",      "Jura",
                            "VS",      "Valais",
                            "FR",      "Fribourg",
                            "GR",      "Graubünden",
                            "BE",      "Bern",
                            NA,        "Sans indication")


canton_df <- data.frame(
  ctn_name = c("Zurich", "Berne", "Lucerne", "Uri", "Schwyz", "Obwald", "Nidwald", "Glaris", 
               "Zoug", "Fribourg", "Soleure", "Bâle-Ville", "Bâle-Campagne", "Schaffhouse", 
               "Appenzell Rh.-Ext.", "Appenzell Rh.-Int.", "Saint-Gall", "Grisons", 
               "Argovie", "Thurgovie", "Tessin", "Vaud", "Valais", "Neuchâtel", "Genève", "Jura"),
  ctn = c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", 
          "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", 
          "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", 
          "GE", "JU"),
  ctn_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
             17, 18, 19, 20, 21, 22, 23, 24, 25, 26)) %>%
  left_join(data.frame(ctn = c("VD","VS","GE","BE","FR","SO","NE","JU","BS","BL","AG","ZH","GL",
                               "SH","AR","AI","SG","GR","TG","LU","UR","SZ","OW","NW","ZG","TI"),
                       NUTS2_id = c(rep(c(1:7),c(3,5,3,1,7,6,1))),
                       NUTS2_name = rep(c("Lake Geneva","Mittelland","Northwest","Zurich","Eastern","Central","Ticino"),
                                        c(3,5,3,1,7,6,1))))

#mun_ids to aggregate in Appenzell
mun_ids_to_agg <- c(3101, 3102, 3104, 3112)


#load sf data
#districts
regions_sf <- st_read(paste0(code_root_path,"data/boundary_data/Boundaries_K4_District_20260101.shp")) #source: https://www.agvchapp.bfs.admin.ch/fr/boundaries?SnapshotDate=01.01.2018&Unit=BAE2018
regions_sf = regions_sf[,c(1,3,7)]
names(regions_sf)[c(1,2)] <- c("dist_name2", "dist_id")

#municipality
mun_sf <- st_read(paste0(code_root_path,"data/boundary_data/Boundaries_K4_Commune_20260101.shp"))
mun_sf = mun_sf[,c(1,3,7)]
names(mun_sf)[c(1,2)] <- c("mun_name2", "mun_id")

#aggregate four municipalities from Appenzel district
new_mun_sf = bind_rows(mun_sf %>%
                         filter(mun_id %in% mun_ids_to_agg) %>%
                         dplyr::summarize(geometry = st_union(geometry)) %>%
                         st_cast("MULTIPOLYGON") %>%
                         dplyr::mutate(mun_id = 31010,
                                       mun_name2 = "Appenzell (aggregated)"),
                       mun_sf %>%
                         filter(!(mun_id %in% mun_ids_to_agg)))

#lake
lake_sf <- st_read(paste0(code_root_path,"data/boundary_data/swissTLMRegio_Product_LV95/Hydrography/swissTLMRegio_Lake.shp"))
mun_buffer <- st_buffer(mun_sf, dist = 1000)  # distance in CRS units (LV95 ~ meters)
# keep lakes that intersect any municipality buffer
lake_sf <- lake_sf %>% 
  st_filter(mun_buffer, .predicate = st_intersects)


