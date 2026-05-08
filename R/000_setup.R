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
path_functions = path_functions[!grepl("000_setup",path_functions)]
print(path_functions)
sapply(path_functions, source)

print("----")
print(code_root_path)
print(path_functions)


#controls
controls=list(load.encrypted.data=FALSE)

#file directories
data_folder = "L:/UNISANTE_DESS/S_SUMAC/DIVERS_PROJETS/03_data/kaspar_staub_birth_data/"

#variable definitions
explanatory_var_df <- data.frame(
  explanatory_var = c(
    "pop_dens_building",
    "pop_dens",
    "pop_dens_tot",
    "prop_infrastructure",
    "prop_building",
    "prop_individual_houses",
    "rel_empl_mean",
    "sep",
    "votation_paternity_leave",
    "votation_reduce_childcare_tax",
    "votation_family_tax_education_support",
    "votation_womens_suffrage"
  ),
  explanatory_var2 = c(
    "Population density",
    "Population density (productive area)",  # Densité de la population (surface productive) 2024,
    "Population density (total area)",  # Densité de la population (surface totale) 2024,
    "Share of housing and infrastructure land use",  # Part des surfaces d'habitat et d'infrastructure dans la superficie totale (%)
    "Share of built-up areas in total land area",  # Part des aires de bâtiments dans la superficie totale (%)
    "Share of single-family houses",  # Part des maisons individuelles 2024
    "Childcare service density",
    "Socio-economic position (SEP)",
    "Share of votes in favour of paternity leave extension (2020)",
    "Share of votes in favour of childcare tax deduction (2020)",
    "Share of votes in favour of family tax and education allowance initiative (2015)",
    "Share of votes in favour of women's suffrage (1971)"
  )
)

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

#cantons
ctn_sf <- st_read(paste0(code_root_path,"data/boundary_data/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp")) #source: https://www.agvchapp.bfs.admin.ch/fr/boundaries?SnapshotDate=01.01.2018&Unit=BAE2018
ctn_sf = ctn_sf[,c("NAME","geometry")]
link_ctn_df <- tibble::tibble( NAME = c("Genève","Thurgau","Valais","Aargau","Schwyz","Zürich","Obwalden","Fribourg","Glarus","Uri",
           "Nidwalden","Solothurn","Appenzell Ausserrhoden","Jura","Graubünden","Vaud","Luzern","Ticino",
           "Zug","Basel-Landschaft","St. Gallen","Schaffhausen","Bern","Basel-Stadt","Neuchâtel","Appenzell Innerrhoden"),
  ctn_abbr = c("GE","TG","VS","AG","SZ","ZH","OW","FR","GL","UR",
               "NW","SO","AR","JU","GR","VD","LU","TI","ZG","BL","SG","SH","BE","BS","NE","AI"))
ctn_sf = ctn_sf %>% 
  left_join(link_ctn_df, by = "NAME") %>% 
  dplyr::select(ctn_abbr, geometry)

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


#
ctz_map <- tribble(
  ~ctz_name, ~ctz_region,
  "Suisse", "Switzerland",
  "Albanie", "Eastern Europe",
  "Andorre", "Western Europe",
  "Belgique", "Western Europe",
  "Bulgarie", "Eastern Europe",
  "Danemark", "Western Europe",
  "Allemagne", "Western Europe",
  "Finlande", "Western Europe",
  "France", "Western Europe",
  "Grèce", "Eastern Europe",
  "Royaume-Uni", "Western Europe",
  "Irlande", "Western Europe",
  "Islande", "Western Europe",
  "Italie", "Western Europe",
  "Liechtenstein", "Western Europe",
  "Luxembourg", "Western Europe",
  "Malte", "Western Europe",
  "Monaco", "Western Europe",
  "Pays-Bas", "Western Europe",
  "Norvège", "Western Europe",
  "Autriche", "Western Europe",
  "Pologne", "Eastern Europe",
  "Portugal", "Western Europe",
  "Roumanie", "Eastern Europe",
  "Saint-Marin", "Western Europe",
  "Suède", "Western Europe",
  "Espagne", "Western Europe",
  "Türkiye", "Eastern Europe",
  "Hongrie", "Eastern Europe",
  "Cité du Vatican", "Western Europe",
  "Chypre", "Western Europe",
  "Slovaquie", "Eastern Europe",
  "Tchéquie", "Eastern Europe",
  "Serbie", "Eastern Europe",
  "Croatie", "Eastern Europe",
  "Slovénie", "Eastern Europe",
  "Bosnie et Herzégovine", "Eastern Europe",
  "Monténégro", "Eastern Europe",
  "Macédoine du Nord", "Eastern Europe",
  "Kosovo", "Eastern Europe",
  "Estonie", "Eastern Europe",
  "Lettonie", "Eastern Europe",
  "Lituanie", "Eastern Europe",
  "Moldova", "Eastern Europe",
  "Russie", "Eastern Europe",
  "Ukraine", "Eastern Europe",
  "Bélarus", "Eastern Europe",
  "Guinée équatoriale", "Africa",
  "Éthiopie", "Africa",
  "Djibouti", "Africa",
  "Algérie", "Africa",
  "Angola", "Africa",
  "Botswana", "Africa",
  "Burundi", "Africa",
  "Bénin", "Africa",
  "Côte d'Ivoire", "Africa",
  "Gabon", "Africa",
  "Gambie", "Africa",
  "Ghana", "Africa",
  "Guinée-Bissau", "Africa",
  "Guinée", "Africa",
  "Cameroun", "Africa",
  "Cabo Verde", "Africa",
  "Kenya", "Africa",
  "Comores", "Africa",
  "Congo (Brazzaville)", "Africa",
  "Congo (Kinshasa)", "Africa",
  "Lesotho", "Africa",
  "Libéria", "Africa",
  "Libye", "Africa",
  "Madagascar", "Africa",
  "Malawi", "Africa",
  "Mali", "Africa",
  "Maroc", "Africa",
  "Mauritanie", "Africa",
  "Maurice", "Africa",
  "Mozambique", "Africa",
  "Niger", "Africa",
  "Nigéria", "Africa",
  "Burkina Faso", "Africa",
  "Zimbabwe", "Africa",
  "Rwanda", "Africa",
  "Zambie", "Africa",
  "Sao Tomé-et-Principe", "Africa",
  "Sénégal", "Africa",
  "Seychelles", "Africa",
  "Sierra Leone", "Africa",
  "Somalie", "Africa",
  "Afrique du Sud", "Africa",
  "Soudan", "Africa",
  "Namibie", "Africa",
  "Eswatini", "Africa",
  "Tanzanie", "Africa",
  "Togo", "Africa",
  "Tchad", "Africa",
  "Tunisie", "Africa",
  "Ouganda", "Africa",
  "Égypte", "Africa",
  "République centrafricaine", "Africa",
  "Érythrée", "Africa",
  "Soudan du Sud", "Africa",
  "Sahara Occidental", "Africa",
  "Argentine", "South America",
  "Bahamas", "North America",
  "Barbade", "North America",
  "Bolivie", "South America",
  "Brésil", "South America",
  "Chili", "South America",
  "Costa Rica", "Central America",
  "République dominicaine", "Central America",
  "Équateur", "South America",
  "El Salvador", "Central America",
  "Guatemala", "Central America",
  "Guyana", "South America",
  "Haïti", "Central America",
  "Belize", "Central America",
  "Honduras", "Central America",
  "Jamaïque", "Central America",
  "Canada", "North America",
  "Colombie", "South America",
  "Cuba", "Central America",
  "Mexique", "Central America",
  "Nicaragua", "Central America",
  "Panama", "Central America",
  "Paraguay", "South America",
  "Pérou", "South America",
  "Suriname", "South America",
  "Trinité-et-Tobago", "Central America",
  "Uruguay", "South America",
  "Venezuela", "South America",
  "États-Unis", "North America",
  "Dominique", "Central America",
  "Grenade", "Central America",
  "Antigua-et-Barbuda", "Central America",
  "Sainte-Lucie", "Central America",
  "Saint-Vincent-et-les Grenadines", "Central America",
  "Saint-Kitts-et-Nevis", "Central America",
  "Afghanistan", "Asia",
  "Bahreïn", "Asia",
  "Bhoutan", "Asia",
  "Brunéi Darussalam", "Asia",
  "Myanmar", "Asia",
  "Sri Lanka", "Asia",
  "Taïwan (Taipei chinois)", "Asia",
  "Chine", "Asia",
  "Inde", "Asia",
  "Indonésie", "Asia",
  "Irak", "Asia",
  "Iran", "Asia",
  "Israël", "Asia",
  "Japon", "Asia",
  "Yémen", "Asia",
  "Jordanie", "Asia",
  "Cambodge", "Asia",
  "Qatar", "Asia",
  "Koweït", "Asia",
  "Laos", "Asia",
  "Liban", "Asia",
  "Malaisie", "Asia",
  "Maldives", "Asia",
  "Oman", "Asia",
  "Mongolie", "Asia",
  "Népal", "Asia",
  "Corée (Nord)", "Asia",
  "Émirats arabes unis", "Asia",
  "Pakistan", "Asia",
  "Philippines", "Asia",
  "Arabie saoudite", "Asia",
  "Singapour", "Asia",
  "Corée (Sud)", "Asia",
  "Syrie", "Asia",
  "Thaïlande", "Asia",
  "Vietnam", "Asia",
  "Bangladesh", "Asia",
  "Timor-Leste", "Asia",
  "Palestine", "Asia",
  "Arménie", "Asia",
  "Azerbaïdjan", "Asia",
  "Géorgie", "Asia",
  "Kazakhstan", "Asia",
  "Kirghizistan", "Asia",
  "Tadjikistan", "Asia",
  "Turkménistan", "Asia",
  "Ouzbékistan", "Asia",
  "Australie", "Oceania",
  "Fidji", "Oceania",
  "Nauru", "Oceania",
  "Vanuatu", "Oceania",
  "Nouvelle-Zélande", "Oceania",
  "Papouasie-Nouvelle-Guinée", "Oceania",
  "Tonga", "Oceania",
  "Samoa", "Oceania",
  "Îles Salomon", "Oceania",
  "Tuvalu", "Oceania",
  "Kiribati", "Oceania",
  "Îles Marshall", "Oceania",
  "Micronésie", "Oceania",
  "Palaos", "Oceania",
  "Îles Cook", "Oceania",
  "Tibet","Asia"
)

