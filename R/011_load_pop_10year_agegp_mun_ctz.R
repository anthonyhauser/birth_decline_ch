load_pop_10year_agegp_mun_ctz = function(){
  #from Kaspar
  pop_mun_df = read_delim("data/population_data/pop_year_age_nat_mun.csv",
                          delim = ";",
                          locale = locale(encoding = "Latin1"),
                          skip = 3) %>% 
    dplyr::rename(year = Jahr,
                  mun_id = Gemeindenummer,
                  mun_name = Gemeindename,
                  age_group = Altersklasse,
                  citizenship_de = Staatsangehörigkeit,
                  n = Anzahl)
  
  citizenship_key = tribble(
    ~citizenship_de,                 ~citizenship,              ~citizenship_region,
    
    # Switzerland
    "Schweiz",                       "Switzerland",                "Switzerland",
    
    # Western Europe
    "Deutschland",                   "Germany",                    "Western Europe",
    "Österreich",                    "Austria",                    "Western Europe",
    "Frankreich",                    "France",                     "Western Europe",
    "Niederlande",                   "Netherlands",                "Western Europe",
    "Belgien",                       "Belgium",                    "Western Europe",
    "Vereinigtes Königreich",        "United Kingdom",             "Western Europe",
    "Irland",                        "Ireland",                    "Western Europe",
    "Dänemark",                      "Denmark",                    "Western Europe",
    "Finnland",                      "Finland",                    "Western Europe",
    "Schweden",                      "Sweden",                     "Western Europe",
    "Italien",                       "Italy",                      "Western Europe",
    "Spanien",                       "Spain",                      "Western Europe",
    "Portugal",                      "Portugal",                   "Western Europe",
    "Griechenland",                  "Greece",                     "Western Europe",
    "Liechtenstein",                 "Liechtenstein",              "Western Europe",
    
    # Eastern Europe
    "Polen",                         "Poland",                     "Eastern Europe",
    "Rumänien",                      "Romania",                    "Eastern Europe",
    "Ex-Jugoslawien",                "Former Yugoslavia",          "Eastern Europe",
    "Übrige europäische Staaten",    "Other European countries",   "Eastern Europe",
    
    # Other continents
    "Afrika",                        "Africa",                     "Africa",
    "Asien",                         "Asia",                       "Asia",
    "Ozeanien",                      "Oceania",                    "Oceania",
    "Vereinigte Staaten",            "United States",              "North America",
    "Kanada",                        "Canada",                     "North America",
    "übrige amerikanische Staaten",  "Other American countries",   "Americas",
    
    # Special code
    "8999",                          "Unknown / Not specified",    "Unknown"
  )
  
  pop_mun_df = pop_mun_df %>% 
    left_join(citizenship_key, by = "citizenship_de")
  
  if(FALSE){
    pop_mun_df$age_group %>% unique()
  }
  return(pop_mun_df)
}
