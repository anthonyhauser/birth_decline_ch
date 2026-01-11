


data_folder = "L:/UNISANTE_DESS/S_SUMAC/DIVERS_PROJETS/03_data/kaspar_staub_birth_data/"

#1987-2015
birth_1987_2015 = read.csv(paste0(data_folder,"birth_data/births_1987_2006.csv"),
                           sep=";")  %>% 
  dplyr::mutate(year = as.numeric(Ereignisjahr),
                iso_year = as.numeric(Statistikjahr),
                month = as.numeric(Ereignismonat),
                mother_age = as.numeric(Alter.Mutter.erfuellt),
                birth_loc = as.numeric(Geburtsort),
                birth_state = as.numeric(Geburtsstaat),
                mother_citizenship = as.numeric(Staatsangehoerigkeit.Mutter),
                mother_municipality = as.numeric(Wohngemeinde.Staat.Mutter),
                parity = as.numeric(biologischer.Rang),
                mother_permanent = as.numeric(Mutter.Wohnsitz),
                mother_age2 = NA,
                live_birth = as.numeric(Lebend..oder.Totgeburt)) %>% 
  dplyr::select(year, iso_year, month, mother_age2, birth_loc, birth_state, mother_citizenship,
                mother_municipality, parity, mother_permanent, mother_age, live_birth)

#2007-2020
birth_2007_2020 = read.csv(paste0(data_folder,"birth_data/bevn2007.csv"),
                           fileEncoding = "latin1",sep=",") %>% 
  dplyr::mutate(year = as.numeric(Ereignisjahr),
                iso_year = as.numeric(Statistikjahr),
                month = as.numeric(Ereignismonat),
                mother_age = as.numeric(Mutter..Alter.in.erfüllten.Jahren),
                birth_loc = as.numeric(Geburtsort),
                birth_state = as.numeric(Geburtsstaat),
                mother_citizenship = as.numeric(Mutter..Staatsangehörigkeit),
                mother_municipality = as.numeric(Mutter..Wohngemeinde...Wohnstaat),
                parity = as.numeric(Kind..biologischer.Rang),
                mother_permanent = as.numeric(Mutter..ständig.oder.nicht.ständiger.Wohnsitz),
                mother_age2 = as.numeric(Mutter..Alter.in.erreichten.Jahren),
                live_birth = as.numeric(lebend.geboren.oder.nicht)) %>% 
  dplyr::select(year, iso_year, month, mother_age2, birth_loc, birth_state, mother_citizenship,
                mother_municipality, parity, mother_permanent, mother_age, live_birth)

#2021
birth_2021 = read.csv(paste0(data_folder,"birth_data/NAISS21.csv"),
                      fileEncoding = "latin1",sep=";") %>% 
  dplyr::mutate(year = as.numeric(Ereignisjahr),
                iso_year = as.numeric(Statistikjahr),
                month = as.numeric(Ereignismonat),
                mother_age=as.numeric(Mutter..Alter.in.erfüllten.Jahren),
                birth_loc = as.numeric(Geburtsort),
                birth_state = as.numeric(Geburtsstaat),
                mother_citizenship = as.numeric(Mutter..Staatsangehörigkeit),
                mother_municipality = as.numeric(Mutter..Wohngemeinde...Wohnstaat),
                parity = as.numeric(Kind..biologischer.Rang),
                mother_permanent = as.numeric(Mutter..ständig.oder.nicht.ständiger.Wohnsitz),
                mother_age2 = as.numeric(Mutter..Alter.in.erreichten.Jahren),
                live_birth = as.numeric(lebend.geboren.oder.nicht)) %>% 
  dplyr::select(year, iso_year, month, mother_age2, birth_loc, birth_state, mother_citizenship,
                mother_municipality, parity, mother_permanent, mother_age, live_birth)

#2022
birth_2022 = read.csv(paste0(data_folder,"birth_data/NAISS22.2.csv"),
                      sep=";") %>% 
  dplyr::mutate(year = as.numeric(Ereignisjahr),
                iso_year = as.numeric(Statistikjahr),
                month = as.numeric(Ereignismonat),
                mother_age=as.numeric(Mutter..Alter.in.erfüllten.Jahren),
                birth_loc = as.numeric(Geburtsort),
                birth_state = as.numeric(Geburtsstaat),
                mother_citizenship = as.numeric(Mutter..Staatsangehörigkeit),
                mother_municipality = as.numeric(Mutter..Wohngemeinde...Wohnstaat),
                parity = as.numeric(Kind..biologischer.Rang),
                mother_permanent = Mutter..ständiger.oder.nicht.ständiger.Wohnsitz,
                mother_age2 = as.numeric(Mutter..Alter.in.erreichten.Jahren),
                live_birth = as.numeric(Art.der.Geburt..Lebend..Totgeburt.)) %>% 
  dplyr::select(year, iso_year, month, mother_age2, birth_loc, birth_state, mother_citizenship,
                mother_municipality, parity, mother_permanent, mother_age, live_birth)

#2023
birth_2023 = read.csv(paste0(data_folder,"birth_data/BIRTHS_UNIZH_IEM_2023.csv"),
                      sep=";") %>% 
  dplyr::mutate(year = as.numeric(Ereignisjahr),
                iso_year = as.numeric(Statistikjahr),
                month = as.numeric(Ereignismonat),
                mother_age=as.numeric(Mutter..Alter.in.erfüllten.Jahren),
                birth_loc = as.numeric(Geburtsort),#will introduce NA because Geburtsort = "" (empty)
                birth_state = as.numeric(Geburtsstaat),
                mother_citizenship = as.numeric(Mutter..Staatsangehörigkeit),
                mother_municipality = as.numeric(Mutter..Wohngemeinde...Wohnstaat),
                parity = as.numeric(Kind..biologischer.Rang),
                mother_permanent = Mutter..ständiger.oder.nicht.ständiger.Wohnsitz,
                mother_age2 = as.numeric(Mutter..Alter.in.erreichten.Jahren),
                live_birth = as.numeric(Art.der.Geburt..Lebend..Totgeburt.)) %>% 
  dplyr::select(year, iso_year, month, mother_age2, birth_loc, birth_state, mother_citizenship,
                mother_municipality, parity, mother_permanent, mother_age, live_birth)

#2024
birth_2024 = read.csv(paste0(data_folder,"birth_data/BIRTHS_UNIZH_IEM_2024.csv"),
                      sep=";") %>% 
  dplyr::mutate(year = as.numeric(Ereignisjahr),
                iso_year = as.numeric(Statistikjahr),
                month = as.numeric(Ereignismonat),
                mother_age=as.numeric(Mutter..Alter.in.erfüllten.Jahren),
                birth_loc = as.numeric(Geburtsort),#will introduce NA because Geburtsort = "" (empty)
                birth_state = as.numeric(Geburtsstaat),
                mother_citizenship = as.numeric(Mutter..Staatsangehörigkeit),
                mother_municipality = as.numeric(Mutter..Wohngemeinde...Wohnstaat),
                parity = as.numeric(Kind..biologischer.Rang),
                mother_permanent = Mutter..ständiger.oder.nicht.ständiger.Wohnsitz,
                mother_age2 = as.numeric(Mutter..Alter.in.erreichten.Jahren),
                live_birth = as.numeric(Art.der.Geburt..Lebend..Totgeburt.)) %>% 
  dplyr::select(year, iso_year, month, mother_age2, birth_loc, birth_state, mother_citizenship,
                mother_municipality, parity, mother_permanent, mother_age, live_birth)


if(FALSE){
  #in years 2007-2015, births are reported by birth_1987_2015 and birth_2007_2020 (some discrepancies)
  left_join(birth_1987_2015 %>% filter(year>=2007) %>% 
              group_by(year) %>% dplyr::summarise(n1=n()),
            birth_2007_2020 %>% filter(year<=2015) %>% 
              group_by(year) %>% dplyr::summarise(n2=n()), by = "year")
  
  #Missing values: birth_state (only 1 in 2017), parity (mostly before 2005, otherwise <1%), mother_permanent (2 in 2021), birth_loc (not used), mother_age2 (not used)
  na_by_col = function(data){
    data %>% dplyr::summarise(across(everything(), ~ sum(is.na(.))))  %>% 
      bind_rows( data %>% dplyr::summarise(across(everything(), ~ mean(is.na(.))))) %>% 
      dplyr::mutate(across(everything(), ~ format(., scientific = FALSE)))
  }
  na_by_col(birth_1987_2015)
  birth_1987_2015 %>% 
    group_by(year) %>% dplyr::summarise(p_na_parity=sum(is.na(parity))/n()) %>%
    ggplot(aes(x=year,y=p_na_parity)) +geom_point()+geom_line()
  na_by_col(birth_2007_2020)
  birth_2007_2020 %>% filter(is.na(birth_state))
  na_by_col(birth_2021)
  na_by_col(birth_2022)
  na_by_col(birth_2023)
  na_by_col(birth_2024)
}

birth_1987_2024 = dplyr::bind_rows(birth_1987_2015 %>% filter(year<2007),
                                   birth_2007_2020,
                                   birth_2021,
                                   birth_2022,
                                   birth_2023,
                                   birth_2024)

birth_1987_2024$mother_municipality %>% unique() %>% sort()

setdiff(birth_1987_2024$mother_municipality %>% unique() %>% sort(),
        final_mun_df$hist_mun_id %>% unique() %>% sort()) %>% sort()


birth_1987_2024$mother_citizenship %>% unique() %>% sort()

birth_1987_2024 %>% filter()


#municipality data: link number with municipality name, https://www.agvchapp.bfs.admin.ch/fr
final_mun_df = load_municipality_data()

mun_df %>% filter(mun_name=="Moutier")


