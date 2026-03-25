df = birth_df %>% 
  group_by(year) %>% 
  dplyr::summarise(n_na = sum(is.na(parity)),
                   p_na = n_na/n())
#missing before 2005
df %>% 
  ggplot(aes(x=year,y=p_na))+geom_point()+geom_line()
#1 missing in 2005, no missing from 2006
df %>% filter(year>=2005) 

cutoff=4
parity_birth_df = birth_df %>% 
  filter(mother_age %in% 15:50,year>=2005, !is.na(parity)) %>% 
  mutate(parity2 = cut(parity, breaks = c(0,1:(cutoff-1), Inf), labels = c(as.character(1:(cutoff-1)), paste0(cutoff, "+")), right = TRUE)) %>% 
  mutate(mother_age2 = cut(mother_age, breaks = c(0,25,35,Inf), right = TRUE)) %>%
  group_by(year,mother_age2,mother_citizenship2,parity2) %>% 
  dplyr::summarise(n_birth = n(),.groups="drop_last") %>% 
  dplyr::mutate(n_birth_tot=sum(n_birth),
                p_birth = n_birth/n_birth_tot)

parity_birth_df %>% 
  ggplot(aes(x=year,y=p_birth,col=mother_citizenship2))+
  geom_point()+geom_line()+
  facet_grid(parity2~mother_age2)

parity_birth_df %>% 
  ggplot(aes(x=year,y=p_birth,col=parity2))+
  geom_point()+geom_line()+
  facet_grid(mother_citizenship2~mother_age2)


parity_birth_df = birth_df %>% 
  filter(mother_age %in% 15:50,year>=2005, !is.na(parity)) %>% 
  dplyr::mutate(parity2 = as.numeric(parity>=2)) %>% 
  group_by(year,mother_citizenship2,mother_age,parity2) %>% 
  dplyr::summarise(n_birth = n(),.groups="drop") %>% 
  pivot_wider(names_from = parity2,values_from = n_birth,values_fill = 0) %>% 
  dplyr::mutate(p_first_birth =  `0`/( `0` + `1`))

parity_birth_df %>% 
  filter(year %in% c(2005,2015,2024),
         mother_age %in% 20:45) %>% 
  ggplot(aes(x=mother_age,y=p_first_birth,col=mother_citizenship2))+
  geom_point()+geom_line()+
  ylim(c(0,1))+
  facet_grid(year~.)

parity_birth_df %>% 
  filter(year %in% c(2005,2015,2024),
         mother_age %in% 20:45) %>% 
  ggplot(aes(x=mother_age,y=p_first_birth,col=factor(year)))+
  geom_point()+geom_line()+
  ylim(c(0,1))+
  facet_grid(mother_citizenship2~.)


parity_birth_df %>% 
  filter(year %in% c(2006,2015,2020:2024),
         mother_age %in% 25:35) %>% 
  ggplot(aes(x=mother_age,y=p_first_birth,col=factor(year)))+
  geom_point()+geom_line()+
  facet_grid(mother_citizenship2~.)
