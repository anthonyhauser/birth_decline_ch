get_prob_childless_by_mun = function(birth_df, pop_mun_df,
                                     filter_ctz = c("swiss","non-swiss"),
                                     p_mun_changes = 0.06){ 
  #&% moving comes from: https://www.swissinfo.ch/eng/various/fewer-swiss-residents-moved-house-in-2024/90601688
  
  filename = paste0(code_root_path,"savepoint/","p_childless_df.RDS")
  
  if(file.exists(filename)){
    pop_birth_mun_df2 = readRDS(paste0(code_root_path,"savepoint/","p_childless_df.RDS"))
  }else{
    
    #aggregate first birth by year, age and mun
    birth_year_mun_df = birth_df %>% 
      filter(mother_citizenship2 %in% filter_ctz) %>% 
      filter(year %in% 2011:2024) %>% 
      filter(parity==1) %>% 
      group_by(year,mun_id=mother_mun_id, age=mother_age) %>% 
      dplyr::summarise(n_birth=n(),.groups="drop")
    
    #aggregate population by year, age and mun
    pop_mun_df2 = pop_mun_df %>%
      filter(citizenship %in% filter_ctz) %>% 
      filter(year %in% 2011:2024) %>% 
      filter(month==1) %>% 
      group_by(year,age,mun_id,mun_name) %>% 
      dplyr::summarise(n_pop=sum(n),.groups="drop")
    
    #combine population with birth
    pop_birth_mun_df = pop_mun_df2 %>% 
      left_join(birth_year_mun_df, by=c("year","age","mun_id")) %>% 
      dplyr::mutate(n_birth = replace_na(n_birth,0))
    
    #calculate the proportion of childless women by age (see below for detailed calculations)
    pop_birth_mun_df2 = pop_birth_mun_df %>%
      #proportion of having a first child in a given age for a given mun
      group_by(age,mun_id,mun_name) %>% 
      dplyr::summarise(n_birth =  sum(n_birth),
                       n_pop_2024 = n_pop[year==2024],
                       n_pop = sum(n_pop),.groups="drop") %>% 
      #proportion of childless women
      dplyr::mutate(p_child=n_birth / n_pop,
                    p_child = replace_na(p_child,0)) %>% 
      arrange(mun_id, age) %>%
      group_by(mun_id) %>%
      dplyr::mutate(p_childless = 1 - lag(cumsum(p_child), default = 0),
                     n_pop_2024 = sum(n_pop_2024)) %>%
      ungroup()
    
    #calculate the proportion of childless women by age (see below for detailed calculations)
    pop_birth_nat_df2 = pop_birth_mun_df %>%
      #proportion of having a first child in a given age for a given mun
      group_by(age) %>% 
      dplyr::summarise(n_birth =  sum(n_birth),
                       n_pop_2024 = sum(n_pop[year==2024]),
                       n_pop = sum(n_pop),.groups="drop") %>% 
      #proportion of childless women
      dplyr::mutate(p_child = n_birth / n_pop,
                    p_child = replace_na(p_child,0)) %>% 
      arrange(age) %>%
      dplyr::mutate(p_childless = 1 - lag(cumsum(p_child), default = 0),
                    n_pop_2024 = sum(n_pop_2024)) %>%
      ungroup()
    
    
    #constraint to 0 if negative
    pop_birth_mun_df2 = pop_birth_mun_df2 %>% 
      dplyr::mutate(p_childless_pos = if_else(p_childless>0,p_childless,0))
    
    #mix with national estimate
    pop_birth_mun_df2 = pop_birth_mun_df2 %>% 
      left_join(pop_birth_nat_df2 %>% dplyr::select(age,p_childless_nat = p_childless),by="age") %>% 
      dplyr::mutate(p_childless_pos2 = (1-p_mun_changes) * p_childless_pos + p_mun_changes * p_childless_nat)
    
    #for age 15, assign value of age 16 to avoid the impossibility to have a second child at this age (would be problematic when looking at second/third etc birth as the susceptible population would be 0 at age 15)
    pop_birth_mun_df2 = pop_birth_mun_df2 %>% 
      group_by(mun_id) %>% 
      dplyr::mutate(p_childless_pos2 = ifelse(age==15,p_childless_pos2[age==16], p_childless_pos2)) %>% ungroup()
    
    if(FALSE){
      #check that all est are comprised between 0 and 1 (otherwise will lead to susceptible population size of 0)
      pop_birth_mun_df2 %>% filter(p_childless_pos2==1 | p_childless_pos2==0)
    }
      
    #save
    saveRDS(pop_birth_mun_df2, file=paste0(code_root_path,"savepoint/","p_childless_df.RDS"))
  }
  
  if(FALSE){
    #municipality where p_childless go below 0
    #Explanation: 
    # q_a is constructed from cross-sectional data (not a true cohort)
    # so N_a varies across ages due to migration and population structure
    #
    # This implies B_a / N_a is an approximate rate, not a true hazard
    # q_a is a reconstructed index of childlessness, not a strict proportion
    # It can become negative in small populations or due to noisy rates
    pop_birth_mun_df2 %>% filter(p_childless<0) %>% 
      group_by(mun_name,n_pop_2024) %>% 
      dplyr::summarise(min_age_below_0 = min(age[p_childless<0])) %>% 
      arrange(-n_pop_2024)
    
    #example:
    pop_birth_mun_df2 %>% filter(mun_name=="Uitikon") %>% View()
    
    #####################
    #p_childless in big cities
    pop_birth_mun_df2 %>% 
      filter(n_pop_2024>10000) %>% 
      ggplot(aes(x=age,y=p_childless_pos2,col=mun_name)) +
      geom_line()
    #by population size of the city
    pop_birth_mun_df2 %>% 
      dplyr::mutate(n_pop_2024_group = cut(n_pop_2024,c(0,100,1000,5000,10000,Inf))) %>%  
      group_by(age,n_pop_2024_group) %>% 
      dplyr::summarise(p_childless = sum(p_childless * n_pop_2024)/sum(n_pop_2024),.groups="drop") %>% 
      ggplot(aes(x=age,y=p_childless,col=n_pop_2024_group)) +
      geom_line()
    pop_birth_mun_df2 %>% 
      dplyr::mutate(n_pop_2024_group = cut(n_pop_2024,c(0,100,1000,5000,10000,Inf))) %>% 
      ggplot(aes(x=age,y=p_childless,group=mun_id)) +
      geom_line(alpha=0.1)+
      facet_grid(n_pop_2024_group~.)
    
  }
  
  return(pop_birth_mun_df2)
}


# Model assumptions (age-specific recursion)
#
# q_1 := 1
# N_susc_1 := N_1
#
# For age a -> a+1:
# N_susc_{a+1} = N_{a+1} * q_a
# q_{a+1} = q_a * (1 - B_{a+1} / N_susc_{a+1})
#
# Step 1: substitute N_susc_{a+1}
#
# q_{a+1} = q_a * (1 - B_{a+1} / (N_{a+1} * q_a))
#
# Step 2: simplify
#
# q_{a+1} = q_a - B_{a+1} / N_{a+1}
#
# Step 3: iterate the recursion
#
# q_2 = q_1 - B_2 / N_2
# q_3 = q_2 - B_3 / N_3
#     = q_1 - B_2 / N_2 - B_3 / N_3
#
# General form:
#
# q_a = 1 - sum_{i=2}^{a} (B_i / N_i)
#
# Equivalent survival interpretation:
#
# q_a = proportion childless at age a
#     = 1 minus cumulative age-specific incidence rates
#
# Important indexing note:
#
# cumsum(B_i / N_i) gives:
# 1 - sum_{i<=a} B_i / N_i
# which corresponds to q_{a+1} (one-age shift)