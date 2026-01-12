#municipality data: link number with municipality name, https://www.agvchapp.bfs.admin.ch/fr
load_municipality_data = function(){
  #load xml files (done one as takes time)
  if(FALSE){
    library(xml2)
    doc <- read_xml(paste0(data_folder,"municipality_data/1.2.0/eCH0071_260101.xml"))
    # Helper function: extract all children of nodes dynamically
    extract_level <- function(parent_xpath) {
      nodes <- xml_find_all(doc, parent_xpath)
      
      # Get all possible child element names
      all_children <- unique(unlist(map(nodes, ~ xml_name(xml_children(.x)))))
      
      # Create tibble with all children as columns
      tibble_map <- map_dfr(nodes, function(node) {
        map(all_children, function(child) {
          # extract text if exists, otherwise NA
          txt <- xml_text(xml_find_first(node, paste0("./", child)))
          if(length(txt) == 0) NA else txt
        }) |> set_names(all_children)
      })
      
      return(tibble_map)
    }
    
    # Cantons
    ctn_df <- extract_level(".//canton")
    # Districts
    dist_df <- extract_level(".//district")
    # Municipalities
    mun_df <- extract_level(".//municipality")
    
    #save as takes time to load dist_df and mun_df
    saveRDS(mun_df,"data/municipality_data/mun_df.rds")
    saveRDS(dist_df,"data/municipality_data/dist_df.rds")
    saveRDS(ctn_df,"data/municipality_data/ctn_df.rds")
  }
  
  #load municipality datasets
  mun_df = readRDS("data/municipality_data/mun_df.rds")
  dist_df = readRDS("data/municipality_data/dist_df.rds")
  ctn_df = readRDS("data/municipality_data/ctn_df.rds")
  
  #bind municipality and district data
  mun_df = mun_df %>%
    dplyr::select(ctn_abbr = cantonAbbreviation, dist_hist_id = districtHistId, mun_id = municipalityId, mun_name = municipalityShortName,
                  mun_adm_id = municipalityAdmissionNumber, mun_adm_date = municipalityAdmissionDate, mun_ab_id = municipalityAbolitionNumber, mun_ab_date=municipalityAbolitionDate) %>% 
    left_join(dist_df %>%
                dplyr::select(dist_hist_id = districtHistId, ctn_id = cantonId, dist_id = districtId, dist_name = districtShortName),by="dist_hist_id") %>% 
    dplyr::select(-c(dist_hist_id)) %>% #used only for join
    dplyr::mutate(mun_id=as.numeric(mun_id),
                  ctn_id = as.numeric(ctn_id),
                  dist_id =as.numeric(dist_id),
                  mun_adm_id = as.numeric(mun_adm_id),
                  mun_ab_id = as.numeric(mun_ab_id),
                  mun_adm_date = as.Date(mun_adm_date),
                  mun_ab_date = as.Date(mun_ab_date))  %>%
    #keep only municipality that were not abolished before 1987
    filter(is.na(mun_ab_date) | (!is.na(mun_ab_date) & mun_ab_date>as.Date("1987-01-01")))
  
  #Aim: assign a current mun_id to each row
  #df with current municipality (i.e., without abolition date) -> final mun_id is mun_id
  mun_df1 = mun_df %>% filter(is.na(mun_ab_date))
  #df with municipalities whose id does not belong to a current municipalities -> need to assign a final mun_id
  mun_df2 = mun_df %>% filter(!(mun_id %in% mun_df1$mun_id))
  #df with ancient municipalities (i.e., with abolition date) -> will be used later to assing a final mun_id to municipalities with an ancient mun_id
  mun_df3 = mun_df %>% filter(!is.na(mun_ab_date))
  
  
  if(FALSE){
    #check that current municipality id is not duplicated (i.e., only one row per mun_id without abolition date)
    mun_df1 %>% group_by(mun_id) %>% dplyr::mutate(n=n()) %>% filter(n>1) %>% arrange(mun_id)
    
    
  }
  
  #identify rows where mun_adm_id is duplicated, linking some "ancient" municipality to multiple current municipalities
  dupl_mun_adm_id_df = mun_df %>%
    #only consider rows where adm_id corresponds to ab_id of ancient municipalities ("descendant" of ancient municipalities)
    filter(mun_adm_id %in% mun_df3$mun_ab_id) %>% 
    group_by(mun_adm_id) %>% dplyr::mutate(n=n()) %>% ungroup() %>% filter(n>1)
  dupl_mun_adm_id_df
  #municipality whose mun_ab_id refers to multiple rows
  mun_df3 %>% filter(mun_ab_id %in% dupl_mun_adm_id_df$mun_adm_id)
  #slice the base of all mun_adm_id "candidate" to remove duplicates 
  base_mun_adm_id_df = mun_df %>% 
    filter(mun_adm_id %in% mun_df3$mun_ab_id) %>% 
    group_by(mun_adm_id) %>% 
    arrange(is.na(mun_ab_id)) %>%   # non-NA first, NA later
    slice(1) %>% ungroup()
  
  #retrieve the final mun_id for each rows in mun_df2 (done sequentially as there might be several link before arriving to mun_id without abolition date)
  x = mun_df2 %>% 
    dplyr::mutate(final_mun_id=NA) %>% 
    dplyr::select(origin_mun_id=mun_id,mun_ab_id,final_mun_id)
  while(sum(!is.na(x$mun_ab_id))>0){
    x = x %>% arrange(origin_mun_id) %>% 
      left_join(base_mun_adm_id_df %>% dplyr::select(new_mun_ab_id = mun_ab_id, new_mun_id = mun_id,
                                                     mun_adm_id),by=c("mun_ab_id"="mun_adm_id")) %>% 
      dplyr::mutate(mun_ab_id = new_mun_ab_id,
                    final_mun_id = if_else(is.na(final_mun_id) & is.na(mun_ab_id),new_mun_id, final_mun_id)) %>% 
      dplyr::select(origin_mun_id,mun_ab_id ,final_mun_id)
    print(sum(!is.na(x$mun_ab_id)))
  }
  
  
  if(FALSE){
    #check that each rows has found a final mun_id
    x %>% filter(is.na(final_mun_id))
    
    #check that rows with the same original mun_id are assigned with the same final mun_id
    x %>%
      group_by(origin_mun_id) %>% 
      dplyr::mutate(n=length(unique(final_mun_id))) %>% filter(n>1)
    
    #this is the case of mun_id 4510
    base_mun_adm_id_df %>% filter(mun_adm_id==1858)
  }
  
  #keep only 1 final mun_id for each original mun_id
  x = x %>% 
    group_by(origin_mun_id) %>% slice(1) %>% ungroup()
  
  #bind mun_id rows from current municipalities with mun_di rows from ancient municipalities, with link to the current mun_id
  link_mun_id_df =  rbind(mun_df1 %>% 
                            dplyr::mutate(hist_mun_id = mun_id) %>% 
                            dplyr::select(hist_mun_id , hist_mun_ab_date = mun_ab_date,mun_id),
                          left_join(x %>% dplyr::select(hist_mun_id = origin_mun_id, mun_id = final_mun_id),
                                    mun_df2 %>% 
                                      group_by(mun_id) %>% 
                                      slice_max(mun_ab_date, with_ties = FALSE) %>% 
                                      ungroup() %>% 
                                      dplyr::select(hist_mun_id=mun_id,hist_mun_ab_date = mun_ab_date),by = "hist_mun_id")) %>% 
    arrange(mun_id)
  
  #For each current mun_id (mun_id), link all the current or ancient mun_id (hist_mun_id)
  final_mun_df = mun_df1 %>%
    left_join(link_mun_id_df,by="mun_id")
  
  return(final_mun_df)
  
  
}
