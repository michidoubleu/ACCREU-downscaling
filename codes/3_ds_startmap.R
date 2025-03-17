load(file = "output/targets_for_startmap.RData")
load(file="output/LUC_targets_startmap.RData")

updated_map <- NULL

unique_country_LUID <- unique(paste0(target_to$country,"_",target_to$LUID))
i <- unique_country_LUID[1]

for(i in unique_country_LUID){
  temp_i <- c(stringr::str_split_fixed(i ,"_",2))

  temp_start <- target_from.SimUID %>%
    dplyr::ungroup() %>%
    dplyr::filter(country==temp_i[1] ,LUID==temp_i[2],value!=0) %>%
    dplyr::select(-protected) %>%
    group_by(country, LUID, lu.class) %>% mutate(share=value/sum(value)) %>%
    rename("lu.from"="lu.class", "init"="value") %>%
    dplyr::select(!LUID)

  temp_LUC <- LUC_targets %>%
    dplyr::ungroup() %>%
    dplyr::filter(country==temp_i[1] ,LUID==temp_i[2],value!=0) %>%
    dplyr::select(!LUID)

  transition <- temp_LUC %>%
    left_join(temp_start) %>%
    mutate(value_to=share*value) %>%
    group_by(SimUID,country,lu.to) %>%
    summarise(value=sum(value_to)) %>%
    ungroup() %>%
    dplyr::select(-country) %>%
    rename("lu.class"="lu.to")


  if(round(sum(temp_LUC$value),0)!=round(sum(transition$value),0)){
    warning("Areas do not match!")
  }

  transition$country <- temp_i[1]
  transition$LUID <- temp_i[2]

  updated_map <- updated_map %>% bind_rows(transition)
}



save(updated_map, file="./output/updated_startmap_SimUID.RData")
