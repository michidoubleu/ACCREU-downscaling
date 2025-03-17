##### BUG it stopps at AFG but all actual transitions have been calculated by then, so just continue with error

load(file = "output/targets_for_startmap.RData")

LUC_targets <- NULL
check_df <- NULL


unique_country_LUID <- unique(paste0(target_to$country,"_",target_to$LUID))
unique_country_LUID <- unique_country_LUID[!grepl("Afgha",unique_country_LUID)]
unique_country_LUID <- unique_country_LUID[!grepl("NotAcc",unique_country_LUID)]
i <- unique_country_LUID[1]

for(i in unique_country_LUID){

  temp_i <- c(stringr::str_split_fixed(i ,"_",2))

  temp_start <- target_from %>%
    dplyr::ungroup() %>%
    dplyr::filter(country==temp_i[1] ,LUID==temp_i[2],value!=0) %>%
    dplyr::select(!LUID) %>%
    dplyr::rename(lu="lu.class")

  temp_targets <- target_to %>%
    dplyr::ungroup() %>%
    dplyr::filter(country==temp_i[1] ,LUID==temp_i[2], value!=0) %>%
    dplyr::select(!LUID) %>%
    dplyr::rename(lu="lu.class")

  if(round(sum(temp_start$value),0)!=round(sum(temp_targets$value),0)){
    warning("Areas do not match!")
  }

  temp_LUC_targets <- LU_to_LUC(temp_start,temp_targets,keep_areas = "from")
  temp_LUC_targets$country <- temp_i[1]
  temp_LUC_targets$LUID <- temp_i[2]

  LUC_targets <- LUC_targets %>% bind_rows(temp_LUC_targets)


}

convergence_check <- LUC_targets %>% group_by(country,LUID,lu.to) %>% summarise(target=sum(value)) %>% left_join(target_to %>% rename(lu.to="lu.class")) %>% mutate(diff.share=abs((target-value)/target), diff=abs(target-value))



save(LUC_targets, file="output/LUC_targets_startmap.RData")
