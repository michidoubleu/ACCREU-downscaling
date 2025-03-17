files <- list.files("./output/", full.names = T)
files <- files[grepl("targetLUC", files)]

full.targets <- NULL
scen <- files[1]
for(scen in files){
  load(paste0(scen))

  full.targets <- full.targets %>% bind_rows(targets)
}

save(full.targets, file=paste0("./output/targets_ACR_LUC_ACCREU.RData"))