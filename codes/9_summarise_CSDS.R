rm(list = ls())
library(stringr)
library(withr)
require(progress)
library(tidyr)
library(dplyr)
##################################################################################
res <- list.files("output/", full.names = T)
res <- res[grepl("CSDS_", res)]
res <- res[grepl(as.character(max(unique(as.numeric(substr(res,13,17))))),res)]

luc.res <- NULL
rrr <- res[1]
for(rrr in res){
    load(rrr)

    res.all <- res.all %>% filter(value!=0)
    luc.res <- luc.res %>% bind_rows(res.all)
  }

  save(luc.res, file=paste0('./results/ACCREU2025_downscaled.RData'))




