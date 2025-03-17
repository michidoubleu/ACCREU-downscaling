rm(list=ls())
library(stringr)
library(gdxrrw)
library(dplyr)
library(tidyr)
library(readr)
library(terra)

GAMSPath = c("C:/GAMS/40")
igdx(GAMSPath)

#### codes 1-4 needs to be run only once, but not for scenario updates
source("codes/1_create_targets_and_SimUmap.R")
source("codes/2_create_LUC_startmaptargets.R")
source("codes/3_ds_startmap.R")
source("codes/4_harmonize_to1km.R")

#### cleaning of output folder before cluster use
file.remove(list.files("./output/", pattern = "targetLUC_", full.names = TRUE))

#### codes 5-7 are dependencies in case of scenario updates requires LIMPOPO cluster
source("codes/S6_update_config_priors.R")
system("run_5_priors.bat")
source("codes/6_summarise_priors.R")
source("codes/7_croppriors.R")

#### cleaning of output folder before cluster use
file.remove(list.files("./output/", pattern = "CSDS_", full.names = TRUE))

#### codes 5-7 are dependencies in case of scenario updates takes quite a long time
source("codes/S7_update_config_CSDS.R")
system("run_8_CSDS.bat")
source("codes/9_summarise_CSDS.R")


















