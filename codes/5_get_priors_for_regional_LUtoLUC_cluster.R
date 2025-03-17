### code to create a full set of targets of crop and LUC based on
### ACR_COMPARE on a regional level.
### REGIONS defined with the new 59REGIONS for this example
### targets are per REGION, timestep, scen(1-3)
### merged_cluster file and a2 need to be in input

if(.Platform$GUI != "RStudio"){
  environment(.libPaths)$.lib.loc = c("renv/library/R-4.1/x86_64-w64-mingw32")
}

library(stringr)
library(gdxrrw)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(downscalr)

GAMSPath = c("C:/GAMS/42")
igdx(GAMSPath)

args <- commandArgs(trailingOnly = TRUE)
JOB <- ifelse(.Platform$GUI == "RStudio",1, as.integer(args[[1]]))
dir.create("output")

source("./codes/S1_simplify_trans.R")
source("./codes/S3_my_LUtoLUC.R")

files <- list.files("./input", full.names = TRUE)

merged <- files[grep("merged", files)]
mapping.file <- files[grep("a2", files)]


mapping <- rgdx.set(file.path(mapping.file), "REGION59_COUNTRY_MAP") %>%
  mutate(across(everything(), as.character))
colnames(mapping) <- c("REGION", "COUNTRY")


########################################################################################

ACR_COMPARE <- rgdx.param(file.path(merged), "ACR_COMPARE") %>%
  mutate(across(everything(), as.character))
colnames(ACR_COMPARE) <- c("shit", "COUNTRY", "AllColRow","AltiClass","SlpClass","SoilClass",
                           "AEZCLASS","SPECIES", "CROPTECH","ALLSCEN1","ALLSCEN2","ALLSCEN3",
                           "AllScenYear","ACR_COMPARE")

ACR_COMPARE <- ACR_COMPARE %>% mutate(value = as.numeric(ACR_COMPARE)*1000/100,
                                      SCEN=paste0(ALLSCEN1,"_",ALLSCEN2,"_",ALLSCEN3)) %>%
  dplyr::select(-ACR_COMPARE, -ALLSCEN1,-ALLSCEN2, -ALLSCEN3, -AEZCLASS, -AltiClass, -SlpClass, -SoilClass) %>%
  mutate(CROPTECH=recode(CROPTECH, "IR_basin"="IR",
                         "IR_furrow"="IR",
                         "IR_drip"="IR",
                         "IR_sprink"="IR"))

ACR_COMPARE <- ACR_COMPARE %>% left_join(mapping) %>% ungroup()  %>%
  group_by(SCEN, COUNTRY,AllColRow, REGION,AllScenYear,SPECIES,CROPTECH) %>%
  summarise(value=sum(value))




land_change <- rgdx.param(file.path(merged), "LUC_COMPARE_SCEN0") %>%
  mutate(across(everything(), as.character))

colnames(land_change) <- c("shit", "REGION",
                           "lu.from","lu.to","ALLSCEN1","ALLSCEN2","ALLSCEN3",
                           "AllScenYear","land_change")

land_change <-  land_change %>%
  mutate(value = as.numeric(land_change)*1000/100, SCEN=paste0(ALLSCEN1,"_",ALLSCEN2,"_",ALLSCEN3)) %>%
  dplyr::select(-land_change, -ALLSCEN1,-ALLSCEN2, -ALLSCEN3, -shit)



sss <- unique(ACR_COMPARE$SCEN)[JOB]


  targets <- NULL
  setDT(targets)
  ACR_COMPARE.temp <- ACR_COMPARE %>% ungroup() %>% filter(SCEN==sss) %>% dplyr::select(-SCEN) %>%
    mutate(CROPSYS=paste0(SPECIES,'_', CROPTECH)) %>% dplyr::select(REGION,COUNTRY,AllColRow, AllScenYear, CROPSYS, value)
  colnames(ACR_COMPARE.temp) <- c("REGION", "COUNTRY", "AllColRow","time","CROPSYS", "value")
  ACR_COMPARE.temp <- ACR_COMPARE.temp %>% group_by(REGION,COUNTRY,AllColRow, time, CROPSYS) %>%
    summarise(value=sum(value), .groups = "drop_last")

  #### added a filter for SS as these are assumed constant
  ACR_COMPARE.temp <- ACR_COMPARE.temp %>% filter(!grepl("SS", CROPSYS))

  times <- as.numeric(unique(ACR_COMPARE.temp$time))[-1]


  regions <- unique(ACR_COMPARE.temp$REGION)
  for(rrr in regions){
    cat(paste0(rrr," region \n"))
    ACR_COMPARE.temp.reg <- ACR_COMPARE.temp %>% filter(REGION==rrr)
    countries <- unique(ACR_COMPARE.temp.reg$COUNTRY)
    for(ccc in countries){
      cat(paste0("  ",ccc," country \n"))
      ACR_COMPARE.temp.reg.country <- ACR_COMPARE.temp.reg %>% filter(COUNTRY==ccc)
      LUs <- unique(ACR_COMPARE.temp.reg.country$AllColRow)
      for(lll in LUs){
        ACR_COMPARE.temp.reg.country.LU <- ACR_COMPARE.temp.reg.country %>% filter(AllColRow==lll)

        for(ttt in times){

          curr.ACR.t0 <- ACR_COMPARE.temp.reg.country.LU %>% filter(time==(ttt-10))
          curr.ACR.t1 <- ACR_COMPARE.temp.reg.country.LU %>% filter(time==ttt)

          curr.start <- curr.ACR.t0 %>% ungroup() %>% dplyr::select(-time) %>% rename("lu"="CROPSYS") %>% filter(value>=0)
          curr.goal <- curr.ACR.t1 %>% ungroup() %>% dplyr::select(-time) %>% rename("lu"="CROPSYS") %>% filter(value>=0)

          if(nrow(curr.ACR.t0)!=0){

            temp_LUC_targets <- LU_to_LUC_prior(curr.start,curr.goal,keep_areas = "both")
            temp_LUC_targets$REGION <- rrr
            temp_LUC_targets$time <- as.character(ttt)
            temp_LUC_targets$scen <- sss
            temp_LUC_targets$COUNTRY <- ccc
            temp_LUC_targets$LUID <- lll

            # Convert targets to data.table if it's not already
            setDT(temp_LUC_targets)

            # Instead of bind_rows, use rbindlist to append
            targets <- rbindlist(list(targets, temp_LUC_targets), use.names = TRUE, fill = TRUE)
          }
        }
      }
    }
  }
  targets.region <- targets[, .(value = sum(value)), by = .(REGION, lu.from, lu.to, time)]
  # Add the 'scen' column
  targets.region[, scen := sss]
  # Convert to data.frame before saving
  targets.region <- as.data.frame(targets.region)






  ACR_COMPARE.temp <- ACR_COMPARE %>% ungroup() %>% filter(SCEN==sss) %>% dplyr::select(-SCEN) %>%
    mutate(CROPSYS=paste0(SPECIES,'_', CROPTECH)) %>% dplyr::select(REGION,AllScenYear, CROPSYS, value)
  colnames(ACR_COMPARE.temp) <- c("REGION","time","CROPSYS", "value")
  ACR_COMPARE.temp <- ACR_COMPARE.temp %>% group_by(REGION,time, CROPSYS) %>%
    summarise(value=sum(value), .groups = "drop_last")

  #### added a filter for SS as these are assumed constant
  ACR_COMPARE.temp <- ACR_COMPARE.temp %>% filter(!grepl("SS", CROPSYS))




  land_change.temp <- land_change %>% filter(SCEN==sss) %>% dplyr::select(-SCEN)
  colnames(land_change.temp) <- c("REGION", "lu.from","lu.to","time", "value")
  land_change.temp <- land_change.temp %>% group_by(REGION, lu.from,lu.to, time) %>%
    summarise(value=sum(value), .groups = "drop_last")

  land_change_crp <- land_change.temp %>% filter(lu.from=="CrpLnd"|lu.to=="CrpLnd") %>%
    group_by(REGION,time, lu.from, lu.to) %>% summarise(value=sum(value), .groups = "drop_last")

  land_change_nocrp <- land_change.temp %>% filter(!(lu.from=="CrpLnd"|lu.to=="CrpLnd")) %>%
    group_by(REGION,time, lu.from, lu.to) %>% summarise(value=sum(value), .groups = "drop_last")


  countries <- intersect(unique(land_change.temp$REGION), unique(ACR_COMPARE.temp$REGION))
  times <- as.numeric(unique(land_change.temp$time))

  nnn <- countries[1]
  ttt <- 2010
  for(nnn in countries){

    prior.temp <- targets.region %>% filter(REGION==nnn)

    for(ttt in times){
      curr.ACR.t0 <- ACR_COMPARE.temp %>% filter(REGION==nnn, time==(ttt-10))
      curr.ACR.t1 <- ACR_COMPARE.temp %>% filter(REGION==nnn, time==ttt)
      curr.land_change_nocrp <- land_change_nocrp %>% filter(REGION==nnn, time==ttt) %>% mutate(scen=sss)
      curr.land_change_crp.t0 <- land_change_crp %>% filter(REGION==nnn, time==ttt, lu.to=="CrpLnd")
      curr.land_change_crp.t1 <- land_change_crp %>% filter(REGION==nnn, time==ttt, lu.from=="CrpLnd")

      curr.start <- curr.ACR.t0 %>% ungroup() %>% dplyr::select(-time) %>% rename("lu"="CROPSYS") %>%
        bind_rows(curr.land_change_crp.t0 %>% ungroup() %>%
                    dplyr::select(-lu.to,-time) %>% rename("lu"="lu.from")) %>% filter(value>=0)

      curr.goal <- curr.ACR.t1 %>% ungroup() %>% dplyr::select(-time) %>% rename("lu"="CROPSYS") %>%
        bind_rows(curr.land_change_crp.t1 %>% ungroup() %>%
                    dplyr::select(-lu.from,-time) %>% rename("lu"="lu.to")) %>% filter(value>=0)

      notrans <- union(unique(curr.land_change_crp.t0$lu.from), unique(curr.land_change_crp.t1$lu.to))
      prior <- expand.grid(lu.from=notrans, lu.to=notrans, value=0)

      crop.all <- union(unique(curr.ACR.t0$CROPSYS), unique(curr.ACR.t1$CROPSYS))
      crop.prior <- expand.grid(lu.from=crop.all, lu.to=crop.all)
      prior.update <- prior.temp %>% filter(time==ttt) %>% ungroup() %>% dplyr::select(lu.from, lu.to, value)

      crop.prior <- crop.prior %>% left_join(prior.update) %>% mutate(value=ifelse(is.na(value),0,value))
      prior.all <- prior %>% bind_rows(crop.prior)

      if(nrow(curr.start)!=0){
        tryCatch({
          # First attempt
          temp_LUC_targets <- LU_to_LUC_prior(curr.start, curr.goal, prior.all, keep_areas = "both")
        }, error = function(e) {
          # If an error occurs, modify prior.all$value and try again
          prior.all$value <- prior.all$value + 0.000001
          temp_LUC_targets <- LU_to_LUC_prior(curr.start, curr.goal, prior.all, keep_areas = "both")
        })
        temp_LUC_targets$REGION <- nnn
        temp_LUC_targets$time <- as.character(ttt)
        temp_LUC_targets$scen <- sss
      } else {
        temp_LUC_targets <- NULL
      }

      curr.target <- curr.land_change_nocrp %>% bind_rows(temp_LUC_targets)



      targets <- targets %>% bind_rows(curr.target)

    }
  }





  to.correct <- targets %>% filter(grepl("_", lu.from), grepl("_", lu.to))
  regions <- unique(to.correct$REGION)
  times <- unique(to.correct$time)
  scens <- unique(to.correct$scen)

  rrr <- "ArgentinaReg"
  ttt <- "2010"
  sss <- "SSP2_GFDL_scenRCP2p6"
  corrected <- NULL
  for(rrr in regions){
    temp1 <- to.correct %>% ungroup() %>% filter(REGION==rrr) %>% dplyr::select(-REGION)
    for(ttt in times){
      temp2 <- temp1 %>% filter(time==ttt) %>% dplyr::select(-time)
      for(sss in scens){
        temp3 <- temp2 %>% filter(scen==sss) %>% dplyr::select(-scen)
        temp3 <- simplify_transitions(temp3)
        corrected <- corrected %>% bind_rows(temp3 %>% mutate(REGION=rrr, time=ttt, scen=sss))
      }
    }
  }

  good <- targets %>% filter(!(grepl("_", lu.from)&grepl("_", lu.to)), lu.from!="NODATA", lu.to!="NODATA")

  targets <- good %>% bind_rows(corrected)

  targets <- targets %>% ungroup() %>% dplyr::select(-COUNTRY, -LUID)


save(targets, file="./output/targetLUC.RData")



