args <- commandArgs(trailingOnly = TRUE)
JOBS <- ifelse(.Platform$GUI == "RStudio",1, as.integer(args[[1]]))
dir.create("output")
if(.Platform$GUI != "RStudio"){
  environment(.libPaths)$.lib.loc = c("renv/library/R-4.1/x86_64-w64-mingw32")
}

require(tidyr)
require(dplyr)
library(downscalr)
library(readr)


######## load stuff
source("./codes/S4_get_croppriors.R")
load("./output/updated_startmap_SimUID_EU.RData")
load("./output/targets_ACR_LUC_ACCREU.RData")
load("./output/restrictions.RData")
load("./input/MINDSTEP_XY.RData")
load("./input/MINDSTEP_betas.RData")


X.raw <- X.raw %>% pivot_wider(names_from = variable,values_from = value) %>% pivot_longer(cols = -c(SimUID), names_to = "ks", values_to = "value") %>% na.omit()

updated_map <- updated_map.EU %>% mutate(SimUID=as.numeric(SimUID)) %>% ungroup() %>% dplyr::select(-country) %>% left_join(simu_region %>% dplyr::select(SimUID,REGION))


Ns <- unique(updated_map$REGION)
Ns <- Ns[!Ns%in%c("Former_USSR", "NorthernAf")]
scens <- unique(targets$scen)
times <- unique(targets$time)

est.grid = expand.grid(Ns = Ns, scens=scens)

curr.N = est.grid$Ns[JOBS]
curr.scen = est.grid$scens[JOBS]

curr.lu_levels <- updated_map %>% filter(REGION==curr.N) %>% rename("ns"="SimUID") %>%
  mutate(ns=as.character(ns)) %>% dplyr::select(-REGION) %>%
  mutate(lu.from=recode(lu.class, "MngFor"="Forest", "PriFor"="Forest")) %>% dplyr::select(-lu.class) %>%
  group_by(ns, lu.from) %>% summarise(value=sum(value)) %>%
  ungroup() %>% filter(ns!="84059") #bug fix Wester africa
curr.lu_levels <- data.frame(curr.lu_levels)


# threshold.change <- sum(curr.lu_levels$value)*0.00001

curr.ns <- as.character(unique(curr.lu_levels$ns))

curr.betas <- betas[betas$Ns==as.character(curr.N),] %>%
  mutate(lu.from.nocrp=recode(lu.from, "OthNatLnd"="NatLnd", "Grass"="GrsLnd"),
         lu.to.nocrp=recode(lu.to, "OthNatLnd"="NatLnd", "Grass"="GrsLnd")) %>%
  dplyr::select(-Ns, -lu.from,-lu.to)



curr.betas <- curr.betas %>%rename("lu.from"="lu.from.nocrp", "lu.to"="lu.to.nocrp") %>% filter(lu.from!="CrpLnd", lu.to!="CrpLnd")%>% dplyr::select(ks, lu.from, lu.to, value) %>%
  mutate(ks=recode(ks, "MngFor"="Forest", "PriFor"="Forest")) %>%
  group_by(ks, lu.from,lu.to) %>% summarise(value=mean(value)) %>%
  ungroup()

curr.betas <- data.frame(curr.betas)


curr.X <- X.raw %>% filter(SimUID%in%curr.ns) %>% dplyr::select(SimUID, ks, value) %>% rename("ns"="SimUID") %>%
  mutate(ks=recode(ks, "MngFor"="Forest", "PriFor"="Forest"), ns=as.character(ns)) %>%
  group_by(ns, ks) %>% summarise(value=sum(value)) %>%
  ungroup() %>% filter(!ks%in%c("OagLnd", "GrsLnd"))
curr.X <- data.frame(curr.X)

lus <- data.frame(ks=curr.lu_levels$lu.from %>% unique(), avail=0)
curr.X <- curr.X %>% left_join(curr.lu_levels %>% rename("ks"="lu.from","updated.v"="value")) %>% left_join(lus) %>% mutate(updated.v=ifelse(is.na(updated.v),avail,updated.v), value=ifelse(is.na(updated.v),value,updated.v)) %>%
  dplyr::select(-updated.v,-avail)



curr.SCEN1 <- substr(curr.scen,1,4)
curr.SCEN3 <- gsub("^(.*[_])","",curr.scen)
curr.SCEN2 <- gsub(paste0(curr.SCEN1,"_"),"",gsub(paste0("_",curr.SCEN3),"",curr.scen))







ttt <- "2010"
res.all <- data.frame()







for(ttt in times){

curr.targets <- targets[targets$REGION==curr.N,] %>% ungroup() %>%
  dplyr::select(-REGION) %>% filter(scen==curr.scen) %>%
  mutate(lu.from=recode(lu.from, "MngFor"="Forest", "PriFor"="Forest"),
         lu.to=recode(lu.to, "MngFor"="Forest", "PriFor"="Forest")) %>%
  rename("times"="time") %>% filter(lu.from!=lu.to) %>% dplyr::select(-scen) %>% filter(times==ttt, value>1)

if(nrow(curr.targets)==0){next}


#### precalculate transitions in case LU changes "fully"

#1 check size of trans compared to init area

area.check <- curr.lu_levels %>% group_by(lu.from) %>% summarise(init.val=sum(value))
area.check <- area.check %>% left_join(curr.targets %>% group_by(lu.from)
                                       %>% summarise(target=sum(value))) %>%
  mutate(share=target/init.val)













actual.trans <- curr.targets %>% mutate(trans=paste0(lu.from,"/", lu.to))
actual.trans <- unique(actual.trans$trans)


priors <- get.priors.crop(prices, yield, tech_shifter, simu_region, actual.trans, curr.N, curr.SCEN1, curr.SCEN2, curr.SCEN3, curr.time=ttt)

###

actual.to <- curr.targets %>% filter(lu.from=="PltFor")
actual.to <- unique(actual.to$lu.to)

actual.from <- curr.targets %>% filter(lu.to=="PltFor") %>% filter(nchar(lu.from)==6)
actual.from <- unique(actual.from$lu.from)

if(sum(length(actual.to),length(actual.from))!=0){
  source("./codes/S5_get_SRPpriors.R")
  SRP_priors <- get.priors.SRP(curr.SRP_Suit, simu_region, actual.from, actual.to, curr.N, curr.SCEN1, curr.SCEN2, curr.SCEN3, times)
  priors <- priors %>% bind_rows(SRP_priors)
}



priors <- priors %>% mutate(ns=as.character(ns))

priors[is.na(priors)] <- 0
priors$value[is.nan(priors$value)] <- 0

####### prior check for 0 priors
pr.check <- priors %>% group_by(lu.from,lu.to) %>% summarise(value=sum(value))
if(any(pr.check==0)){
  zero.lus <- pr.check %>% filter(value==0)
  starting <- curr.lu_levels %>% filter(lu.from%in%unique(zero.lus$lu.from)) %>% group_by(lu.from) %>%
    mutate(value=(value-min(value))/(max(value)-min(value)))

  new.priors <- starting %>% full_join(zero.lus %>% dplyr::select(-value)) %>% ungroup() %>% mutate(times=ttt)

  priors <- priors %>% bind_rows(new.priors) %>% group_by(ns,times,lu.from, lu.to) %>% summarise(value=sum(value), weight=mean(weight, na.rm=TRUE))

}

priors <- as.data.frame(priors)
priors2 <- priors
priors2$value <- priors2$value+abs(rnorm(length(priors$value))/100)

priors2 <- priors2 %>% mutate(value=ifelse(value>1,1,value))


#### need mapping from LU, country, 2000 is crop present to SimUID
curr.restriction <- restrictions %>% filter(SimUID %in% curr.ns) %>% rename("ns"="SimUID", "lu.to"="CROPSYS") %>% mutate(ns=as.character(ns))


priors2 <- priors2 %>% left_join(curr.restriction %>% rename("rest"="value")) %>%
  mutate(value=ifelse(is.na(rest), value, rest*value),
         weight=ifelse(is.na(rest), 1, 1)) %>% dplyr::select(-rest)

priors3 <- priors2 %>% group_by(lu.from, lu.to) %>% mutate(value=value/max(value, na.rm = T))
priors3$value[is.na(priors3$value)] <- 0
priors3$weight[is.na(priors3$weight)] <- 0

factors <- curr.targets  %>% group_by(lu.from) %>% summarise(value.t=sum(value)) %>% left_join(curr.lu_levels %>% group_by(lu.from) %>% summarise(value=sum(value))) %>% mutate(scale=value.t/value) %>% filter(scale>1) %>% dplyr::select(lu.from, scale)
if(nrow(factors)!=0){
  curr.targets <- curr.targets %>% left_join(factors) %>% mutate(value=ifelse(is.na(scale),value,value/(scale*1.01))) %>% dplyr::select(-scale)
}

if(ttt==2040){
if(nrow(priors3)!=0){
  res1 = downscale(targets = curr.targets,
                   start.areas = curr.lu_levels,
                   xmat = curr.X,
                   betas = curr.betas %>% filter(ks %in% unique(curr.X$ks)) %>% mutate(ks=as.character(ks)),
                   priors = priors3,
                   options = downscale_control(MAX_EXP = log(.Machine$double.xmax)-200))
} else {
  res1 = downscale(targets = curr.targets,
                   start.areas = curr.lu_levels,
                   xmat = curr.X,
                   betas = curr.betas %>% filter(ks %in% unique(curr.X$ks)) %>% mutate(ks=as.character(ks)),
                   options = downscale_control(MAX_EXP = log(.Machine$double.xmax)-200))
}} else {

  if(nrow(priors3)!=0){
    res1 = downscale(targets = curr.targets,
                     start.areas = curr.lu_levels,
                     xmat = curr.X,
                     betas = curr.betas %>% filter(ks %in% unique(curr.X$ks)) %>% mutate(ks=as.character(ks)),
                     priors = priors3,
                     options = downscale_control(MAX_EXP = log(.Machine$double.xmax)-200))
  } else {
    res1 = downscale(targets = curr.targets,
                     start.areas = curr.lu_levels,
                     xmat = curr.X,
                     betas = curr.betas %>% filter(ks %in% unique(curr.X$ks)) %>% mutate(ks=as.character(ks)),
                     options = downscale_control(MAX_EXP = log(.Machine$double.xmax)-200))
  }

}




  # chck.targets =   curr.targets %>%# filter(value>1) %>%
  #   left_join(
  #     res1$out.res %>%
  #       group_by(lu.from,lu.to,times) %>%
  #       summarize(downscale.value = sum(value),.groups = "keep"),by = c("lu.from", "lu.to","times") ) %>%
  #   mutate(diff = value - downscale.value)



 ### identify undistributed changes
  luc.temp <- res1$out.res
 #
 #  to.dist <- chck.targets %>% filter(abs(diff)>0.01) %>% dplyr::select(times, lu.from, lu.to, diff) %>%
 #    rename("value"="diff")
 #
 #  if(nrow(to.dist)!=0){
 #
 #    if(any(to.dist$value<0)){
 #      negatives <- to.dist %>% filter(value<0)
 #    correct.lus <- unique(negatives[,c(2,3)])
 #    jjj <- 1
 #      for(jjj in 1:nrow(correct.lus)){
 #      luc.temp.pos <- luc.temp %>% filter(lu.from==correct.lus$lu.from[jjj] , lu.to==correct.lus$lu.to[jjj]) %>%
 #        mutate(weighting=value/sum(value))
 #      distribution <- negatives[jjj,] %>% full_join(luc.temp.pos %>% dplyr::select(-value)) %>%
 #        mutate(value=value*weighting) %>% dplyr::select(-weighting)
 #      luc.temp <- luc.temp %>% bind_rows(distribution) %>%
 #        group_by(times, lu.from, lu.to,ns) %>% summarise(value=sum(value))
 #      correction.fact <- distribution %>% group_by(lu.from, ns) %>% summarise(corr.fac=sum(value)) %>%
 #        mutate(lu.to=lu.from)
 #
 #
 #      luc.temp <- luc.temp %>% left_join(correction.fact) %>%
 #        mutate(value=ifelse(is.na(corr.fac),value,value-corr.fac)) %>% dplyr::select(-corr.fac)
 #      }
 #
 #
 #    }
 #
 #    if(any(to.dist$value>0)){
 #      positives <- to.dist %>% filter(value>0)
 #      correct.lus <- unique(positives$lu.from)
 #
 #      luc.temp.pos <- luc.temp %>% filter(lu.from%in%correct.lus) %>% filter(lu.to==lu.from) %>% group_by(lu.from) %>%
 #        mutate(weighting=value/sum(value)) %>% dplyr::select(-lu.to)
 #      distribution <- positives %>% full_join(luc.temp.pos %>% dplyr::select(-value)) %>%
 #        mutate(value=value*weighting) %>% dplyr::select(-weighting)
 #      luc.temp <- luc.temp %>% bind_rows(distribution) %>%
 #        group_by(times, lu.from, lu.to,ns) %>% summarise(value=sum(value))
 #      correction.fact <- distribution %>% group_by(lu.from, ns) %>% summarise(corr.fac=sum(value)) %>%
 #        mutate(lu.to=lu.from)
 #
 #
 #      luc.temp <- luc.temp %>% left_join(correction.fact) %>%
 #        mutate(value=ifelse(is.na(corr.fac),value,value-corr.fac)) %>% dplyr::select(-corr.fac)
 #      if(any(luc.temp$value<0)){warning('ALAAAARM')}
 #    }


  # }


  # quick.test <-   curr.targets %>%
  #   left_join(
  #     luc.temp %>%
  #       group_by(lu.from,lu.to,times) %>%
  #       summarize(downscale.value = sum(value),.groups = "keep"),by = c("lu.from", "lu.to","times") ) %>%
  #   mutate(diff = value - downscale.value)

  #### prepare for next timestep


  curr.lu_levels <- luc.temp %>% group_by(ns, lu.to) %>% summarise(value=sum(value)) %>% rename("lu.from"="lu.to")
  curr.lu_levels <- data.frame(curr.lu_levels) %>% mutate(value=ifelse(value<0,0,value))

  curr.X <- curr.X %>% left_join(curr.lu_levels %>% rename("ks"="lu.from", "update.v"="value")) %>%
    mutate(value=ifelse(is.na(update.v),value,update.v)) %>% dplyr::select(-update.v)
  curr.X <- data.frame(curr.X)



res.all <- res.all %>% bind_rows(luc.temp)

}
save(res.all, file = "output/CSDS.RData")

