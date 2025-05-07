########### read in GLOBIOM results

files <- list.files("./input/", full.names = TRUE)
jj <- files[grep("merged", files)]
a6 <- files[grep("a6", files)]

######## read and combine the right-hand-side of the LU to LUC (GLOBIOM past harmonization)
ACR_COMPARE <- rgdx.param(file.path(jj), "ACR_COMPARE") %>%
  mutate(across(everything(), as.character))
colnames(ACR_COMPARE) <- c("shit","COUNTRY", "AllColRow", "AltiClass", "SlpClass", "SoilClass", "AEZCLASS", "SPECIES", "CROPTECH", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "AllScenYear", "ACR_COMPARE")
ACR_COMPARE <- ACR_COMPARE %>%
  mutate(value = as.numeric(ACR_COMPARE)*1000/100, SCEN=paste0(ALLSCEN1,"_",ALLSCEN2,"_",ALLSCEN3)) %>%
  filter(AllScenYear=="2000") %>%
  dplyr::select(-shit, -ACR_COMPARE, -ALLSCEN1,-ALLSCEN2, -ALLSCEN3, -AllScenYear, -AEZCLASS, -AltiClass, -SlpClass, -SoilClass) %>%
  mutate(CROPTECH=recode(CROPTECH, "IR_basin"="IR",
                         "IR_furrow"="IR",
                         "IR_drip"="IR",
                         "IR_sprink"="IR"))




scens <- unique(ACR_COMPARE$SCEN)[1]
ACR_COMPARE <- ACR_COMPARE %>% filter(SCEN==scens) %>% dplyr::select(-SCEN) %>% mutate(CROPSYS=paste0(SPECIES,'_', CROPTECH)) %>% dplyr::select(COUNTRY, AllColRow, CROPSYS, value)
colnames(ACR_COMPARE) <- c("country", "LUID","CROPSYS", "value")
ACR_COMPARE <- ACR_COMPARE %>% group_by(country, LUID, CROPSYS) %>% summarise(value=sum(value))



Land_Compare2 <- rgdx.param(file.path(jj), "Land_Compare2") %>%
  mutate(across(everything(), as.character))
colnames(Land_Compare2) <- c("shit","COUNTRY", "AllColRow", "AltiClass", "SlpClass", "SoilClass", "AEZCLASS", "LU", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "AllScenYear", "Land_Compare2")
Land_Compare2 <- Land_Compare2 %>%
  mutate(value = as.numeric(Land_Compare2)*1000/100, SCEN=paste0(ALLSCEN1,"_",ALLSCEN2,"_",ALLSCEN3)) %>%
  filter(AllScenYear=="2000") %>%
  dplyr::select(-shit, -Land_Compare2, -ALLSCEN1,-ALLSCEN2, -ALLSCEN3, -AllScenYear, -AEZCLASS, -AltiClass, -SlpClass, -SoilClass)
scens <- unique(Land_Compare2$SCEN)[1]
Land_Compare2 <- Land_Compare2 %>% filter(SCEN==scens) %>% dplyr::select(-SCEN)
colnames(Land_Compare2) <- c("country", "LUID","lu.class", "value")
Land_Compare2 <- Land_Compare2 %>% group_by(country, LUID, lu.class) %>% summarise(value=sum(value))


target_to <- Land_Compare2 %>% filter(lu.class != "CrpLnd") %>% bind_rows(ACR_COMPARE %>% rename(lu.class=CROPSYS))



#### mapping to SIMU now needed
load("./input/SimU_AEZ_mapping.RData")
mapping <- mapping %>% dplyr::select(SimUID, country, LUID, AEZClass)

other.areas.from <- rgdx.param(file.path(a6), "LANDCOVER_INIT_SIMUID") %>%
  mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(LANDCOVER_INIT_SIMUID)*1000/100) %>%
  dplyr::select(-LANDCOVER_INIT_SIMUID)

simu.areas <- other.areas.from %>% filter(LC_TYPES_EPIC == "SimUarea")
other.areas.from <- other.areas.from %>% filter(LC_TYPES_EPIC != "SimUarea")

other.areas.from <- mapping %>% left_join(other.areas.from %>% mutate(SimUID=as.numeric(SimUID))) %>%
  dplyr::filter(!(LC_TYPES_EPIC %in% c("CrpLnd_S", "CrpLnd_L", "CrpLnd_I", "CrpLnd_H"))) %>%
  group_by(SimUID, country, LUID, AEZClass, LC_TYPES_EPIC) %>% summarise(value=sum(value)) %>% na.omit() %>%
  rename("lu.class"="LC_TYPES_EPIC")

WDPA.areas <- rgdx.param(file.path(a6), "WDPA_Alloc") %>%
  mutate(across(everything(), as.character)) %>%
  mutate(wdpa.factor = as.numeric(WDPA_Alloc)) %>%
  dplyr::select(-WDPA_Alloc, -AltiClass, -SlpClass, -SoilClass) %>%
  rename("lu.class"="LC_TYPE", "country"="ANYREGION", "LUID"="AllColRow", "AEZClass"="AEZCLASS") %>% mutate(lu.class=recode(lu.class, "PriFor"="Forest", "NatLnd"="OthNatLnd"))

other.areas.from <- other.areas.from %>% left_join(WDPA.areas) %>% mutate(wdpa.factor=ifelse(is.na(wdpa.factor),0,wdpa.factor)) %>% mutate(protected=value*wdpa.factor, value.corr=value-protected)


other.areas.from.corr <- other.areas.from %>% group_by(SimUID, country,LUID,lu.class) %>% summarise(protected=sum(protected),value=sum(value.corr))

# prot_areas <- other.areas.from.corr %>% dplyr::select(-value) %>% mutate(lu.class=paste0("protected_",lu.class), protected=protected*1000/100) %>% rename("value"="protected")


cropsys <- read.csv("input/cropsys.csv")
CROPSYS_from <- mapping %>% dplyr::select(-AEZClass) %>% left_join(cropsys) %>%
  pivot_longer(cols = c(-SimUID, -country, -LUID ), names_to = "CROPSYS") %>% mutate(value=value/100)


target_from <- other.areas.from.corr %>% filter(lu.class != "CrpLnd") %>% bind_rows(CROPSYS_from %>% rename(lu.class=CROPSYS)) %>% mutate(protected=ifelse(is.na(protected),0,protected))



########### harmo factors per country/LU split for notrel and rel

target_from.simu <- target_from %>% group_by(SimUID) %>% summarise(value_from=sum(value))
area.compare <- target_from.simu %>% full_join(simu.areas %>% mutate(SimUID=as.numeric(SimUID)) %>% dplyr::select(-LC_TYPES_EPIC) %>% rename("value_to"="value")) %>% mutate(value_to=ifelse(is.na(value_to),0,value_to), conv.fac=value_to/value_from) %>% mutate(conv.fac=ifelse(is.na(conv.fac)|is.infinite(conv.fac),0,conv.fac)) %>% dplyr::select(SimUID, conv.fac)


target_from.corr <- target_from %>% left_join(area.compare) %>% mutate(value=value*conv.fac) %>% dplyr::select(-conv.fac)


### add mng/pri forest share

forest.split <- target_to %>% filter(lu.class %in% c("MngFor","PriFor")) %>% pivot_wider(id_cols = c(country,LUID), names_from = "lu.class", values_fill = 0) %>% mutate(mngfor.share=MngFor/(PriFor+MngFor)) %>% dplyr::select(-MngFor,-PriFor)


target_from.corr.forest <- target_from.corr %>% filter(lu.class=="Forest") %>% left_join(forest.split) %>% mutate(mngfor.share=ifelse(is.na(mngfor.share),0,mngfor.share), MngFor=value*mngfor.share, PriFor=value-MngFor) %>% dplyr::select(SimUID, country, LUID, MngFor, PriFor) %>% pivot_longer(cols = c(MngFor, PriFor), names_to = "lu.class", values_to = "value")


target_from.SimUID <- target_from.corr %>% filter(lu.class!="Forest") %>% bind_rows(target_from.corr.forest) %>% mutate(lu.class=recode(lu.class, "Grass"="GrsLnd", "OthNatLnd"="NatLnd", "OthAgri"="OagLnd"))


#### renaming to match the classes from target_to

target_from <- target_from.SimUID %>% group_by(country,LUID,lu.class) %>% summarise(value=sum(value))


still.missing <- sum(target_from$value)-sum(target_to$value)

test <- target_from.SimUID  %>% group_by(country, LUID) %>% summarise(value=sum(value)) %>% left_join(target_to %>% group_by(country, LUID) %>% summarise(value.to=sum(value))) %>% mutate(diff=(value-value.to)/value) %>% dplyr::select(country, LUID, diff) %>% na.omit()

test <- target_from.SimUID %>% left_join(test) %>% mutate(diff=ifelse(is.na(diff),0,ifelse(diff<0,0,diff)), prot.diff=value*diff, value=value-prot.diff)
to.add <- test %>% group_by(country, LUID) %>% summarise(value=sum(prot.diff)) %>% mutate(lu.class="prot.other")

target_to <- target_to %>% bind_rows(to.add)


save(target_from, target_to, mapping, target_from.SimUID, file = "output/targets_for_startmap.RData")



restrictions <- NULL

i <- "Whea_HI"

for(i in unique(ACR_COMPARE$CROPSYS)){
  temp <- mapping %>% dplyr::select(-AEZClass) %>% mutate(CROPSYS=i) %>% left_join(ACR_COMPARE) %>% mutate(value=ifelse(is.na(value),0,1))
  restrictions <- restrictions %>% bind_rows(temp)
}

restrictions <- restrictions %>% ungroup() %>% dplyr::select(-country, -LUID)
save(restrictions, file = "output/restrictions.RData")


