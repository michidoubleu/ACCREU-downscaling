### map for LU_to
load(file = "./output/updated_startmap_SimUID.RData")


### get map for LU_from
Simu.map <- read.csv("./input/CELLCODE_SimU_mapping.csv")
LUC <- read.csv("./input/LUM_change.csv")
class.mapping.file <- "./input/class_mapping.csv"
class.mapping <- read.csv(class.mapping.file)
colnames(class.mapping)[1] <- "Class"

LU <- LUC %>% dplyr::select(CELLCODE, area, LUM2000) %>% filter(LUM2000!=0) %>%
  left_join(class.mapping %>% rename("LUM2000"="Class") %>% dplyr::select(-Name), by = join_by(LUM2000)) %>% group_by(CELLCODE, LU_mapping) %>% summarise(value=sum(area))
rm("LUC")

LU_simu <- LU %>% left_join(Simu.map) %>% group_by(SimUID, LU_mapping) %>% summarise(value=sum(value))  %>% filter(SimUID != 0 & LU_mapping!="NotRel")


EU.simus <- unique(LU_simu$SimUID)
updated_map.EU <- updated_map %>% filter(SimUID%in%EU.simus,)

rice <- updated_map %>% filter(lu.class=="Whea_HI")

simu.rast <- raster::raster("./input/simu_raster/w001001.adf")

# e <- extent(-25, 40, 35, 80)
# simu.rast <- crop(simu.rast,e)

rice.rast <- simu.rast
rast.vals <- data.frame(SimUID=values(rice.rast))
rast.vals <- rast.vals %>% left_join(rice %>% dplyr::select(SimUID, value))
values(rice.rast) <- rast.vals$value
plot(rice.rast)


save(updated_map.EU, file = "./output/updated_startmap_SimUID_EU.RData")
