### code to create a full set of targets of crop and LUC based on
### ACR_COMPARE and LUC_COMPARE_SCEN0 on a regional level.
### REGIONS defined with the new 59REGIONS for this example
### targets are per REGION, timestep, scenario(1-3)
### merged_cluster file and a2 need to be in input

files <- list.files("./input", full.names = TRUE)

merged <- files[grep("merged", files)]
mapping.file <- files[grep("a2", files)]
a6.file <- files[grep("a6", files)]
cropdat.file <- files[grep("data_crops", files)]

load("input/SimU_AEZ_mapping.RData")

mapping2 <- rgdx.set(file.path(mapping.file), "REGION59_COUNTRY_MAP") %>%
  mutate(across(everything(), as.character))
colnames(mapping2) <- c("REGION", "country")

simu_region <- full_join(mapping2, mapping %>% dplyr::select(SimUID,country)) %>% na.omit()

########### load prices
prices <- rgdx.param(file.path(merged), "Price_Compare2") %>%
  mutate(across(everything(), as.character))
colnames(prices) <- c("shit","item", "REGION","ALLSCEN1","ALLSCEN2","ALLSCEN3",
                           "AllScenYear","price")

#### no clim shifters in this GLOBIOM version
# climate_shifter <- rgdx.param(file.path(a6.file), "SHIFTER_CROPLAND_EPIC_WORLD") %>%
#   mutate(across(everything(), as.character))
# colnames(climate_shifter) <- c("shit", "COUNTRY", "AllColRow","AltiClass","SlpClass","SoilClass",
#                            "AEZCLASS","SPECIES", "CROPTECH","ALLSCEN1","ALLSCEN2","ALLSCEN3",
#                            "AllScenYear","climate_shifter")


tech_shifter <- rgdx.param(file.path(a6.file), "YLD_SSP_STAT") %>%
  mutate(across(everything(), as.character)) %>% mutate(YLD_SSP_STAT=as.numeric(YLD_SSP_STAT))
colnames(tech_shifter) <- c("ALLSCEN1", "REGION", "SPECIES","time","tech_shifter")

prices <- prices %>% filter(item %in% unique(tech_shifter$SPECIES)) %>% mutate(price=as.numeric(price))


yield <- rgdx.param(file.path(cropdat.file), "CROP_DATA") %>%
  mutate(across(everything(), as.character))
colnames(yield) <- c("country", "LUID","altitude","slope","soil_type",
                           "AEZClass","SPECIES", "CROPTECH",
                           "ITEM","value")

yield <- yield %>% full_join(mapping) %>% filter(ITEM==SPECIES) %>% filter(nchar(CROPTECH)==2) %>% dplyr::select(SimUID, SPECIES, CROPTECH, value) %>% mutate(CROPSYS=paste0(SPECIES,'_', CROPTECH)) %>% dplyr::select(-SPECIES, -CROPTECH) %>% mutate(value=as.numeric(value))


save(prices, yield, tech_shifter, simu_region, file = "./input/data_for_croppriors.RData")
