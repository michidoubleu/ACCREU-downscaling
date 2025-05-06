# shifters <- tech_shifter
# curr.region <- curr.N
# curr.time <- as.character("2010")
# mapping <- simu_region
# SCEN1 <- curr.SCEN1
# SCEN2 <- curr.SCEN2
# SCEN3 <- curr.SCEN3

options(scipen = 10)
get.priors.crop <- function(prices, yield, shifters, mapping, actual.trans, curr.region, SCEN1="SSP2", SCEN2="GHG_ALL_2030_000_RUN00018", SCEN3="scenRCPref", curr.time){
  ns <- mapping$SimUID[mapping$REGION==curr.region]
  prices <- prices %>% filter(REGION==curr.region, ALLSCEN1==SCEN1, ALLSCEN2==SCEN2, ALLSCEN3==SCEN3)
  shifters <- shifters %>% filter(ALLSCEN1==SCEN1, REGION==curr.region)
  yield <- yield %>% filter(SimUID %in% ns)


  actual.trans1 <- actual.trans[nchar(actual.trans)==15]

  times <- curr.time
  full.table <- expand.grid(ns=ns, time=times, lu.fromto=actual.trans1) %>%
    mutate(lu.from=substr(lu.fromto,1,7), lu.to=substr(lu.fromto,9,15)) %>% filter(lu.from!=lu.to) %>% dplyr::select(-lu.fromto)

  full.table <- full.table %>% mutate(weight=1) %>%
    left_join((yield %>% rename("yield.to"="value", "lu.to"="CROPSYS", "ns"="SimUID"))) %>%
    left_join(yield %>% rename("yield.from"="value", "lu.from"="CROPSYS", "ns"="SimUID")) %>%
    mutate(lu.from.crop=substr(lu.from,1,4), lu.to.crop=substr(lu.to,1,4)) %>%
    left_join(prices %>% dplyr::select(item, AllScenYear, price) %>%
                rename("lu.from.crop"="item", "time"="AllScenYear", "price.from"="price")) %>%
    left_join(prices %>% dplyr::select(item, AllScenYear, price) %>%
                rename("lu.to.crop"="item", "time"="AllScenYear", "price.to"="price"))

  gc()

  full.table <- full.table %>%
    left_join(shifters %>% dplyr::select(SPECIES, time, tech_shifter) %>%
                rename("lu.from.crop"="SPECIES", "shifter.from"="tech_shifter"))%>%
    left_join(shifters %>% dplyr::select(SPECIES, time, tech_shifter) %>%
                rename("lu.to.crop"="SPECIES", "shifter.to"="tech_shifter"))

  full.table[is.na(full.table)] <- 0

  full.table <- full.table %>% group_by(lu.from) %>%
    mutate(unnnorm.prior=(yield.to*shifter.to*price.to)-(yield.from*shifter.from*price.from)) %>%
    mutate(value=(unnnorm.prior-min(unnnorm.prior, na.rm = T))/(max(unnnorm.prior, na.rm = T)-min(unnnorm.prior, na.rm = T))) %>%
    mutate(time=time, ns=as.character(ns)) %>% ungroup() %>%
    dplyr::select(ns, time, lu.from, lu.to, value, weight) %>% rename("times"="time")

  full.table[is.na(full.table)] <- 0

  actual.trans2 <- actual.trans[nchar(actual.trans)==14]

  if(length(actual.trans2)!=0){
  full.table2 <-
    expand.grid(ns=ns, time=times, lu.fromto=actual.trans2) %>% mutate(weight=0.5) %>%
    mutate(lu.from=gsub("[/].*","",lu.fromto),lu.to=gsub("^(.*[/])","",lu.fromto)) %>%
    filter(lu.from!=lu.to) %>% dplyr::select(-lu.fromto)

  full.table2 <- full.table2 %>% mutate(weight=0.5) %>%
    left_join((yield %>% rename("yield.to"="value", "lu.to"="CROPSYS", "ns"="SimUID"))) %>%
    left_join(yield %>% rename("yield.from"="value", "lu.from"="CROPSYS", "ns"="SimUID")) %>%
    mutate(lu.from.crop=substr(lu.from,1,4), lu.to.crop=substr(lu.to,1,4)) %>%
    left_join(prices %>% dplyr::select(item, AllScenYear, price) %>%
                rename("lu.from.crop"="item", "time"="AllScenYear", "price.from"="price")) %>%
    left_join(prices %>% dplyr::select(item, AllScenYear, price) %>%
                rename("lu.to.crop"="item", "time"="AllScenYear", "price.to"="price"))

  gc()

  full.table2 <- full.table2 %>%
    left_join(shifters %>% dplyr::select(SPECIES, time, tech_shifter) %>%
                rename("lu.from.crop"="SPECIES", "shifter.from"="tech_shifter"))%>%
    left_join(shifters %>% dplyr::select(SPECIES, time, tech_shifter) %>%
                rename("lu.to.crop"="SPECIES", "shifter.to"="tech_shifter"))



  full.table2[is.na(full.table2)] <- 0


  full.table2 <- full.table2 %>% group_by(lu.from) %>%
    mutate(value=(yield.to*shifter.to*price.to-yield.from*shifter.from*price.from),
           time=time, ns=as.character(ns)) %>% mutate(value=ifelse(value<0,1/abs(value),value)) %>%
    mutate(value=(value-min(value))/(max(value)-min(value))) %>%
    dplyr::select(ns, time, lu.from, lu.to, value, weight) %>% rename("times"="time")





  full.table <- full.table %>% bind_rows(full.table2)
  }


  return(full.table)

}