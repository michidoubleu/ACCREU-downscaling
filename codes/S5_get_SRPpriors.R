load("./input/SRP_prior.RData")

# 
# curr.region <- curr.N
# SRP <- curr.SRP_Suit
# mapping <- simu_region
# SCEN1 <- curr.SCEN1
# SCEN2 <- curr.SCEN2
# SCEN3 <- curr.SCEN3


get.priors.SRP <- function(SRP,mapping, actual.from, actual.to, curr.region, SCEN1="SSP2", SCEN2="GHG_ALL_2030_000_RUN00018", SCEN3="scenRCPref", times){
  ns <- as.character(mapping$SimUID[mapping$REGION==curr.region])

  if(length(actual.from)!=0){
  SRP_from <- expand.grid(ns=ns, times=times, lu.from=actual.from, lu.to="PltFor")
  SRP_from <- SRP_from %>% mutate(weight=1) %>% 
       left_join(SRP)
  SRP_from[is.na(SRP_from)] <- 0
  full.table <- SRP_from 
  }
  
  if(length(actual.to)!=0){
    SRP_to <- expand.grid(ns=ns, times=times, lu.to=actual.to, lu.from="PltFor")
    SRP_to <- SRP_to %>% mutate(weight=1) %>% 
      left_join(SRP) %>% mutate(value = 1 / value) %>% mutate(value = value / max(value))
    SRP_to[is.na(SRP_to)] <- 0
    
    
    if(length(actual.from)!=0){
      full.table <- full.table %>% bind_rows(SRP_to) 
    } else {
      full.table <- SRP_to 
  }
  }
  
  return(full.table)
  
}