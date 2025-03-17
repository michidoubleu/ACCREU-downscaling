# Define your config file path
config_path <- "config_8_CSDS.R"

load("./output/updated_startmap_SimUID_EU.RData")
load("./output/targets_ACR_LUC_ACCREU.RData")

updated_map <- updated_map.EU %>% mutate(SimUID=as.numeric(SimUID)) %>% ungroup() %>% dplyr::select(-country) %>% left_join(simu_region %>% dplyr::select(SimUID,REGION))

Ns <- unique(updated_map$REGION)
Ns <- Ns[!Ns%in%c("Former_USSR", "NorthernAf")]
scens <- unique(full.targets$scen)
times <- unique(full.targets$time)

est.grid = expand.grid(Ns = Ns, scens=scens)


# Define the current list of jobs
current_jobs <- 1:nrow(est.grid)

# Read the config file
config_lines <- readLines(config_path)

# Update the JOBS line
config_lines <- gsub(
  pattern = "^JOBS = .*",
  replacement = paste0("JOBS = c(", paste(current_jobs[1],current_jobs[length(current_jobs)],  sep = ":"), ")"),
  x = config_lines
)

# Write the updated config back to the file
writeLines(config_lines, config_path)