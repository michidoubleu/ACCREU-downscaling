# Define your config file path
config_path <- "config_5_priors.R"

files <- list.files("./input", full.names = TRUE)
merged <- files[grep("merged", files)]
ACR_COMPARE <- rgdx.param(file.path(merged), "ACR_COMPARE") %>%
  mutate(across(everything(), as.character))
colnames(ACR_COMPARE) <- c("shit", "COUNTRY", "AllColRow","AltiClass","SlpClass","SoilClass",
                           "AEZCLASS","SPECIES", "CROPTECH","ALLSCEN1","ALLSCEN2","ALLSCEN3",
                           "AllScenYear","ACR_COMPARE")
ACR_COMPARE <- ACR_COMPARE %>% mutate(value = as.numeric(ACR_COMPARE)*1000/100,
                                      SCEN=paste0(ALLSCEN1,"_",ALLSCEN2,"_",ALLSCEN3))

# Define the current list of jobs
current_jobs <- 1:length(unique(ACR_COMPARE$SCEN))
rm("ACR_COMPARE")

# Read the config file
config_lines <- readLines(config_path)

# Update the JOBS line
config_lines <- gsub(
  pattern = "^JOBS = .*",
  replacement = paste0("JOBS = c(",  paste(current_jobs[1],current_jobs[length(current_jobs)],  sep = ":"), ")"),
  x = config_lines
)

# Write the updated config back to the file
writeLines(config_lines, config_path)