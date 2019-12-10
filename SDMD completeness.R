# ScotPHO indicators: SDMD follow-up completeness and SDMD initial completeness
# Raw files comes from ISD drugs team. 
# Initial: Individuals on SDMD compared with those at DATWT. 
# Follow up: Individuals on SDMD with 12 week follow up record. 

#   Part 1 - Prepare basefiles
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
library(janitor)
###############################################.
## Part 1 - Prepare basefile ----
###############################################.
#Initial completeness file from drugs team
sdmd_initial <- read_csv(paste0(data_folder, "Received Data/IR2019-00925_SDMD_initial_compliance.csv")) %>%
  clean_names() %>% 
  rename(denominator = sdmdpeople, numerator = datwtpeople, hb2019name = location_code) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

#sdmd_initial[, 4:6][sdmd_initial[, 4:6] == "X"] <- NA
#sdmd_initial[, 4:6][sdmd_initial[, 4:6] == "*"] <- NA

#lookup for geogrpahy codes
geography_codes <- readRDS('/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/codedictionary.rds') %>% 
  setNames(tolower(names(.))) %>%
  rename(hb2019name = areaname)
  
sdmd_initial <- left_join(sdmd_initial, geography_codes, "hb2019name") %>%
  select(code, hb2019name, year, numerator, denominator) %>%
  mutate(year = as.numeric(substr(year,1,4))) %>%
  select(year, code, numerator, denominator)

saveRDS(sdmd_initial, file=paste0(data_folder, 'Temporary/sdmd_initialcompl_formatted.rds'))

#Follow-up completeness file from drugs team
sdmd_follow <- read_csv(paste0(data_folder, "Received Data/IR2019-00925_SDMD_followup_completeness.csv")) %>%
  clean_names() %>% 
  rename(denominator = sdmdpeople, numerator = followuppeople, hb2019name = location) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

#sdmd_follow[, 4:6][sdmd_follow[, 4:6] == "X"] <- NA
#sdmd_follow[, 4:6][sdmd_follow[, 4:6] == "*"] <- NA

#adding geography codes
sdmd_follow <- left_join(sdmd_follow, geography_codes, "hb2019name") %>%
  select(code, hb2019name, year, numerator, denominator) %>%
  mutate(year = as.numeric(substr(year,1,4))) %>%
  select(year, code, numerator, denominator)
  
saveRDS(sdmd_follow, file=paste0(data_folder, 'Temporary/sdmd_follow_formatted.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#Initial completeness
analyze_second(filename = "sdmd_initialcompl", measure = "percent", time_agg = 1, 
               ind_id = 4137, year_type = "financial")

# Merging with old adp data that it has not been produced in latest years because 
# of data quality issues
sdmd_shiny_initial <- rbind(readRDS(paste0(data_folder, "Shiny Data/sdmd_initialcompl_shiny.rds")),
                      readRDS(paste0(data_folder, "Prepared Data/sdmd_adp_initial_donotdelete.rds")))

saveRDS(sdmd_shiny_initial, paste0(data_folder, "Shiny Data/sdmd_initialcompl_shiny.rds"))
write_csv(sdmd_shiny_initial, paste0(data_folder, "Shiny Data/sdmd_initialcompl_shiny.csv"))

###############################################.
# Follow-up completeness
analyze_second(filename = "sdmd_follow", measure = "percent", time_agg = 1, 
               ind_id = 4138, year_type = "financial")
##END