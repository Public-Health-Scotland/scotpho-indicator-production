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
#Initial completeness file extracted from SDMD publication
sdmd_initial <- read.xlsx(paste0(data_folder, "Received Data/SDMD Initial Completeness 2021.xlsx")) %>%
  rename(numerator = sdmd_ind, denominator = total_datwt, hb2019name = LocationCode) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

#lookup for geogrpahy codes
geography_codes <- readRDS('/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/codedictionary.rds') %>%
  setNames(tolower(names(.))) %>% #set names to lower case
  rename(hb2019name = areaname)

sdmd_initial <- left_join(sdmd_initial, geography_codes, "hb2019name") %>%
  mutate(year = as.numeric(substr(year,1,4))) %>% #format as financial year
  select(year, code, numerator, denominator)

saveRDS(sdmd_initial, file=paste0(data_folder, 'Temporary/sdmd_initialcompl_formatted.rds'))

# Follow-up completeness file extracted from SDMD publication
sdmd_follow <- read.xlsx(paste0(data_folder, "Received Data/SDMD Followup Completeness 2021.xlsx")) %>%
  # We don't want anything below HB level
  filter(LocationType == "Scotland" | LocationType == "Health Board") %>%
  # Prisons are counted as a health board in the data, and we don't want to include them
  filter(LocationCode != "Prisons") %>%
  rename(denominator = individuals, numerator = no_FUs, hb2019name = LocationCode) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

#adding geography codes
sdmd_follow <- left_join(sdmd_follow, geography_codes, "hb2019name") %>%
  mutate(year = as.numeric(substr(year,1,4))) %>% #format as financial year
  select(year, code, numerator, denominator)

saveRDS(sdmd_follow, file=paste0(data_folder, 'Temporary/sdmd_follow_formatted.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#Initial completeness
analyze_second(filename = "sdmd_initialcompl", measure = "percent", time_agg = 1,
               ind_id = 4137, year_type = "financial")

# Follow-up completeness
analyze_second(filename = "sdmd_follow", measure = "percent", time_agg = 1,
               ind_id = 4138, year_type = "financial")

##END