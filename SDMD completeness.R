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
library(stringr)

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

#lookup for geogrpahy codes
geography_codes <- readRDS('/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/codedictionary.rds') %>%
  setNames(tolower(names(.))) %>% #set names to lower case
  rename(hb2019name = areaname) %>%
  # Keep only the levels we need, otherwise will get ADPs matching to wrong codes
  filter(str_starts(code, "S00") | str_starts(code, "S08") | str_starts(code, "S11")) %>%
  # Need to change some of the ADP names to get a match with the SDMD data
  mutate(hb2019name = recode(hb2019name,
                             "Argyll & Bute" = "Argyll and Bute",
                             "East Ayrshire" = "Ayrshire East",
                             "North Ayrshire" = "Ayrshire North",
                             "South Ayrshire" = "Ayrshire South",
                             "Dumfries & Galloway" = "Dumfries and Galloway",
                             "East Dunbartonshire" = "Dunbartonshire East",
                             "West Dunbartonshire" = "Dunbartonshire West",
                             "Edinburgh, City of" = "Edinburgh City",
                             "West Lothian" = "Lothian West",
                             "Mid and East Lothian" = "Lothian Mid and East",
                             "Perth & Kinross" = "Perth and Kinross",
                             "East Renfrewshire" = "Renfrewshire East"))

#Initial completeness file extracted from SDMD publication
sdmd_initial <- read_xlsx(paste0(data_folder, "Received Data/SDMD Initial Completeness 2022.xlsx")) %>%
  rename(numerator = sdmd_ind, denominator = total_datwt, hb2019name = LocationCode) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

sdmd_initial <- left_join(sdmd_initial, geography_codes, "hb2019name") %>%
  mutate(year = as.numeric(substr(year,1,4))) %>% #format as financial year
  select(year, code, numerator, denominator)

saveRDS(sdmd_initial, file=paste0(data_folder, 'Temporary/sdmd_initialcompl_formatted.rds'))

# Follow-up completeness file extracted from SDMD publication
sdmd_follow <- read_xlsx(paste0(data_folder, "Received Data/SDMD Followup Completeness 2022.xlsx")) %>%
  # Prisons are counted as a health board in the data, and we don't want to include them
  filter(LocationCode != "Prisons") %>%
  # Geography lookup does not have ADP at the end of ADP location names
  mutate(LocationCode = str_replace(LocationCode, " ADP", "")) %>%
  rename(denominator = individuals, numerator = no_FUs, hb2019name = LocationCode) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

# Our lookup currently has only a single ADP for Lanarkshire, but the SDMD data
# has separate North and South Lanarkshire ADPs.
#
# Each individual is counted once per area & year, so summing would introduce
# double counting. NHS Lanarkshire is same area as North + South Lanarkshire,
# so just use board data for the combined ADP.
#
# Probably best check this is still the right approach each update.
#
# TODO: remove this when our lookups are updated

sdmd_follow_lanarkshire =
  sdmd_follow %>%
  filter(hb2019name == "NHS Lanarkshire") %>%
  mutate(hb2019name = "Lanarkshire", LocationType = "ADP", concatenated = NA)

sdmd_follow =
  sdmd_follow %>%
  filter(hb2019name != "Lanarkshire North", hb2019name != "Lanarkshire South") %>%
  bind_rows(sdmd_follow_lanarkshire)

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