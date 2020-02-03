# WORK N PROGRESS - Hep C in PWID indicator

#   Part 1 - read in data
#   Part 2 - Prepare geographies
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Read in data ----
###############################################.

hepc_pwid <- read_csv(paste0(data_folder, "Received Data/Hep C Among PWID_2008-09 to 2017-18_compiled.csv")) %>%
  setNames(tolower(names(.))) %>% 
  rename("areaname" = "code") %>%
  mutate(areaname = recode(areaname, "AA" = "NHS Ayrshire and Arran", "BR" = "NHS Borders", "DG" = "NHS Dumfries and Galloway",
                           "FF" = "NHS Fife", "FV" = "NHS Forth Valley", "GGC" = "NHS Greater Glasgow and Clyde",
                           "GR" = "NHS Grampian", "HG" = "NHS Highland", "LN" = "NHS Lanarkshire", "LO" = "NHS Lothian",
                           "TY" = "NHS Tayside")) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

geography_codes <- readRDS('/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/codedictionary.rds') %>% 
  setNames(tolower(names(.))) 

hepc_pwid <- left_join(hepc_pwid, geography_codes, "areaname") %>%
  mutate(year = as.numeric(substr(year,1,4))) %>% #format as financial year
  distinct(year, areaname, .keep_all =TRUE) 
  
hepc_pwid2 <- hepc_pwid %>%
  select(year, code, numerator, denominator)

saveRDS(hepc_pwid2, file=paste0(data_folder, 'Temporary/hepc_pwid2_formatted.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_second(filename = "hepc_pwid2", measure = "percent", time_agg = 1, 
               ind_id = 4122, year_type = "financial")

