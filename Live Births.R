# ScotPHO indicators: Live births

# Part 1 - Format raw data ready for analysis functions
# Part 2 - calling the analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") # source functions & libraries to run script

# Old functions can be deleted once new functions have been embedded 
# source("1.indicator_analysis.R") #Normal indicator functions
# source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.

# live births data sourced via bespoke request to NRS
live_births <- read_excel(paste0(scotpho_folder,"/Received Data/Live births/Births 2002-2023 datazone_2011.xlsx")) %>%
  setNames(tolower(names(.))) %>%
  rename(year = "registration year", datazone = datazone_2011) %>%
  group_by(year, datazone) %>%
  summarise(numerator = sum(count, na.rm = TRUE), .groups = "drop")

saveRDS(live_births, file=paste0(scotpho_folder, '/Prepared Data/live_births_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.


#call main analysis function 
main_analysis(filename = "live_births",  measure = "crude",
  geography = "datazone11",  year_type = "calendar",  ind_id = 20008, 
  time_agg = 1,  yearstart = 2002,   yearend = 2023, pop = 'DZ11_pop_allages', 
  crude_rate = 1000, # rate is crude rate per 1000
  test_file = FALSE, QA = TRUE)


#calls to old functions - lines can be deleted once we are sure new analysis functions are working ok.
#analyze_first(filename = "live_births", geography = "datazone11", measure = "crude",
#              yearstart = 2002, yearend = 2023, time_agg = 1, pop ='DZ11_pop_allages')
# 
#analyze_second(filename = "live_births", measure = "crude", time_agg = 1, crude_rate=1000,
#                ind_id = 20008, year_type = "calendar")
# 
