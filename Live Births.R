# ScotPHO indicators: Live births

# Part 1 - Format raw data ready for analysis functions
# Part 2 - calling the analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.
live_births <- read_excel(paste0(data_folder,
                                 "Received Data/Births 2002-2020 datazone_2011.xlsx")) %>%
  setNames(tolower(names(.))) %>%
  rename(year = "registration year", datazone = datazone_2011) %>%
  group_by(year, datazone) %>%
  summarise(numerator = sum(count, na.rm = TRUE), .groups = "drop")

saveRDS(live_births, file=paste0(data_folder, 'Prepared Data/live_births_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "live_births", geography = "datazone11", measure = "crude",
              yearstart = 2002, yearend = 2020, time_agg = 1, pop ='DZ11_pop_allages')



analyze_second(filename = "live_births", measure = "crude", time_agg = 1, crude_rate=1000,
               ind_id = 20008, year_type = "calendar")



