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
live_births <- read.csv(paste0(data_folder, "Received Data/births by 2011 data zones and sex - 2002 -2018.csv")) %>%
  setNames(tolower(names(.))) %>%
  mutate(male = case_when(male == "-" ~ "0", TRUE ~ paste0(male)),
         female = case_when(female == "-" ~ "0", TRUE ~ paste0(female)),
         male = as.numeric(as.character(male)),
         female = as.numeric(as.character(female))) %>%
  filter(!is.na(datazone)) %>% # exclude rows with no datazone.
  group_by(year, datazone) %>%
  mutate(total = sum(male, female)) %>%
  summarise(numerator = sum(total, na.rm =T)) %>% ungroup()
         
         
# pivot_longer(-c(year, datazone), names_to = "sex", values_to = "value")
# ?pivot_longer
# mutate(datazone = case_when(is.na(datazone) ~ lag(datazone),
#                              TRUE ~ datazone)) %>%
#mutate(sex = if_else(sex == 1, "male", "female")) %>%
  

saveRDS(live_births, file=paste0(data_folder, 'Prepared Data/live_births_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "live_births", geography = "datazone11", measure = "crude", 
              yearstart = 2002, yearend = 2018, time_agg = 1, pop ='DZ11_pop_allages')

analyze_second(filename = "live_births", measure = "crude", time_agg = 1, crude_rate=1000,
               ind_id = 20008, year_type = "calendar")
