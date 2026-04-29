# ScotPHO indicators: teenage pregnancies #

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

# Reading data provided by maternity team
# All data is included, but to calculate the rates only 15-19 female pop is used, 
# following ISD publication methodology.
teen_preg <- read_csv(file.path(profiles_data_folder, "Received Data/Teenage pregnancies/IR2026-00003_TeenPregs.csv")) %>% 
  clean_names() %>% #set names to lower case
  rename(datazone = datazone2011, numerator = tp, year = yearcon) %>% 
  subset(!(is.na(datazone) | datazone == "         " | datazone == "Unknown")) %>% #excluding non-Scottish residents
  # aggregate to get the count, removing age groups
  group_by(year, datazone) %>% 
  summarise(numerator = sum(numerator, na.rm =T)) %>% ungroup()

saveRDS(teen_preg, file.path(profiles_data_folder, 'Prepared Data/teen_preg_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "teen_preg", geography = "datazone11", measure = "crude",
              yearstart = 2002, yearend = 2023, time_agg = 3, pop = "DZ11_pop_fem15to19",
              ind_id = 21001, year_type = "calendar", crude_rate = 1000)

#Deprivation analysis function
deprivation_analysis(filename="teen_preg", measure="crude", time_agg=3, crude_rate=1000,
                    yearstart= 2014, yearend=2023, year_type = "calendar", 
                     ind_id = 21001)

##END