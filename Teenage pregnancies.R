# ScotPHO indicators: teenage pregnancies #

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

# Reading data provided by maternity team
# All data is included, but to calculate the rates only 15-19 female pop is used, 
# following ISD publication methodology.
teen_preg <- read_csv( paste0(data_folder, "Received Data/IR2019-00921_teenpregnancy.csv")) %>% 
  setNames(tolower(names(.))) %>% #set names to lower case
  rename(datazone = datazone2011, numerator = tp, year = yearcon) %>% 
  subset(!(is.na(datazone) | datazone == "         ")) %>% #excluding non-Scottish residents
  # aggregate to get the count, removing age groups
  group_by(year, datazone) %>% 
  summarise(numerator = sum(numerator, na.rm =T)) %>% ungroup()

saveRDS(teen_preg, file=paste0(data_folder, 'Prepared Data/teen_preg_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "teen_preg", geography = "datazone11", measure = "crude", 
              yearstart = 2002, yearend = 2017, time_agg = 3, pop='DZ11_pop_fem15to19')

analyze_second(filename = "teen_preg", measure = "crude", time_agg = 3, crude_rate=1000,
               ind_id = 21001, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="teen_preg", measure="crude", time_agg=3, crude_rate=1000,
                    yearstart= 2014, yearend=2017, year_type = "calendar", 
                    pop = "depr_pop_fem15to19", ind_id = 21001)

##END