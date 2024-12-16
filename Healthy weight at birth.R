# ScotPHO indicators: Healthy weight at birth (HWB and CYP profile)

# Dec 2024: indicator definition was revised to define healthy weight as 'appropriate weight for gestational age'
# ScotPHO apply restrictions to our indicator to remove impact of variation cause by things like prematurity/multiple births
# Scotpho indicator only considers live singleton fullterm births - which may lead to differences with official births in scotland publication.

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.


birthweight <- read_csv(paste0(data_folder, "Received Data/Healthy birth weight/IR2024-00846_live_singleton_births_amended.csv")) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(datazone = datazone2011, numerator = live_fullterm_singletons_appropriate_weight, denominator = live_fullterm_singleton_births) %>% 
  mutate(year = substr(finyear, start=1, stop=4)) %>%  # Fyear coded by year ending Mar31. Change to match profiles. 
  select(datazone, year, numerator, denominator)

saveRDS(birthweight, file=paste0(data_folder, 'Prepared Data/healthy_birth_raw.rds'))

#Deprivation file
birthweight_dep <- birthweight %>% filter(year>=2014) 

saveRDS(birthweight_dep, file=paste0(data_folder, 'Prepared Data/healthy_birth_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "healthy_birth", geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2023, time_agg = 3)

analyze_second(filename = "healthy_birth", measure = "percent", time_agg = 3, 
               ind_id = 21105, year_type = "financial")

#Deprivation analysis function
#Deprivation function could technically be run be results suggest limited difference between quintiles - possibly needs more investigation
# into whether this information might be useful
# analyze_deprivation(filename="healthy_birth_depr", measure="percent", time_agg=3, 
#                     yearstart= 2014, yearend=2023, year_type = "financial", ind_id = 21105)

##END


