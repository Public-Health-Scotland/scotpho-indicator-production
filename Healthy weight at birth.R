# ScotPHO indicators: Healthy weight at birth

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
birthweight <- read_csv(paste0(data_folder, "Received Data/Healthy birth weight/IR2024-00846_live_singleton_births.csv")) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(datazone = datazone2011, numerator = fullterm_appropriate_weight, denominator = total_singleton_births) %>% 
  mutate(year = substr(finyear, start=1, stop=4)) %>%  # Fyear coded by year ending Mar31. Change to match profiles. 
  select(datazone, year, numerator, denominator)

saveRDS(birthweight, file=paste0(data_folder, 'Prepared Data/healthy_birth_raw.rds'))

#Deprivation file
birthweight_dep <- birthweight %>% filter(year>=2014) 

saveRDS(birthweight_dep, file=paste0(data_folder, 'Prepared Data/healthy_birth_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#CYP profile
analyze_first(filename = "healthy_birth", geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2023, time_agg = 3)

analyze_second(filename = "healthy_birth", measure = "percent", time_agg = 3, 
               ind_id = 21105, year_type = "financial")

#Deprivation analysis function
#Deprivation function not applicable for this indicator
#analyze_deprivation(filename="healthy_birth_depr", measure="percent", time_agg=3, 
                    #yearstart= 2014, yearend=2022, year_type = "financial", ind_id = 21105)

##END


