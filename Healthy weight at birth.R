# ScotPHO indicators: Healthy weight at birth (HWB and CYP profile)

# Dec 2024: indicator definition was revised to define healthy weight as 'appropriate weight for gestational age'
# ScotPHO apply restrictions to our indicator to remove impact of variation cause by things like prematurity/multiple births
# Scotpho indicator only considers live singleton fullterm births - which may lead to differences with official births in scotland publication.

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function
source("./functions/data cleaning functions/fix_fin_year.R")

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

birthweight <- read_csv(file.path(profiles_data_folder, "Received Data/Healthy birth weight/IR2026-00003_fullterm_awga.csv")) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(datazone = datazone2011, numerator = live_fullterm_singletons_appropriate_weight, denominator = live_fullterm_singleton_births) %>% 
  fix_fin_year("finyear", "4") %>%  # convert fin_year to first year and rename to year
  select(datazone, year, numerator, denominator) %>%
  mutate(datazone = dplyr::na_if(datazone, "unknown")) #replacing "unknown" datazones w/ NA so they're still added to Scotland totals. #have to specify package as hablar is masking dplyr::na_if

saveRDS(birthweight, file.path(profiles_data_folder, 'Prepared Data/healthy_birth_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "healthy_birth", geography = "datazone11", measure = "percent",
              yearstart = 2002, yearend = 2024, time_agg = 3, ind_id = 21105,
              year_type = "financial")

#Deprivation analysis function
#Deprivation function could technically be run be results suggest limited difference between quintiles - possibly needs more investigation
# into whether this information might be useful
# analyze_deprivation(filename="healthy_birth_depr", measure="percent", time_agg=3, 
#                     yearstart= 2014, yearend=2023, year_type = "financial", ind_id = 21105)

##END


