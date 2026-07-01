# ScotPHO indicators: Premature births

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
premature <- read_csv(file.path(profiles_data_folder, "Received Data/Premature Births/IR2026-00003_premature.csv")) |> 
  clean_names() %>%   #variables to lower case
  rename(year = finyear, datazone = datazone2011, numerator = live_pre_term, 
         denominator = all_live_births) %>% 
  fix_fin_year("year", "4") %>%
  mutate(datazone = dplyr::na_if(datazone, "unknown")) #replacing "unknown" datazones w/ NA so they're still added to Scotland totals. #have to specify package as hablar is masking dplyr::na_if

saveRDS(premature, file.path(profiles_data_folder, 'Prepared Data/premature_births_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#Main analysis
main_analysis(filename = "premature_births", geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2024, time_agg = 3, ind_id = 13022, year_type = "financial")

#Deprivation analysis function
deprivation_analysis(filename ="premature_births", measure ="percent", time_agg = 3, 
                    yearstart = 2014, yearend = 2024, year_type = "financial", ind_id = 13022)


##END

