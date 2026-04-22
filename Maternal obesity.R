# ScotPHO indicators: Maternal obesity

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
mat_obesity <- read_csv(file.path(profiles_data_folder, "Received Data/Maternal Obesity/IR2026-00003_obesity.csv")) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(year = finyear, datazone = datazone2011, numerator = obese, denominator = all_known) %>% 
  fix_fin_year("year", "4") |> 
  mutate(datazone = dplyr::na_if(datazone, "unknown")) #replacing "unknown" datazones w/ NA so they're still added to Scotland totals. #have to specify package as hablar is masking dplyr::na_if

saveRDS(mat_obesity, file.path(profiles_data_folder, 'Prepared Data/mat_obesity_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#Main analysis
main_analysis(filename = "mat_obesity", geography = "datazone11", measure = "percent",
              yearstart = 2010, yearend = 2024, time_agg = 3, ind_id = 13021, year_type = "financial")

#Deprivation analysis function
deprivation_analysis(filename = "mat_obesity", measure ="percent", time_agg = 3, 
                    yearstart = 2014, yearend = 2024, year_type = "financial", ind_id = 13021)

##END