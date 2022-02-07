# ScotPHO indicators: Premature births

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
premature <- read_csv(paste0(data_folder, "Received Data/IR2022-00008_premature.csv")) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(year = finyear, datazone = datazone2011, numerator = live_pre_term, 
         denominator = all_live_births) %>% 
  mutate(year = year - 1) # Fyear coded by year ending Mar31. Change to match profiles. 

saveRDS(premature, file=paste0(data_folder, 'Prepared Data/premature_births_raw.rds'))

#Deprivation file
premature_dep <- premature %>% filter(year>=2014) 

saveRDS(premature_dep, file=paste0(data_folder, 'Prepared Data/premature_births_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#CYP profile
analyze_first(filename = "premature_births", geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2020, time_agg = 3)

analyze_second(filename = "premature_births", measure = "percent", time_agg = 3, 
               ind_id = 13022, year_type = "financial")

#Deprivation analysis function
analyze_deprivation(filename="premature_births_depr", measure="percent", time_agg=3, 
                    yearstart= 2014, yearend=2020, year_type = "financial", ind_id = 13022)


##END

