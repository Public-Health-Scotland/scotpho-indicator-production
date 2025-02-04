# ScotPHO indicators: Maternal obesity

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
mat_obesity <- read_csv(paste0(data_folder, "Received Data/Maternal Obesity/IR2024-00846_obesity.csv")) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(year = finyear, datazone = datazone2011, numerator = obese, denominator = all_known) %>% 
  mutate(year = substr(year, start=1, stop=4)) # Fyear coded by year ending Mar31. Change to match profiles. 

saveRDS(mat_obesity, file=paste0(data_folder, 'Prepared Data/mat_obesity_raw.rds'))

#Deprivation file
mat_obesity_dep <- mat_obesity %>% filter(year>=2014) 

saveRDS(mat_obesity_dep, file=paste0(data_folder, 'Prepared Data/mat_obesity_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#CYP profile
analyze_first(filename = "mat_obesity", geography = "datazone11", measure = "percent", 
              yearstart = 2010, yearend = 2023, time_agg = 3)

analyze_second(filename = "mat_obesity", measure = "percent", time_agg = 3, 
               ind_id = 13021, year_type = "financial")

#Deprivation analysis function
analyze_deprivation(filename="mat_obesity_depr", measure="percent", time_agg=3, 
                    yearstart= 2014, yearend=2023, year_type = "financial", ind_id = 13021)

##END