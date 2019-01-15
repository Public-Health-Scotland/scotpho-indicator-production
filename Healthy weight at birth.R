# ScotPHO indicators: Healthy weight at birth

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
birthweight <- read_csv(paste0(data_folder, "Received Data/IR2018-02001_birthweight.csv")) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(year = finyear, datazone = datazone2011, numerator = numerator_normal) %>% 
  filter(!is.na(datazone)) %>%  # exclude rows with no datazone.
  mutate(year = year - 1) %>%  # Fyear coded by year ending Mar31. Change to match profiles. 
  select(-numerator_low)

saveRDS(birthweight, file=paste0(data_folder, 'Prepared Data/healthy_birth_raw.rds'))

#Deprivation file
birthweight_dep <- birthweight %>% filter(year>=2014) 

saveRDS(birthweight_dep, file=paste0(data_folder, 'Prepared Data/healthy_birth_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#CYP profile
analyze_first(filename = "healthy_birth", geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2017, time_agg = 3)

analyze_second(filename = "healthy_birth", measure = "percent", time_agg = 3, 
               ind_id = 21105, year_type = "financial", profile = "HN", min_opt = 1445673)

#Deprivation analysis function
analyze_deprivation(filename="healthy_birth_depr", measure="percent", time_agg=3, 
                    yearstart= 2014, yearend=2017, year_type = "financial", ind_id = 21105)

##END