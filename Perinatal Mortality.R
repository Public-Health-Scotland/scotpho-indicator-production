# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  Indicator: Perinatal Mortality Rate (Inidicator ID 99139)

# Description: Perinatal Mortality Rate per 1,000 births (stillbirths plus deaths in the first week of life) 

# Data source is Section 4: Stillbirths and infant deaths from NRS:
# Section 4: Stillbirths and infant deaths (specifically Table 4.02) 
# https://www.nrscotland.gov.uk/publications/vital-events-reference-tables-2023/

# Note that data only available at Scotland level (and there is no confidence intervals) and goes back as far as 1971
# Would be better to source Board level data from 2002 onwards instead however PHS SBAND dashboard
# only includes scotland level data https://scotland.shinyapps.io/phs-pregnancy-births-neonatal/ so unlikely to happen?

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Packages/Filepaths/Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
source("functions/main_analysis.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in and clean data  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean data
perinatal_data <- read_xlsx(file.path(profiles_data_folder,"Received Data", "Perinatal mortality", "Perinatal-Data.xlsx"), sheet = "4.02", skip = 4) %>%
  setNames(tolower(names(.))) %>% #variables to lower case 
  rename(year = 1) %>% # rename first column to year
  select(year, "perinatal deaths") %>% # select relevant columns
  rename(rate = 2) %>%
  filter(row_number() >= which(year == 1971)[1]) %>%  #get rid of everything before the first year of relevant data
  filter(row_number() <= which(year == 2023)[1]) %>%# get rid of everything beneath the most recent year of data (there are more tables underneath)
  mutate(rate = gsub("\\(.*?\\)", "", rate) %>% #get rid of the bracketed value which is irrelevant
           trimws()) |>
  mutate(across(everything(), ~ as.numeric(.))) # convert columns to class numeric


# add required columns 
perinatal_data <- perinatal_data %>%
  mutate(ind_id = "99139",
         trend_axis = year, 
         def_period = paste0(year, " calendar year"),
         lowci = NA, 
         upci = NA,
         numerator = NA,
         code = "S00000001") %>%
  # Select relevant columns
  select(ind_id, code, year, trend_axis, def_period, numerator, rate, lowci, upci) 


# restrict data to year 2002 onwards - to be consistent with rest of ScotPHO tool - not sure if earlier data is required.
perinatal_data <- perinatal_data %>%
  filter(year>=2002)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save final files -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save files in folder to be checked
write.csv(perinatal_data, file.path(profiles_data_folder, "Data to be checked/99139_perinatal_mortality_shiny.csv"), row.names = FALSE)
write_rds(perinatal_data, file.path(profiles_data_folder, "Data to be checked/99139_perinatal_mortality_shiny.rds"))

# QA the data (note there are no CIs so it will flag an error about them)
run_qa("99139_perinatal_mortality", type = "main", test_file = FALSE)

#END