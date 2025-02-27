# ~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~

#Indicator: Disability Pay Gap (Indicator ID 99138)
# Description: Percentage point gap in median hourly pay between disabled and non-disabled employees

# Data source is Labour Market Statistics for Scotland by Disability: January to December 2022 supporting tables:
#(specifically Table_15) 
#https://www.gov.scot/publications/labour-market-statistics-for-scotland-by-disability-january-to-december-2022/documents/

# Note that data only up to 2019. The accuracy of income weights are affected by an issue with the coding of 
# occupations identified by ONS. ONS have advised earnings information should be used with caution for 
# later years therefore SG publication excludes more recent years data until the issue is resolved.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages/Filepaths/Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(readxl)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read and clean data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in downloaded data
disability_data <- read_xlsx(file.path(profiles_data_folder, "Received Data", "Disability pay gap/Labour+Market+Statistics+for+Scotland+by+Disability+Tables=.xlsx"), sheet = "Table_15", skip = 6) %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  rename(rate = "disability pay gap [note 25]")


# clean data 
disability_data <- disability_data %>%
  
  # select relevant columns
  select(year, rate) %>%
  
  #remove unnecessary rows
  filter(!grepl("Change", year)) %>%
  
  #remove [r] from year names
  mutate(year = gsub("\\[r\\]", "", year)) %>%

  mutate(ind_id = "99138",
         rate = rate*100,
         #trend axis is field which populates the year axis in charts (needs to be short simple description of the time period that will display nicely in a chart)
         trend_axis = year, 
         #def_period is field that appears in indicator meta data which can be a more expansive description of the type of year (calendar/financial) and, if applicable, details about rolling averages    
         def_period = paste0(year, " survey year"),
         lowci = NA, 
         upci = NA,
         numerator = NA,
         code = "S00000001" ) %>% #assign the scotland code as theres no LA breakdown
  
  # Select relevant columns
  select(ind_id, code, year, trend_axis, def_period, numerator, rate, lowci, upci) 
  

# ~~~~~~~~~~~~~~~~~~~~~~
# save final files ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# Save files in folder to be checked
write.csv(disability_data, file.path(profiles_data_folder, "Data to be checked/99138_disability_paygap_shiny.csv"), row.names = FALSE)
write_rds(disability_data, file.path(profiles_data_folder, "Data to be checked/99138_disability_paygap_shiny.rds"))

# insert call to QA report
run_qa(filename ="99138_disability_paygap")


#END