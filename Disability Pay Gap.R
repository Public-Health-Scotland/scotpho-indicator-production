# ~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~

#Indicator: Disability Pay Gap (Indicator ID 99138)
# Description: Percentage point gap in median hourly pay between disabled and non-disabled employees

# Data sourced from 'Raw disability pay gaps UK' ONS tables (created using Annual Population Survey):
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/disability/datasets/rawpaygapsbydisability

# Note APS based labour market statistics are now classified as 'Official Statistics under development'
# due to ongoing challenges with response rates, levels, and weighting approach mean
# See 'Note' tab of the excel table. 
# Estimates used here are considered robust for full time series (check statistical robustness column of Table 5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages/Filepaths/Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(readxl)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read and clean data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in downloaded data
disability_data <- read_xlsx(file.path(profiles_data_folder, "Received Data/Disability pay gap", "dpgrawpaygaps2024.xlsx"), sheet = "Table 5", skip = 5)



# clean data 
disability_data <- disability_data %>%
  
  # clean column names
  clean_names() %>%

  # filter on relevant rows
  filter(disability_status == "Disabled employees" & country_and_english_region == "Scotland") %>%
  
  # select and rename columns
  select(
    year, 
    rate = pay_gap_percent,
    upci = pay_gap_ucl_percent,
    lowci = pay_gap_lcl_percent
    ) %>%
  
  # convert rate, upci and lowci columns to class numeric
  mutate(across(c("rate", "upci", "lowci"), ~ as.numeric(.))) %>%
  

  mutate(ind_id = 99138,
         #trend axis is field which populates the year axis in charts (needs to be short simple description of the time period that will display nicely in a chart)
         trend_axis = as.character(year), 
         #def_period is field that appears in indicator meta data which can be a more expansive description of the type of year (calendar/financial) and, if applicable, details about rolling averages    
         def_period = paste0(year, " survey year"),
         numerator = as.numeric(NA),
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
run_qa(filename = "99138_disability_paygap", type = "main") 


#END