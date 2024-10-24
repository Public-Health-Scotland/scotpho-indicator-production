##  ScotPHO Profiles Tool: Indicator update script.
##  Indicator: Climate change action (Inidicator ID 99136) ---- 

# Description: Percentage of adults agreeing with the statement 
# 'I understand what actions people like myself should take to help tackle climate change

# New indicator initially created for the Care and wellbeing Portfolio profile Oct 2024

# Data source is Scottish Household Survey (SHS) Data:
# 2022 SHS report document downloads - see "SHS 2022 - Annual Report - Tables - 7 Environment" (specifically Table 7.41) 
#https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2023/12/scottish-household-survey-2022-key-findings/documents/shs-2022-annual-report-tables-7-environment/shs-2022-annual-report-tables-7-environment/govscot%3Adocument/SHS%2B2022%2B-%2BAnnual%2BReport%2B-%2BTables%2B-%2B7%2BEnvironment.ods



###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R") 



###############################################.
## Read in data  ----
###############################################.


climate_data <- read_xlsx(paste0(data_folder, "Received Data/Scottish Household Survey/SHS 2022 Report_Table7_environment.xlsx"), sheet = "7_41", skip = 1) %>%
  #tidy areanames so that they will match the recognised descriptions as used by ScotPHO lookups and enable matching on of standard geography codes
  mutate(areaname = str_replace_all(council, c("Edinburgh, City of" = "City of Edinburgh"))) %>%
  setNames(tolower(names(.)))  #variables to lower case

                                   
# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>%
  filter(areatype %in% c("Scotland", "Council area")) %>% 
  select(c(code, areaname))

# Join area codes by area names
climate_data <- climate_data %>%
left_join(dictionary, by = "areaname")

# CHECK ALL GEOGRAPHIES HAVE FOUND A MATCH



climate_data <- climate_data %>%
  
  # filter response where strongly agree
  filter(answer == "Strongly agree") %>%
  
  # format & transpose data to long format 
  mutate(across(c("2015", "2017", "2018","2022"), as.numeric)) %>%
  select(!council) %>%
  pivot_longer(cols = c("2015", "2017", "2018", "2022"),
               names_to = "year",
               values_to = "rate") %>%
  # excel file contains numbers formatted as percentages which need to be converted to percentage values once formatting lost
  mutate(rate=rate*100,
         ind_id = 99136,
         #trend axis is field which populates the year axis in charts (needs to be short simple description of the time period that will display nicely in a chart)
         trend_axis = year, 
         #def_period is field that appears in indicator meta data which can be a more expansive decription of the type of year (calendar/financial) and, if applicable, details about rolling averages    
         def_period = paste0(year, " calendar year"),
         lowci = NA, 
         upci = NA,
         numerator = NA) %>%
  
  # Select relevant columns
  select(ind_id, code, year, trend_axis, def_period, numerator, rate, lowci, upci)



### 3. Prepare final files -----

# Save files in folder to be checked
write.csv(climate_data, paste0(data_folder, "Data to be checked/climate_action_shiny.csv"), row.names = FALSE)
write_rds(climate_data, paste0(data_folder, "Data to be checked/climate_action_shiny.rds"))



#END







