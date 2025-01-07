##  ScotPHO Profiles Tool: Indicator update script.
##  Indicator: Climate change is immediate & urgent problem (Inidicator ID 99137) ---- 

# Description: Percentage of adults agreeing with the statement 'Climate change is an immediate and urgent problem'

# New indicator initially created for the Care and wellbeing Portfolio profile Oct 2024

# Data source is Scottish Household Survey (SHS) Data:
# 2022 SHS report document downloads - see "SHS 2022 - Annual Report - Tables - 7 Environment" (specifically Table 7.1 to 7_10) 
#https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2023/12/scottish-household-survey-2022-key-findings/documents/shs-2022-annual-report-tables-7-environment/shs-2022-annual-report-tables-7-environment/govscot%3Adocument/SHS%2B2022%2B-%2BAnnual%2BReport%2B-%2BTables%2B-%2B7%2BEnvironment.ods


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R") 

ind_number <- 99137 # source from techdoc


###############################################.
## Functions ----
###############################################.

read_in_tabs <- function(tab_number, rows_skipped){
  
  tab_data <- read_xlsx(paste0(data_folder, "Received Data/Scottish Household Survey/SHS 2022 Report_Table7_environment.xlsx"), sheet = tab_number, skip = rows_skipped) %>%
    setNames(tolower(names(.))) |>  #variables to lower case
    #tidy areanames so that they will match the recognised descriptions as used by ScotPHO lookups and enable matching on of standard geography codes
    # hidden characters in some of the descriptions causing issues with matching on to lookup
    mutate(areaname= case_when(council=="Edinburgh, City of" ~ "City of Edinburgh",
                               council=="Dumfries &\r\nGalloway" ~ "Dumfries & Galloway", 
                               council=="West\r\nDunbartonshire" ~ "West Dunbartonshire", 
                               TRUE ~ str_trim(council)),
           answer= case_when(answer=="Climate change is an immediate and urgent\r\nproblem" ~ "Climate change is an immediate and urgent problem",
                             TRUE ~ answer))

  #join with area codes by area names 
  tab_data <- tab_data %>%
    left_join(dictionary, by = "areaname")
}


output_processing <- function(data){
  data <- data %>%
    
    # filter response where response is Climate change is an immediate and urgent problem
    filter(answer == "Climate change is an immediate and urgent problem") %>%
    
    # format & transpose data to long format 
    mutate(across(c("2013","2014","2015","2016", "2017", "2018","2019","2022"), as.numeric)) %>%
    select(!council) %>%
    pivot_longer(cols = c("2013","2014","2015","2016", "2017", "2018","2019","2022"),
                 names_to = "year",
                 values_to = "rate") %>%
    # excel file contains numbers formatted as percentages which need to be converted to percentage values once formatting lost
    mutate(rate=rate*100,
           ind_id = ind_number, 
           #trend axis is field which populates the year axis in charts (needs to be short simple description of the time period that will display nicely in a chart)
           trend_axis = year, 
           #def_period is field that appears in indicator meta data which can be a more expansive description of the type of year (calendar/financial) and, if applicable, details about rolling averages    
           def_period = paste0(year, " calendar year"),
           lowci = NA, 
           upci = NA,
           numerator = NA) %>%
    
    # Select relevant columns
    select(ind_id, code, year, trend_axis, def_period, numerator, rate, lowci, upci, answer) 
}


# Reponses rate for different population groups very incomplete - currently not producing outputs for use in profile
# population_output_processing <- function(data,category){
#   data <- data %>%
#     
#     # format & transpose data to long format 
#     mutate(across(c("2013","2014","2015","2016", "2017", "2018","2019","2022"), as.numeric)) %>%
#     select(!council) %>%
#     pivot_longer(cols = c("2013","2014","2015","2016", "2017", "2018","2019","2022"),
#                  names_to = "year",
#                  values_to = "rate") %>%
#     # excel file contains numbers formatted as percentages which need to be converted to percentage values once formatting lost
#     mutate(rate=rate*100,
#            ind_id = ind_number, 
#            #trend axis is field which populates the year axis in charts (needs to be short simple description of the time period that will display nicely in a chart)
#            trend_axis = year, 
#            #def_period is field that appears in indicator meta data which can be a more expansive description of the type of year (calendar/financial) and, if applicable, details about rolling averages    
#            def_period = paste0(year, " calendar year"),
#            lowci = NA, 
#            upci = NA,
#            numerator = NA,
#            split_name = category) %>%
#     rename(split_value = answer) %>%
#     select(!areaname) %>%
#     #remove any rows which contain the word base as they aren't relevant 
#     filter(!str_detect(split_value, "Base"))
# }

#Insufficient reponse rate
# SIMD_output_processing <- function(data){
#   data <- data %>%
#     
#     # format & transpose data to long format 
#     mutate(across(c("2013","2014","2015","2016", "2017", "2018","2019","2022"), as.numeric)) %>%
#     select(!council) %>%
#     pivot_longer(cols = c("2013","2014","2015","2016", "2017", "2018","2019","2022"),
#                  names_to = "year",
#                  values_to = "rate") %>%
#     # excel file contains numbers formatted as percentages which need to be converted to percentage values once formatting lost
#     mutate(rate=rate*100,
#            ind_id = ind_number, 
#            #trend axis is field which populates the year axis in charts (needs to be short simple description of the time period that will display nicely in a chart)
#            trend_axis = year, 
#            #def_period is field that appears in indicator meta data which can be a more expansive description of the type of year (calendar/financial) and, if applicable, details about rolling averages    
#            def_period = paste0(year, " calendar year"),
#            lowci = NA, 
#            upci = NA,
#            numerator = NA,
#            quint_type = "la_quint",
#            quintile = str_extract(answer, "(?<=Quintile )\\d+")) %>%
#     select(!areaname) %>%
#     #remove any rows which contain the word base as they arent relevant 
#     filter(!str_detect(answer, "Base"))
# }


###############################################.
## Read in data  ----
###############################################.

# # Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>%
  filter(areatype %in% c("Scotland", "Council area")) %>%
  select(c(code, areaname))

#read in the climate data for the generic output
climate_data <- read_in_tabs(tab_number="7_1",rows_skipped =  1)

# Insufficient response rate to produce data splits by population group or SIMD split

#read in the climate data split by various population splits
# climate_data_education <- read_in_tabs("7_2", 1)
# climate_data_geog <- read_in_tabs("7_4", 2)
# climate_data_age <- read_in_tabs("7_5", 1)
# climate_data_sex <- read_in_tabs("7_6", 2)
# climate_data_health <- read_in_tabs("7_7", 1)
# climate_data_disability <- read_in_tabs("7_8", 2)
# climate_data_ethnicity <- read_in_tabs("7_9", 2)
# climate_data_religion <- read_in_tabs("7_10", 2)

#read in the climate data split by simd
#climate_data_simd <- read_in_tabs("7_3", 1)


###############################################.
## Process Data  ----
###############################################.


#process data for main output - this will filter to only specific question about climate change being a problem
climate_problem_main <- output_processing(data=climate_data)

# INSUFFICIENT RESPONSE RATE TO PRODUCE THESE SPLITS
#process data for population output
# climate_education_output <- population_output_processing(climate_data_education, "Educational attainment")
# climate_geog_output <- population_output_processing(climate_data_geog, "Urban rural classification")
# climate_age_output <- population_output_processing(climate_data_age, "Age")
# climate_sex_output <- population_output_processing(climate_data_sex, "Sex")
# climate_health_output <- population_output_processing(climate_data_health, "Self perception of health")
# climate_disability_output <- population_output_processing(climate_data_disability, "Disability")
# climate_ethnicity_output <- population_output_processing(climate_data_ethnicity, "Ethnicity")
# climate_religion_output <- population_output_processing(climate_data_religion, "Religious Belonging")
# #bind all the population outputs together
# climate_population_output <- rbind(climate_education_output,climate_geog_output,climate_age_output,climate_sex_output,
#                                    climate_health_output,climate_disability_output, climate_ethnicity_output, climate_religion_output)

# INSUFFICIENT RESPONSE RATE TO PRODUCE THIS SPLIT
#process data for simd output
#climate_simd_output <- SIMD_output_processing(climate_data_simd)


### 3. Prepare final files -----

# Save climate files in folder to be checked
write.csv(climate_problem_main, paste0(data_folder, "Data to be checked/climate_problem_shiny.csv"), row.names = FALSE)
write_rds(climate_problem_main, paste0(data_folder, "Data to be checked/climate_problem_shiny.rds"))


# Run QA reports and check the output files 
run_qa(filename = "climate_problem")







# # Save population files in folder to be checked
# write.csv(climate_population_output, paste0(data_folder, "Data to be checked/climate_action_shiny_population.csv"), row.names = FALSE)
# write_rds(climate_population_output, paste0(data_folder, "Data to be checked/climate_action_shiny_population.rds"))
# 
# # Save SIMD files in folder to be checked
# write.csv(climate_simd_output, paste0(data_folder, "Data to be checked/climate_action_shiny_simd.csv"), row.names = FALSE)
# write_rds(climate_simd_output, paste0(data_folder, "Data to be checked/climate_action_shiny_simd.rds"))



#END