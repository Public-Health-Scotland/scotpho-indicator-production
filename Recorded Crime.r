
#To do 
#decide what range to use for each indicator based on legislation, data changes etc
#run for 2025 data once the MYE estimates released on 14/07

# ScotPHO indicators: Recorded Crime

#Temporary script to restore CA-level figures while dz-level data is quality assured
#This script draws from the SG publication rather than Police Scotland FOI data

#   Part 1 - Prepare basefile
#   Part 2 - Create automated crime breakdown function
#   Part 3 - Recorded crime (21108)
#   Part 4 - Attempted Murder and Serious Assault (4111)
#   Part 5 - Threatening and Abusive Behaviour (4156)
#   Part 6 - Common Assault (4154)
#   Part 7 - Drug Crimes (20806)
#   Part 8 - Vandalism (4155)
#   Part 9 - Violent Crime (20805)
#   Part 10 - Driving under the Influence (4158)

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R")
source("./functions/deprivation_analysis.R")

source("./functions/data cleaning functions/ca_names_to_codes.R")
source("./functions/data cleaning functions/fix_fin_year.R")

library(readxl)

###############################################.
## Part 1 - Prepare basefile for all crime types ----
###############################################.

crime <- read.csv(file.path(profiles_data_folder, "Received Data/Crime data/SG/crime_data_all_years.csv")) |> 
  clean_names() |> #col names to snake case
  rename(ca2019 = feature_code, #rename to formats expected by analysis functions
        year = date_code,
        numerator = value) |> 
  filter(ca2019 != "S92000003", #remove Scotland figures - re-added in analysis functions
         measurement == "Count") |> #remove ratios
  select(-feature_name, -units, -feature_type, -measurement) |>  #drop unneeded cols
  fix_fin_year("year", "4") #take first year of financial year and change to numeric

###############################################.
## Part 2 - Create Automated Crime Breakdown Function  ----
###############################################.

#Arguments required:
#data -this should be the crime df created in part 1
#crime_categories - a vectorised list of crime_or_offence values to be filtered on. If more than one category is listed, these
#are aggregated. E.g. common assault includes both common assault and common assault (of an emergency worker)

crime_breakdown <- function(data, crime_categories){ 
  data <- data |> 
    filter(crime_or_offence %in% crime_categories)  #filter df to only include specified cats/types
  
  crimes_not_found <- setdiff(crime_categories, union(unique(data$crime_or_offence), unique(data$crime_or_offence))) #identify crimes specified in function argument that couldn't be found in data
  
  #If crimes could not be found in the data, list which ones. Otherwise do not produce a pop-up
  if(length(crimes_not_found) > 0){
    cli::cli_alert_info("The following crimes could not be found in the data:")
    cli::cli_ul(crimes_not_found)
  }
  
  if(length(crime_categories)>1){ #if more than one category specified, aggregate these
    data <- data |> 
      group_by(ca2019, year) |> 
      summarise(numerator = sum(numerator), .groups = "drop") 
    
    }else{
      data <- data |> 
      select(-crime_or_offence) #drop crime type if no aggregation needed. Done automatically during summarisation if more than 1 crime cat
  } #close else statement
  
  return(data) #produces the data as an output after the ifelse operation
  
  
} #close function


###############################################.
## Part 3 - Recorded Crime (21108)  ----
###############################################.

recorded_crime <- crime_breakdown(crime, c("All Crimes", "All Offences"))

#Save prepared data for analysis functions
saveRDS(recorded_crime, file.path(profiles_data_folder, '/Prepared Data/recorded_crime_raw.rds'))

#Run analysis function
main_analysis(filename = "recorded_crime", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 21108, time_agg = 1, yearstart = 2013, 
              yearend = 2025, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 4 - Attempted Murder and Serious Assault (4111)  ----
###############################################.

attempted_murder <- crime_breakdown(crime, c("Crimes: Group 1: Serious assault and attempted murder"))

#Save prepared data for analysis functions
saveRDS(attempted_murder, file.path(profiles_data_folder, '/Prepared Data/attempted_murder_raw.rds'))

#Run analysis function
main_analysis(filename = "attempted_murder", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4111, time_agg = 1, yearstart = 2002, 
              yearend = 2025, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 5 - Threatening and Abusive Behaviour (4156)  ----
###############################################.

tab <- crime_breakdown(crime, c("Offences: Group 6: Threatening and abusive behaviour"))

#Save prepared data for analysis functions
saveRDS(tab, file.path(profiles_data_folder, '/Prepared Data/threatening_and_abusive_behaviour_raw.rds'))

#Run analysis function
main_analysis(filename = "threatening_and_abusive_behaviour", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4156, time_agg = 1, yearstart = 2002, 
              yearend = 2025, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 6 - Common Assault (4154)  ----
###############################################.

#Note - minor assault and common assault seem to be used interchangeably in Scotland
common_assault <- crime_breakdown(crime, c("Crimes: Group 1: Common assault")) 

#Save prepared data for analysis functions
saveRDS(common_assault, file.path(profiles_data_folder, '/Prepared Data/common_assault_raw.rds'))

#Run analysis functions
main_analysis(filename = "common_assault", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4154, time_agg = 1, yearstart = 2002, 
              yearend = 2025, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 7 - Drug Crimes (20806) ----
###############################################.

drugs <- crime_breakdown(crime, c("Crimes: Group 5: Drugs - Supply", "Crimes: Group 5: Drugs - Possession"))

#Save prepared data for analysis functions
saveRDS(drugs, file.path(profiles_data_folder, '/Prepared Data/drug_crimes_raw.rds'))

#Run analysis functions
main_analysis(filename = "drug_crimes", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 20806, time_agg = 1, yearstart = 2002, 
              yearend = 2025, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 8 - Vandalism (4155) ----
###############################################.

vandalism <- crime_breakdown(crime, c("Crimes: Group 4: Vandalism")) 

#Save prepared data for analysis functions
saveRDS(vandalism, file.path(profiles_data_folder, '/Prepared Data/vandalism_raw.rds'))

#Run analysis functions
main_analysis(filename = "vandalism", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4155, time_agg = 1, yearstart = 2002, 
              yearend = 2025, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 9 - Non-sexual Crimes of Violence (20805) ----
###############################################.

#Filter data on relevant crime bulletin categories
violent_crime <- crime_breakdown(crime, c("All Group 1: Non-sexual crimes of violence")) 

#Save prepared data for analysis functions
saveRDS(violent_crime, file.path(profiles_data_folder, '/Prepared Data/violent_crime_raw.rds'))

#Run analysis functions
main_analysis(filename = "violent_crime", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 20805, time_agg = 1, yearstart = 2002, 
              yearend = 2025, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 10 - Driving under the Influence (4158) ----
###############################################.

ddo <- crime_breakdown(crime, c("Offences: Group 8: Driving under the influence")) 

#Save prepared data for analysis functions
saveRDS(ddo, file.path(profiles_data_folder, '/Prepared Data/drink_drug_driving_raw.rds'))

#Run analysis functions
main_analysis(filename = "drink_drug_driving", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4158, time_agg = 1, yearstart = 2002, 
              yearend = 2024, pop = "CA_pop_allages", crude_rate = 10000)


############################################.
##End.