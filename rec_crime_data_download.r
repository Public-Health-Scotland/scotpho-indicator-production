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

#Read in Tables 1 and 2 from publication and combine - number of offences and crimes at CA level.
filepath <- file.path(profiles_data_folder, "Received Data/Crime data/SG/rec_crime_2425.xlsx")

sheets_list <- list(
  crimes <- read_excel(filepath, sheet = "Table_1", skip = 2), #skip first two lines of metadata
  offences <- read_excel(filepath, sheet = "Table_2", skip = 2)
)

sheets_list <- lapply(sheets_list, function(df) {
  names(df)[c(2, 3, 6, 13)] <- c("crime_offence_group", "crime_offence_type", "2017-18", "2024-25") #renaming crime/offence cols so they can be combined vertically
  df <- df |> 
  mutate(across(4:13, as.character)) |>   #mutating all years to character as they're a mixture of strings and numeric
  slice(1:(n() - 2))  # remove last two rows from each df which contain metadata
  
  df
})

crime_offence <- bind_rows(sheets_list) |> 
  janitor::clean_names() |> #change names to snake case
  filter(local_authority != "Scotland") |>   #remove Scotland figs - to be re-added by analysis functions 
  select(-c(14:15)) |>  #drop percentage change cols 
  tidyr::pivot_longer(cols = c(4:13), names_to = "year", values_to = "numerator") |>  #pivot longer to only end up with one year type
  mutate(year = str_sub(year, start = 2)) |>   #cut off the x from the beginning of all the years
  fix_fin_year(fy_col_name = "year", first_year_digits = "4") |>  #converts string financial year to numeric first year
  filter(!numerator %in% c("n/l", "x")) |>  #removing rows for years where the offence was not in place yet
  mutate(numerator = as.numeric(numerator)) |> #convert string numerators to numeric
  filter(year >= 2010) |> #filtering out incomplete early data
  filter(year != 2024) |>  #filter out incomplete 24/25 data
  ca_names_to_codes(local_authority) #convert ca names to codes

###############################################.
## Part 2 - Create Automated Crime Breakdown Function  ----
###############################################.

#Arguments required:
#data -this should be the crime_offence df created in part 1
#crime_categories - a vectorised list of crime_offence_groups to be filtered on. If more than one category is listed, these
#are aggregated. E.g. common assault includes both common assault and common assault (of an emergency worker)

crime_breakdown <- function(data, crime_categories){ 
  data <- data |> 
    filter(crime_offence_group %in% crime_categories | crime_offence_type %in% crime_categories)  #filter df to only include specified cats/types
    
#Unless producing the recorded crime indicator, remove all rows where the type is "Total" to prevent duplication
 if(!any(c("Total Offences", "Total Crimes") %in% crime_categories)){
   data <- data |> 
   filter(crime_offence_type != "Total")
 }
  
  crimes_not_found <- setdiff(crime_categories, union(unique(data$crime_offence_group), unique(data$crime_offence_type))) #identify crimes specified in function argument that couldn't be found in data
  
  #If crimes could not be found in the data, list which ones. Otherwise do not produce a pop-up
  if(length(crimes_not_found) > 0){
    cli::cli_alert_info("The following crimes could not be found in the data:")
    cli::cli_ul(crimes_not_found)
  }
  
  if(length(crime_categories)>1){ #if more than one category specified, aggregate these
    data <- data |> 
      group_by(code, year) |> 
      summarise(numerator = sum(numerator), .groups = "drop") #|> 
    #select(-crime_bulletin_category) #then drop crime bulletin leaving only dz, year, numerator
    
  #If one crime category selected but multiple types present within, aggregate across these
  }else if (length(unique(data$crime_offence_type)) > 1){
    data <- data |> 
      group_by(code, year) |> 
      summarise(numerator = sum(numerator), .groups = "drop") 
    
    }else{
      data <- data |> 
      select(-c("crime_offence_group", "crime_offence_type")) #drop crime bulletin
  } #close else statement
  
  return(data) #produces the data as an output after the ifelse operation
  
  
} #close function


###############################################.
## Part 3 - Recorded Crime (21108)  ----
###############################################.

recorded_crime <- crime_breakdown(crime_offence, c("Total Crimes", "Total Offences"))

#Save prepared data for analysis functions
saveRDS(recorded_crime, file.path(profiles_data_folder, '/Prepared Data/recorded_crime_raw.rds'))

#Run analysis function
main_analysis(filename = "recorded_crime", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 21108, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 4 - Attempted Murder and Serious Assault (4111)  ----
###############################################.

attempted_murder <- crime_breakdown(crime_offence, c("Serious assault and attempted murder [note 2]"))

#Save prepared data for analysis functions
saveRDS(attempted_murder, file.path(profiles_data_folder, '/Prepared Data/attempted_murder_raw.rds'))

#Run analysis function
main_analysis(filename = "attempted_murder", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4111, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 5 - Threatening and Abusive Behaviour (4156)  ----
###############################################.

tab <- crime_breakdown(crime_offence, c("Threatening and abusive behaviour"))

#Save prepared data for analysis functions
saveRDS(tab, file.path(profiles_data_folder, '/Prepared Data/threatening_and_abusive_behaviour_raw.rds'))

#Run analysis function
main_analysis(filename = "threatening_and_abusive_behaviour", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4156, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 6 - Common Assault (4154)  ----
###############################################.

#Note - minor assault and common assault seem to be used interchangeably in Scotland
common_assault <- crime_breakdown(crime_offence, c("Common assault")) 

#Save prepared data for analysis functions
saveRDS(common_assault, file.path(profiles_data_folder, '/Prepared Data/common_assault_raw.rds'))

#Run analysis functions
main_analysis(filename = "common_assault", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4154, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 7 - Drug Crimes (20806) ----
###############################################.

drugs <- crime_breakdown(crime_offence, c("Drugs - Supply", "Drugs - Possession"))

#Save prepared data for analysis functions
saveRDS(drugs, file.path(profiles_data_folder, '/Prepared Data/drug_crimes_raw.rds'))

#Run analysis functions
main_analysis(filename = "drug_crimes", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 20806, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 8 - Vandalism (4155) ----
###############################################.

vandalism <- crime_breakdown(crime_offence, c("Vandalism")) 

#Save prepared data for analysis functions
saveRDS(vandalism, file.path(profiles_data_folder, '/Prepared Data/vandalism_raw.rds'))

#Run analysis functions
main_analysis(filename = "vandalism", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4155, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 9 - Non-sexual Crimes of Violence (20805) ----
###############################################.

#Filter data on relevant crime bulletin categories
violent_crime <- crime_breakdown(crime_offence, c("Non-sexual crimes of violence")) 

#Save prepared data for analysis functions
saveRDS(violent_crime, file.path(profiles_data_folder, '/Prepared Data/violent_crime_raw.rds'))

#Run analysis functions
main_analysis(filename = "violent_crime", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 20805, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


###############################################.
## Part 10 - Driving under the Influence (4158) ----
###############################################.

ddo <- crime_breakdown(crime_offence, c("Driving under the influence")) 

#Save prepared data for analysis functions
saveRDS(ddo, file.path(profiles_data_folder, '/Prepared Data/drink_drug_driving_raw.rds'))

#Run analysis functions
main_analysis(filename = "drink_drug_driving", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4158, time_agg = 1, yearstart = 2015, 
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)


############################################.
##End.