###############################################.
## Analyst notes ----
###############################################. 

# ScotPHO Indicators: Recorded Crime rate

# Each year Improvement Service (IS) request recorded crime data via an FOI, who then send us a link to the data.
# The IS CPOP tool has a recorded crime indicator and we follow their methodology in terms of definition etc.
# Note - add link to FOI page on Police Scotland website if the indicator continues to be updated this way.


# The data provided so far has only included datazone names, rather than codes
# There are some datazones that share the same name but are different e.g. different DZ code and map to different parent geographies
# In some cases, we can work out the correct DZ code using the Police Division name (provided in extract).
# However, there are a small number of datazones that share the same name and belong to the same Police Division but different councils.
# These datazones have been included in the Scotland total but the Intermediate zones that they may belong to
# have been excluded since we cannot identify where exactly they should sit. Note this only affects a small number
# of crimes and there's only 4 IZs being excluded in total.

#   Part 1 - Prepare basefile
#   Part 2 - Recorded crime (21108)
#   Part 3 - Create crime breakdown function
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
source("./functions/main_analysis.R") #main analysis function
source("./functions/deprivation_analysis.R") #deprivation analysis function

library(lubridate) #for converting strings to date format
library(phsmethods) #for extracting financial year from calendar year
library(readxl) #for reading in xlsx filetype
library(stringr) #for handling strings

filepath <- file.path(profiles_data_folder, "/Received Data/Crime data/") #general crime data folder


##############################################.
## Part 1 - Prepare Basefile
###############################################.
#1. Read in files for all years

#Function to identify and import files
combine_files <- function(files) {
  combined <- data.table::rbindlist(lapply(files, function(x) {
    data <- read_xlsx(x, sheet = 2) 
  }))
  return(combined)
}

#Running the function - may take a few minutes. All files must be closed or it won't work. 
recorded_crime <- combine_files(file = list.files(path = file.path(filepath, "/data/"), pattern = ".xlsx", full.names = T))

rec_crime <- recorded_crime |> 
  janitor::clean_names() |> #simplify col names
  select(c(1:3, 5:7)) |> #keeping only number of crimes, year, month number, datazone name, division name, crime bulletin category
  mutate(rec_date = lubridate::my(paste(month_number, year_2)), #convert month and year columns to date format
         fin_year = phsmethods::extract_fin_year(rec_date)) #extract financial year from date

#2. Aggregate files by datazone name, financial year and division name
rec_crime_agg <- rec_crime |> 
  group_by(datazone, fin_year, division_name, crime_bulletin_category) |>  #aggregate the months to get whole year totals by dz. Inc. division name as some duplicate dz names e.g. City Centre 01
  summarise(numerator = sum(number_of_crimes)) |> 
  rename(year = fin_year) |>  #rename columns for analysis functions
  mutate(year = substr(year, 1, 4)) |> #abbreviate financial year to first calendar year
  mutate(year = as.numeric(year),
         datazone = na_if(datazone, "NULL")) |> #Converts "NULL" datazones to actual NAs
  ungroup()

#Filter 2007 data from incomplete FY 2006/07 and incomplete most recent year
max_year <- max(rec_crime_agg$year)

rec_crime_filtered <- rec_crime_agg |> 
  filter(year != 2006 & year != max_year)


#A small number of intermediate zones must be excluded from the data because they contain
#datazones that share a name in common with another datazone in both the council area and police division

#For example, there are two sets of "Western Edge" datazones in Dundee City/Tayside that cannot be distinguished
#And two sets of Hillhead datazones in Glasgow City/Greater Glasgow division 
rec_crime_filtered <- rec_crime_filtered |> 
mutate(datazone = case_when((stringr::str_detect(datazone, pattern = "Hillhead") & division_name != "Ayrshire") ~ "NULL", #converting 2 datazones that can't be matched 
                            stringr::str_detect(datazone, pattern = "Western Edge") ~ "NULL",
                            TRUE ~ datazone))

#3. Use lookups to get datazone s-codes codes, then aggregate to intermediate zone level

#Read in datazone lookups, join and tidy
dz_lookup <- read_excel(file.path(filepath, "dz_lookup.xlsx")) #rename to join on "datazone

#read in second lookup matching division names from FOI to LA names. This is to help deal with duplicate dz names e.g. multiple divisions have dz "City Centre - 01"
la_div_lookup <- read_excel(file.path(filepath, "police_division_la_lookup.xlsx"))

#Join both lookups
lookup <- left_join(la_div_lookup, dz_lookup) |> 
  rename(datazone = DZ2011_Name)

#Join crime data to lookup
crime_dz_code <- left_join(rec_crime_filtered, lookup, by = c("datazone", "division_name")) |>
  select(c(2,4:5, 7)) |>  #select only year, numerator, dz code and crime cat
  rename(datazone = DZ2011_Code) |> #change name for analysis function
  select(datazone, everything()) #move DZ to first col


###############################################.
## Part 2 - Recorded Crime (21108)  ----
###############################################.

#Aggregate across all crime categories to get total crime rate for each DZ
rec_crime_final <- crime_dz_code |> 
  group_by(datazone, year) |> 
  summarise(numerator = sum(numerator)) |> 
  ungroup()

#Save prepared data for analysis functions
saveRDS(rec_crime_final, file=file.path(profiles_data_folder, '/Prepared Data/recorded_crime_raw.rds'))

#Run analysis functions
main_analysis(filename = "recorded_crime", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 21108, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000, QA = FALSE)

deprivation_analysis(filename = "recorded_crime", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 21108)


#Create function to read in final file produced by analysis function, remove datazones that couldn't be matched
#then export final file

remove_dz <- function(filename){
  
  function_data <- readRDS(paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
  
  final_data <- function_data |> 
    filter(!(code %in% c("S02001528","S02001953","S02002233","S02001475")))
  
  saveRDS(final_data, file=paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
  write.csv(final_data, file=paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.csv"), row.names = FALSE)
  
}

remove_dz("recorded_crime")

###############################################.
## Part 3 - Create Automated Crime Breakdown Function  ----
###############################################.

#Arguments required:
#data -this should just be "crime_dz_code" if using prepared data from Part 1.
#crime_categories - a vectorised list of crimes to be filtered on. If more than one category is listed, these
#are aggregated. E.g. common assault includes both common assault and common assault (of an emergency worker)

crime_breakdown <- function(data, crime_categories){ 
  data <- data |> 
    filter(crime_bulletin_category %in% crime_categories) #filter df to only include specified cats
  
  crimes_not_found <- setdiff(crime_categories, unique(data$crime_bulletin_category)) #identify crimes specified in function argument that couldn't be found in data
  
  #If crimes could not be found in the data, list which ones. Otherwise do not produce a pop-up
  if(length(crimes_not_found) > 0){
    cli::cli_alert_info("The following crimes could not be found in the data:")
    cli::cli_ul(crimes_not_found)
  }

  if(length(crime_categories)>1){ #if more than one category specified, aggregate these
    data <- data |> 
    group_by(datazone, year) |> 
      summarise(numerator = sum(numerator), .groups = "drop") #|> 
      #select(-crime_bulletin_category) #then drop crime bulletin leaving only dz, year, numerator
  }else{
    data <- data |> 
      select(-crime_bulletin_category) #drop crime bulletin
  } #close else statement
  
  return(data) #produces the data as an output after the ifelse operation
  
 
} #close function

###############################################.
## Part 4 - Attempted murder and serious assault (4111)  ----
###############################################.

#Filter data on relevant crime bulletin categories
am_sa <- crime_breakdown(crime_dz_code, c("Attempted Murder", "Serious Assault (incl. culpable & reckless conduct - causing injury)"))

#Save prepared data for analysis functions
saveRDS(am_sa, file=file.path(profiles_data_folder, '/Prepared Data/attempted_murder_raw.rds'))

#Run analysis functions
main_analysis(filename = "attempted_murder", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4111, time_agg = 2, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

#Exclude unmatchable intermediate zones
remove_dz("attempted_murder")

###############################################.
## Part 5 - Threatening and Abusive Behaviour (4156)  ----
###############################################.
#This indicator is a renamed version of the indicator Breach of the Peace
#It contains BOTP and Threatening and Abusive Behaviour (TAB)
#BOTP refers to TAB which takes place in a public setting and therefore 
#constitutes a minority of offences

#Filter data on relevant crime bulletin categories
tab <- crime_breakdown(crime_dz_code, c("Breach of the Peace", "Threatening and abusive behaviour"))

#Save prepared data for analysis functions
saveRDS(tab, file=file.path(profiles_data_folder, '/Prepared Data/threatening_and_abusive_behaviour_raw.rds'))

#Run analysis functions
main_analysis(filename = "threatening_and_abusive_behaviour", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4156, time_agg = 1, yearstart = 2010, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

deprivation_analysis(filename = "threatening_and_abusive_behaviour", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 4156)


#Exclude unmatchable intermediate zones
remove_dz("threatening_and_abusive_behaviour")

###############################################.
## Part 6 - Common Assault (4154)  ----
###############################################.

#Note: crime categories selected will need revisiting for 2024/25 data
#as common assault is being broken down to include with/without injury
#and include more info about victim e.g. retail staff

#Filter data on relevant crime bulletin categories
#Note - minor assault and common assault seem to be used interchangeably in Scotland
ca <- crime_breakdown(crime_dz_code, c("Minor Assault", "Minor Assault (of an emergency worker)")) 

#Save prepared data for analysis functions
saveRDS(ca, file=file.path(profiles_data_folder, '/Prepared Data/common_assault_raw.rds'))

#Run analysis functions
main_analysis(filename = "common_assault", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4154, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

deprivation_analysis(filename = "common_assault", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 4154)


#Exclude unmatchable intermediate zones
remove_dz("common_assault")

###############################################.
## Part 7 - Drug Crimes (20806) ----
###############################################.

#Filter data on relevant crime bulletin categories
drugs <- crime_breakdown(crime_dz_code, c("Bringing drugs into prison", "Other drugs offences (incl. importation)",
                                          "Possession of drugs", "Production, manufacture or cultivation of drugs", 
                                          "Supply of drugs (incl. possession with intent)"))

#Save prepared data for analysis functions
saveRDS(drugs, file=file.path(profiles_data_folder, '/Prepared Data/drug_crimes_raw.rds'))

#Run analysis functions
main_analysis(filename = "drug_crimes", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 20806, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

deprivation_analysis(filename = "drug_crimes", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 20806)


#Exclude unmatchable intermediate zones
remove_dz("drug_crimes")

###############################################.
## Part 8 - Vandalism (4155) ----
###############################################.

#Filter data on relevant crime bulletin categories
vandalism <- crime_breakdown(crime_dz_code, c("Vandalism (incl. reckless damage, etc.)")) 

#Save prepared data for analysis functions
saveRDS(vandalism, file=file.path(profiles_data_folder, '/Prepared Data/vandalism_raw.rds'))

#Run analysis functions
main_analysis(filename = "vandalism", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4155, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

deprivation_analysis(filename = "vandalism", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 4155)


#Exclude unmatchable intermediate zones
remove_dz("vandalism")

###############################################.
## Part 9 - Non-sexual Crimes of Violence (20805) ----
###############################################.

#A list of non-violent crimes published by the SG as part of the Recorded Crime publication was used to manually 
# match to the FOI data. The list can be found in Sheet 4 of the document below:
# https://www.gov.scot/publications/recorded-crime-scotland-classification-crimes-offences/
# Each year the list of crimes included should be compared against the FOI data to ensure crimes are being recorded 
# under the same names as they have been historically. 

#If any crimes specified within vc_list cannot be found in the data, they will be listed in the console. 
#These can then be checked against the FOI data. 


vc_list <- c("Attempted Murder", "Minor Assault", "Minor Assault (of an emergency worker)", 
             "Murder", "Other Group 1 crimes", "Reckless conduct (with firearms)",
             "Robbery and assault with intent to rob", "Serious Assault (incl. culpable & reckless conduct - causing injury)",
             "Threats and extortion", "Domestic Abuse (of female)", "Domestic Abuse (of male)")

#Filter data on relevant crime bulletin categories
vc <- crime_breakdown(crime_dz_code, vc_list)

#Save prepared data for analysis functions
saveRDS(vc, file=file.path(profiles_data_folder, '/Prepared Data/violent_crime_raw.rds'))

#Run analysis functions
main_analysis(filename = "violent_crime", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 20805, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

deprivation_analysis(filename = "violent_crime", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 20805)

#Exclude unmatchable intermediate zones
remove_dz("violent_crime")


###############################################.
## Part 10 - Driving under the Influence (4158) ----
###############################################.

#No direct replacement - would need to use "Drink, Drug driving offences incl. Failure to provide a specimen
#May need renaming as well 
#Filter data on relevant crime bulletin categories
ddo <- crime_breakdown(crime_dz_code, c("Drink, Drug driving offences incl. Failure to provide a specimen")) 

#Save prepared data for analysis functions
saveRDS(ddo, file=file.path(profiles_data_folder, '/Prepared Data/drink_drug_driving_raw.rds'))

#Run analysis functions
main_analysis(filename = "drink_drug_driving", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4158, time_agg = 2, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)


#Exclude unmatchable intermediate zones
remove_dz("drink_drug_driving")

############################################.
##End.