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
#   Part 4 - Attempted murder and serious assault (4111)
#   Part 5 - Common Assault (4154)
#   Part 6 - Vandalism (4155)
#   Part 7 - Breach of the Peace (4156)
#   Part 8 - Driving under the Influence (4158)
#   Part 9 - Violent Crime (20805)
#   Part 10 - Drug Crimes (20806)

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #main analysis function
source("./functions/deprivation_analysis.R") #deprivation analysis function

library(lubridate) #for converting strings to date format
library(phsmethods) #for extracting financial year from calendar year
library(readxl) #for reading in xlsx filetype
library(stringr) #for handling strings

filepath <- paste0(profiles_data_folder, "/Received Data/Crime data/data/") #general crime data folder

#Temporary code - remove after all crime indicators developed
crime_dz_code <- readRDS(file.path(profiles_data_folder, "/Received Data/Crime data/crime_dz_code.rds"))

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
recorded_crime <- combine_files(file = list.files(path = filepath, pattern = ".xlsx", full.names = T))

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
dz_lookup <- read_excel(paste0(profiles_data_folder, "/Received Data/Crime data/dz_lookup.xlsx")) #rename to join on "datazone

#read in second lookup matching division names from FOI to LA names. This is to help deal with duplicate dz names e.g. multiple divisions have dz "City Centre - 01"
la_div_lookup <- read_excel(paste0(profiles_data_folder, "/Received Data/Crime data/police_division_la_lookup.xlsx"))

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
saveRDS(rec_crime_final, file=paste0(profiles_data_folder, '/Prepared Data/recorded_crime_raw.rds'))
saveRDS(rec_crime_final, file=paste0(profiles_data_folder, '/Prepared Data/recorded_crime_depr_raw.rds'))

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
  
  function_data <- readRDS(paste0 (profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
  
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
  
  if(length(crime_categories)>1){ #if more than one category specified, aggregate these
    data <- data |> 
    group_by(datazone, year) |> 
      summarise(numerator = sum(numerator)) |> 
      ungroup() #|> 
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
saveRDS(am_sa, file=paste0(profiles_data_folder, '/Prepared Data/attempted_murder_raw.rds'))

#Run analysis functions
main_analysis(filename = "attempted_murder", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4111, time_agg = 2, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

#Exclude unmatchable intermediate zones
remove_dz("attempted_murder")

###############################################.
## Part 6 - Breach of the Peace (4156)  ----
###############################################.

#Filter data on relevant crime bulletin categories
botp <- crime_breakdown(crime_dz_code, c("Breach of the Peace", "Threatening and abusive behaviour", "Stalking")) 

#Save prepared data for analysis functions
saveRDS(botp, file=paste0(profiles_data_folder, '/Prepared Data/breach_of_the_peace_raw.rds'))
saveRDS(botp, file=paste0(profiles_data_folder, '/Prepared Data/breach_of_the_peace_depr_raw.rds'))

#Run analysis functions
main_analysis(filename = "breach_of_the_peace", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4156, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

deprivation_analysis(filename = "breach_of_the_peace", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 4156)


#Exclude unmatchable intermediate zones
remove_dz("breach_of_the_peace")

###############################################.
## Part 7 - Common Assault (4154)  ----
###############################################.

#Note: crime categories selected will need revisiting for 2024/25 data
#as common assault is being broken down to include with/without injury
#and include more info about victim e.g. retail staff

#Filter data on relevant crime bulletin categories
#Note - minor assault and common assault seem to be used interchangeably in Scotland
ca <- crime_breakdown(crime_dz_code, c("Minor Assault", "Minor Assault (of an emergency worker")) 

#Save prepared data for analysis functions
saveRDS(ca, file=paste0(profiles_data_folder, '/Prepared Data/common_assault_raw.rds'))
saveRDS(ca, file=paste0(profiles_data_folder, '/Prepared Data/common_assault_depr_raw.rds'))

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
## Part 8 - Drug Crimes (20806) ----
###############################################.

#Bringing drugs into prison
#Other drugs offences (incl. importation)
#Possession of drugs
#Production, manufacture or cultivation of drugs
#Supply of drugs (incl. possession with intent)

#Filter data on relevant crime bulletin categories
drugs <- crime_breakdown(crime_dz_code, c("Bringing drugs into prison", "Other drugs offences (incl. importation)",
                                          "Possession of drugs", "Production, manufacture or cultivation of drugs", 
                                          "Supply of drugs (incl. possession with intent)"))

#Save prepared data for analysis functions
saveRDS(drugs, file=paste0(profiles_data_folder, '/Prepared Data/drug_crimes_raw.rds'))
saveRDS(drugs, file=paste0(profiles_data_folder, '/Prepared Data/drug_crimes_depr_raw.rds'))

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
## Part 9 - Vandalism (4155) ----
###############################################.

#Filter data on relevant crime bulletin categories
vandalism <- crime_breakdown(crime_dz_code, c("Vandalism (incl. reckless damage, etc.)")) 

#Save prepared data for analysis functions
saveRDS(vandalism, file=paste0(profiles_data_folder, '/Prepared Data/vandalism_raw.rds'))
saveRDS(vandalism, file=paste0(profiles_data_folder, '/Prepared Data/vandalism_depr_raw.rds'))

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
## Part 10 - Violent Crime (20805) ----
###############################################.

#All group 1 crimes - but maybe rename indicator to non-sexual violent crime

###############################################.
## Part 11 - Driving under the Influence (4158) ----
###############################################.

#No direct replacement - would need to use "Drink, Drug driving offences incl. Failure to provide a specimen
#May need renaming as well 
#Filter data on relevant crime bulletin categories
ddo <- crime_breakdown(crime_dz_code, c("Drink, Drug driving offences incl. Failure to provide a specimen")) 

#Save prepared data for analysis functions
saveRDS(ddo, file=paste0(profiles_data_folder, '/Prepared Data/drink_drug_driving_raw.rds'))

#Run analysis functions
main_analysis(filename = "drink_drug_driving", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 4158, time_agg = 2, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)


#Exclude unmatchable intermediate zones
remove_dz("drink_drug_driving")

############################################.
##End.



#Code template
#Filter data on relevant crime bulletin categories
xxxx <- crime_breakdown(crime_dz_code, c("Crime 1", "Crime 2")) 

#Save prepared data for analysis functions
saveRDS(xxxxxx, file=paste0(profiles_data_folder, '/Prepared Data/xxxxxx_raw.rds'))
saveRDS(xxxxxx, file=paste0(profiles_data_folder, '/Prepared Data/xxxxxx_depr_raw.rds'))

#Run analysis functions
main_analysis(filename = "xxxxxxxxx", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 9999, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_allages", crude_rate = 10000)

deprivation_analysis(filename = "xxxxxxxxxx", yearstart = 2014, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     crude_rate = 10000, ind_id = 9999)


#Exclude unmatchable intermediate zones
remove_dz("xxxxxxxxxxx")