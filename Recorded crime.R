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
#   Part 4 - Domestic abuse (20804)
#   Part 5 - Attempted murder and serious assault (4111)
#   Part 6 - Breach of the Peace (4156)
#   Part 7 - Common Assault (4154)
#   Part 8 - Drug Crimes (20806)
#   Part 9 - Vandalism (4155)
#   Part 10 - Violent Crime (20805)
#   Part 11 - Driving under the Influence

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
              yearend = 2022, pop = "DZ11_pop_16to64", crude_rate = 10000, QA = F)

deprivation_analysis(filename = "recorded_crime", yearstart = 2007, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     pop_age = c(16, 64), crude_rate = 10000, ind_id = 21108)


#Create function to read in final file produced by analysis function, remove datazones that couldn't be matched
#then export final file

remove_dz <- function(filename){
  
  function_data <- readRDS(paste0 (profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
  
  final_data <- function_data |> 
    filter(!(code %in% c("S02001528","S02001953","S02002233","S02001475")))
  
  saveRDS(rec_crime_final, file=paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
  write.csv(rec_crime_final, file=paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.csv"), row.names = FALSE)
  
}

remove_dz(recorded_crime)

###############################################.
## Part 3 - Create Automated Crime Breakdown Function  ----
###############################################.

#data -this should just be crime_dz_code 
#crime_categories - a vectorised list of crimes to be filtered on. If more than one category is listed, these
#are aggregated. E.g. domestic abuse of male and female are aggregated into one domestic abuse indicator

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
## Part 4 - Domestic Abuse (20804)  ----
###############################################.

#Filter data
domestic_abuse <- crime_breakdown(crime_dz_code, c("Domestic Abuse (of female)", "Domestic Abuse (of male)"))

#Save prepared data for analysis functions
saveRDS(domestic_abuse, file=paste0(profiles_data_folder, '/Prepared Data/domestic_abuse_raw.rds'))
saveRDS(domestic_abuse, file=paste0(profiles_data_folder, '/Prepared Data/domestic_abuse_depr_raw.rds'))

#Run analysis functions
main_analysis(filename = "domestic_abuse", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 21108, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_16to64", crude_rate = 10000)

deprivation_analysis(filename = "domestic_abuse", yearstart = 2007, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     pop_age = c(16, 64), crude_rate = 10000, ind_id = 21108)

###############################################.
## Part 5 - Attempted murder and serious assault (4111)  ----
###############################################.

#Attempted Murder
#Serious Assault (incl. culpable & reckless conduct – causing injury)

#Filter data
am_sa <- crime_breakdown(crime_dz_code, c("Attempted Murder", "Serious Assault (incl. culpable & reckless conduct – causing injury)"))

#Save prepared data for analysis functions
saveRDS(domestic_abuse, file=paste0(profiles_data_folder, '/Prepared Data/domestic_abuse_raw.rds'))
saveRDS(domestic_abuse, file=paste0(profiles_data_folder, '/Prepared Data/domestic_abuse_depr_raw.rds'))

#Run analysis functions
main_analysis(filename = "domestic_abuse", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 21108, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_16to64", crude_rate = 10000)

deprivation_analysis(filename = "domestic_abuse", yearstart = 2007, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     pop_age = c(16, 64), crude_rate = 10000, ind_id = 21108)



###############################################.
## Part 6 - Breach of the Peace (4156)  ----
###############################################.

#"Breach of the Peace"

###############################################.
## Part 7 - Common Assault (4154)  ----
###############################################.

#Common Assault
#Common Assault (of an emergency worker)

###############################################.
## Part 8 - Drug Crimes (20806) ----
###############################################.

#Bringing drugs into prison
#Other drugs offences (incl. importation)
#Possession of drugs
#Production, manufacture or cultivation of drugs
#Supply of drugs (incl. possession with intent)

###############################################.
## Part 9 - Vandalism (4155) ----
###############################################.

#Vandalism (incl. reckless damage, etc.)

###############################################.
## Part 10 - Violent Crime (20805) ----
###############################################.

#All group 1 crimes - but maybe rename indicator to non-sexual violent crime

###############################################.
## Part 11 - Driving under the Influence (4158) ----
###############################################.

#No direct replacement - would need to use "Drink, Drug driving offences incl. Failure to provide a specimen
#May need renaming as well 

##End.
