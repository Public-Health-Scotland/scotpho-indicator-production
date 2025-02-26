#ScotPHO Indicators: Recorded Crime rate

#   Part 1 - Prepare basefile
#   Part 3 - Run analysis functions

#Note - add link to FOI page on Police Scotland website if the indicator continues to be updated this way.

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

#Running the function - may take a few minutes. 
recorded_crime <- combine_files(file = list.files(path = filepath, pattern = ".xlsx", full.names = T))

rec_crime <- recorded_crime |> 
  janitor::clean_names() |> #simplify col names
  select(c(1:3, 5:6)) |> #keeping only number of crimes, year, month number, datazone name, division name
  mutate(rec_date = lubridate::my(paste(month_number, year_2)), #convert month and year columns to date format
         fin_year = phsmethods::extract_fin_year(rec_date)) #extract financial year from date

#2. Aggregate files by datazone name, financial year and division name
rec_crime_agg <- rec_crime |> 
  group_by(datazone, fin_year, division_name) |>  #aggregate the months to get whole year totals by dz. Inc. division name as some duplicate dz names e.g. City Centre 01
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
  select(c(2,4,6)) |>  #select only year, numerator and dz code
  rename(datazone = DZ2011_Code) |> #change name for analysis function
  select(datazone, everything()) #move DZ to first col

#Save prepared data for analysis functions
saveRDS(crime_dz_code, file=paste0(profiles_data_folder, '/Prepared Data/recorded_crime_raw.rds'))
saveRDS(crime_dz_code, file=paste0(profiles_data_folder, '/Prepared Data/recorded_crime_depr_raw.rds'))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "recorded_crime", geography = "datazone11", measure = "crude",
              year_type = "financial", ind_id = 21108, time_agg = 1, yearstart = 2007, 
              yearend = 2022, pop = "DZ11_pop_16to64", test_file = T, crude_rate = 10000)

deprivation_analysis(filename = "recorded_crime", yearstart = 2007, yearend = 2022,
                     time_agg = 1, year_type = "financial", measure = "crude", pop_sex = "all",
                     pop_age = c(16, 64), crude_rate = 10000, ind_id = 21108, test_file = T)

###############################################.
## Part 3 - Remove affected intermediate zones ----
###############################################.
crime_dz_code <- readRDS(paste0 (profiles_data_folder, "/Test Shiny Data/recorded_crime_shiny.rds"))

crime_dz_code<- crime_dz_code |> 
  filter(!(code %in% c("S02001528","S02001953","S02002233","S02001475")))
  
#Save final file
saveRDS(crime_dz_code, file=paste0(profiles_data_folder, "/Test Shiny Data/recorded_crime_shiny.rds"))
write.csv(crime_dz_code, file=paste0(profiles_data_folder, "/Test Shiny Data/recorded_crime_shiny.csv"), row.names = FALSE)

##End.