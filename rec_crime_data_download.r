# ScotPHO indicators: Recorded Crime

#Temporary script to restore CA-level figures while dz-level data is quality assured
#This script draws from the SG publication rather than Police Scotland FOI data

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
## Part 2 - Create Recorded Crime Indicator (21108)  ----
###############################################.

recorded_crime <- crime_offence |> 
  filter(crime_offence_group %in% c("Total Crimes", "Total Offences"))



