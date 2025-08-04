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

source("./functions/data cleaning functions/fix_fin_year.R")

library(opendatascot)

#Uncomment this code to install the opendatascot package
# devtools::install_github(
#   "ScotGovAnalysis/opendatascot",
#   upgrade = "never",
#   build_vignettes = TRUE
# )

###############################################.
## Part 1 - Prepare basefile for all crime types ----
###############################################.

crime_data <- opendatascot::ods_dataset("recorded-crime", measureType = "count") |> 
  filter(crimeOrOffence == c("all-offences", "all-crimes"))


crime_data_2 <- crime_data |> 
  janitor::clean_names() |> #change names to snake case
  filter(ref_area != "S92000003") |>  #remove Scotland figures - to be re-added by analysis functions
  select(-measure_type) |>  #drop as only contains count
  fix_fin_year(ref_period) |>  #converts string financial year to numeric first year
  #mutate(numerator = as.numeric(value), .keep = "unused") |>  #creates numerator column which is value column as numeric, then drops val
  filter(year >= 2010) |> #filtering out incomplete early data
  filter(year != 2024) #filter out incomplete 24/25 data
  

###############################################.
## Part 2 - Create Recorded Crime Indicator (21108)  ----
###############################################.

recorded_crime <- crime_data_2 |> 
  filter(crime_or_offence == c("all-offences", "all-crimes"))

#exp 32 councils x 2 crime types x 14 years = 896 rows. Only have 415 rows.

