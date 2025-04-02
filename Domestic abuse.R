# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# 20804: Domestic Abuse

# Definition: Rate of incidents of domestic abuse recorded by the police per 10,000 population

# Data is downloaded from SG website
#https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdomestic-abuse-recorded-by-the-police-number-of-incidents-and-rates

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

#sourcing analysis function
source("./functions/main_analysis.R")

#council area lookup
ca_lookup <- readRDS(paste0(profiles_data_folder, "/Lookups/Geography/CAdictionary.rds"))

###############################################.
## Read in data  ----
###############################################.

data <- readr::read_csv(paste0(profiles_data_folder, "/Received Data/Domestic Abuse/domestic_abuse.csv"),
                        skip = 6, col_select = -1) |> #skipping first rows of metadata and first column of s-codes formatting incorrectly

###############################################.
## Tidy up data  ----
###############################################.

  #removing unneeded rows/cols
  select(-c(2:5)) |>  #dropping early years of data to align with existing output
  slice(-c(1)) |>  #dropping Scotland figs - to be re-added in function

  #pivot longer
  tidyr::pivot_longer(cols = c(2:22), names_to = "year", values_to = "numerator") |> #create 1 year column instead of 1 per year
  mutate(numerator = as.numeric(numerator)) |>  #convert numerator from character to numeric
  rename(areaname = `Reference Area`) #|> #rename to areaname to match lookup
    
#join data to lookup 
data_cleaned <-  left_join(data, ca_lookup, by = "areaname") |> 
  mutate(year = substr(year, 1, 4), #abbreviate fin year to first calendar year
         year = as.numeric(year)) |> #convert year to numeric
  select(4, 2:3) #select final columns

#save prepared data
saveRDS(data_cleaned, file = paste0(profiles_data_folder, "/Prepared Data/domestic_abuse_raw.rds"))
  
###############################################.
## Run analysis function
###############################################.

main_analysis(filename = "domestic_abuse", measure = "crude", geography = "council", 
              year_type = "financial", ind_id = 20804, time_agg = 1, yearstart = 2003,
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)

##End.
