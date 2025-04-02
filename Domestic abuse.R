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

###############################################.
## Read in data  ----
###############################################.

data <- opendatascot::ods_dataset("domestic-abuse-recorded-by-the-police-number-of-incidents-and-rates")  |> 
  janitor::clean_names() |> 
  
###############################################.
## Tidy up data  ----
###############################################.

filter(ref_area != "S9200000" & ref_area != "S92000003") |> #remove Scotland figs - to be re-added in analysis function
filter(measure_type == "count") |> #drop ratio measure and keep count which forms numerator
mutate(ref_period = substr(ref_period, 1, 4), #abbreviate fin year to first calendar year
       ref_period = as.numeric(ref_period)) |> #convert to numeric from character
rename(code = ref_area, #rename columns to format expected by analysis function
       year = ref_period,
       numerator = value) |> 
filter(year > 2012) |> 
select(-c(measure_type)) #drop count as no longer needed

#save prepared data
saveRDS(data, file = paste0(profiles_data_folder, "/Prepared Data/domestic_abuse_raw.rds"))
  
###############################################.
## Run analysis function
###############################################.

main_analysis(filename = "domestic_abuse", measure = "crude", geography = "council", 
              year_type = "financial", ind_id = 20804, time_agg = 1, yearstart = 2013,
              yearend = 2023, pop = "CA_pop_allages", crude_rate = 10000)

##End.
