# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following 2 indicators:
# 13107: Primary school children
# 13108: Secondary school children

# Data is downloaded following the release of the Pupil census supplementary statistics (usually in March):
# https://www.gov.scot/publications/pupil-census-supplementary-statistics/

# Save the latest published file in the pupil census data folder (/PHI_conf/ScotPHO/Profiles/Data/Receieved Data/School pupil census data)
# and update the name of the file in the 'pupil_census_data' object below to read in the latest data. Update the end_date parameters in the 
# main_analysis functions.

# Note March 2025: Indicators cannot be updated following release of 2025 pupil census data (due end of March 2025) as population estimates are the denominator for these indicators
# we do not yet have population estimates for 2024 or 2025 to do this. Data therefore only prepared up to 2023 until we have denominator available.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions/filepaths/packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("functions/main_analysis.R") # source main analysis function 
library(rio) # for import_list() function to reading and combine multiple excel sheets from same file 

# full path to raw pupil census data 
pupil_census_folder <- file.path(profiles_data_folder, "Received Data", "School pupil census data")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read and prepare raw data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in data from the 2 sheets containing 
# data on number of primary and secondary pupils 
data <- rio::import_list(file = file.path(pupil_census_folder, "Pupil+Census+Supplementary+Statistics+2024+-+December+v2.xlsx"), 
                         which = c("Table 6.2", "Table 7.2"), 
                         rbind = TRUE, 
                         rbind_label = "sheet", 
                         skip = 1)


# create column to identify if data relates to primary or secondary pupils
# Data from Table 6.2 is primary pupils and 7.2 is secondary pupils
data <- data |>
  mutate(cohort = if_else(sheet == "Table 6.2", "primary", "secondary")) |>
  select(-sheet)


# remove columns containing male and female splits
data <- data |>
  select(-contains(c("male", "female")))


# pivot data data longer so there's just 1 year column 
# as the raw data is in wide format with 1 column for each year 
data <- pivot_longer(data, 
                     cols = -c(`Local Authority`, cohort), 
                     names_to = "year", 
                     values_to = "numerator",
                     names_transform = list(year = ~ as.numeric(substr(., 1, 4)))
                     )


# read in historic pupil census data from 2007 publication to get:
# 2002-2005 for primary 1 pupils (latest publication data starts from 2006 onwards)
# 2002-2004 for primary 7 pupils (latest publication data starts from 2005 onwards)
data_historic <- readRDS(file.path(pupil_census_folder, "pupil_census_2002_to_2005.rds"))


# combine historic data with latest publication data to start time trend from 2002
data <- data |>
  bind_rows(data_historic) |>
  arrange(year)

# replace some council names in order to successfully join data with council area lookup in next step
data <- data |>
  mutate(`Local Authority` = str_replace(`Local Authority`, "&", "and"),
         `Local Authority` = str_replace(`Local Authority`, "Edinburgh City", "City of Edinburgh"),
         `Local Authority` = str_replace(`Local Authority`, "Eilean Siar", "Na h-Eileanan Siar")
         ) |>
  filter(!`Local Authority` %in% c("All local authorities", "Grant aided", "Scotland"))

# get council area lookup containing geography codes 
lookup <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "CAdictionary.rds"))


# join data with lookup so there is a geography code column
data <- data |>
  left_join(lookup, by = c(`Local Authority` = "areaname"))



# read in council area population estimates - these are required to use as denominator 
# note the analysis function expects denominator column to already be included in basefile
# as all of our percentage measures typically don't use populations as the denominator
# hence why this step is being done here
pop_lookup <- get_population_lookup(folder = file.path(profiles_data_folder, "Lookups", "Population"), 
                                    pop = "CA_pop_allages", 
                                    measure = "percent")


# join data with population estimates to create denominator column 
data <- left_join(data, pop_lookup, by = c("year", "code"))


# split the data by primary and secondary pupils 
data <- split(data, data$cohort)


# save temporary data files to be used in analysis functions 
saveRDS(data$primary, file.path(profiles_data_folder, "Prepared Data", "13107_primary_pupils_raw.rds"))
saveRDS(data$secondary, file.path(profiles_data_folder, "Prepared Data", "13108_secondary_pupils_raw.rds"))  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run analysis functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# analyse and prepare final file for primary pupils indicator  
main_analysis(filename = "13107_primary_pupils", geography = "council", measure = "percent", yearstart = 2002,
              yearend = 2023, time_agg = 1, year_type = "calendar", ind_id = 13107)


# analyse and prepare final file for secondary pupils indicator 
main_analysis(filename = "13108_secondary_pupils", geography = "council", measure = "percent", yearstart = 2002,
              yearend = 2023, time_agg = 1, year_type = "calendar", ind_id = 13108)


## END
