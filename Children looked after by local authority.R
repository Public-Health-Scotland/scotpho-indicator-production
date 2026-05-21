# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script prepares data for the following indicator:-
# 20503 - Children looked after by local authority

# Full definition:
# Children looked after by the local authority; number and rate per 1,000 children aged 0-17 years.
# based on children looked after as at 31 July when snapshot taken

# Trend data used to be extracted solely from statistics.gov.scot using the opendatascot R package
# However, due to delays in that platform being updated, we have to use a combination 
# of open data (2009 - 2022) and data from two publication (for 2023 and 2024 data)
# from the childrens social work statistics (additional tables):

# 2023 data
# https://www.gov.scot/publications/childrens-social-work-statistics-2022-23-looked-after-children/

# 2024 data
# https://www.gov.scot/publications/childrens-social-work-statistics-looked-after-children-2023-24/documents/

# Review this as at next update as statistics.gov.scot platform may have been updated:
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Flooked-after-children


#   1. load/install packages and functions
#   2. Extract open data
#   3. Read in publication tables data
#   4. Prepare data file for analysis
#   5. run analysis function


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Load/install Packages and functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("functions/main_analysis.R") # for creating main dataset indicator file 
library(opendatascot) # for extracting data from statistics.gov.scot
library(readxl) # for reading in excel files
library(data.table) # for rbindlist() function to combine excel files


# uncomment the 2 lines below to install package if required:-
# install.packages("devtools")
# devtools::install_github("datasciencescotland/opendatascot")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Extract open data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# extract data 
opendata_extract <- ods_dataset("looked-after-children", 
                                measureType="count",
                                residentialStatus = "all",
                                geography = "la") 


# select/rename columns and change column classes
opendata_extract  <- opendata_extract |>
  select(code = refArea, 
         year = refPeriod, 
         numerator = value)
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Read in publication tables data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# name of folder where tables have been saved
folder <- file.path(profiles_data_folder, "Received Data", "Looked after children")

# full filepath for each file in folder 
files <- list.files(folder, full.names = TRUE)

# read in and combine 'Table 3.2 'from different years publications
# excel files must be closed or you'll get an error!
pub_tables <- rbindlist(lapply(files, function(x) {
  read_xlsx(x, sheet = "Table 3.2", skip = 4) |>
    mutate(file = basename(x))
  }))


# create year column taking the end year of the financial year
# as the data on looked after children is based on a snapshot in 
# July which falls in the 2nd year of the FY 
pub_tables <- pub_tables |>
  mutate(year = case_when(
    grepl("2022-23", file) ~ 2023,
    grepl("2023-24", file) ~ 2024,
    .default = NA)
    )




# read in council area lookup containing geography codes
ca_dictionary <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "CAdictionary.rds"))

# join with lookup 
pub_tables <- pub_tables |>
  inner_join(ca_dictionary, by = c("Local authority" = "areaname"))



# select/rename required cols
pub_tables <- pub_tables |>
  select(year,code, numerator = `Total number of children looked after`)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Prepare data file for analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# combine open data and publication data to get full time series
data_combined <- bind_rows(pub_tables, opendata_extract)
  
# save file to be used in analysis function
saveRDS(data_combined, file.path(profiles_data_folder, 'Prepared Data/looked_after_raw.rds'))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. run analysis function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main_analysis(filename = "looked_after", ind_id = 20503, geography = "council", 
              measure = "crude", pop = "CA_pop_under18", crude_rate = 1000,
              yearstart = 2009, yearend = 2024, time_agg = 1, year_type = "snapshot")



## END