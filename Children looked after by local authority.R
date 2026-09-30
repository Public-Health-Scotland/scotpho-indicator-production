# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script prepares data for the following indicator:-
# 20503 - Children looked after by local authority

# Full definition:
# Children looked after by the local authority; number and rate per 1,000 children aged 0-17 years.
# based on children looked after as at 31 July when snapshot taken

# Trend data from data.gov.scot only available from 2009-2022. Therefore have to use a combination of this 
# and data from three publication (for 2023-2025 data)
# from the childrens social work statistics (additional tables):

# 2023 data (Table 3.2)
# https://www.gov.scot/publications/childrens-social-work-statistics-2022-23-looked-after-children/

# 2024 data (Table 3.2)
# https://www.gov.scot/publications/childrens-social-work-statistics-looked-after-children-2023-24/documents/

# 2025 data (Table 23)
# https://www.gov.scot/publications/childrens-social-work-statistics-looked-after-children-2024-25/documents/

# Review this as at next update as data.gov.scot platform may have been updated:
# https://data.gov.scot/dataset/looked_after_children


#   1. load/install packages and functions
#   2. Extract data.gov.scot data
#   3. Read in publication tables data
#   4. Prepare data file for analysis
#   5. run analysis function


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Load/install Packages and functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("functions/main_analysis.R") # for creating main dataset indicator file
source("functions/data cleaning functions/ca_names_to_codes.R") # for adding geo code column
library(readxl) # for reading in excel files
library(data.table) # for rbindlist() function to combine excel files




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Extract open data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# extract data 
opendata_extract <- read.csv("https://data.gov.scot/dataset/looked_after_children/resource/08744c2f-657f-402e-b6df-1e9d4e94a485/download")

# Apply filters
opendata_extract <- opendata_extract |>
  filter(
    Measurement == "Count" &
      Residential.Status == "All" &
      GeographyType == "Council Areas"
  )



# select/rename columns and change column classes
opendata_extract  <- opendata_extract |>
  select(code = GeographyCode, 
         year = DateCode, 
         numerator = Value)
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Read in publication tables data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# name of folder where tables have been saved
folder <- file.path(profiles_data_folder, "Received Data", "Looked after children")

# full filepath for each file in folder 
files <- list.files(folder, full.names = TRUE)


# read in and combine correct sheet from different years publications
# excel files must be closed or you'll get an error!
pub_tables <- rbindlist(lapply(files, function(x) {
  
  # name of file to read in 
  filename <- basename(x)
  
  # name of sheet to read in
  sheetname <- ifelse(grepl("2022-23|2023-24", filename), "Table 3.2", "Table 23")
  
  read_xlsx(x, sheet = sheetname, range = "A5:N37") |>
    mutate(file = filename)
  
  }))


# create year column taking the end year of the financial year
# as the data on looked after children is based on a snapshot in 
# July which falls in the 2nd year of the FY 
pub_tables <- pub_tables |>
  mutate(year = case_when(
    grepl("2022-23", file) ~ 2023,
    grepl("2023-24", file) ~ 2024,
    grepl("2024-25", file) ~ 2025,
    .default = NA)
    )


# add council area codes
pub_tables <- pub_tables |>
  ca_names_to_codes(council_area = `Local authority`)


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
              yearstart = 2009, yearend = 2025, time_agg = 1, year_type = "snapshot")



## END