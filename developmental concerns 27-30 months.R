#######################.
# Analyst notes ----
#######################.

# ScotPHO indicator: Developmental concerns at 27-30 weeks

# Data is requested annually from the child health team following this release (typically published in April)
# https://publichealthscotland.scot/publications/early-child-development/

# When data is recieved save it in the 'Developmental Concerns subfolder within the Recieved data folder and
# a. update the file-path to the data to be read in 
# b. update the 'yearend' parameters in the analysis functions

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("functions/main_analysis.R") #Normal indicator functions
source("functions/deprivation_analysis.R") # deprivation function
library(readxl)


###############################################.
## Part 1 - Prepare basefile ----
###############################################.

# read in the data (update filepath when recieve new data )
dev_concerns <- readxl::read_excel(file.path(profiles_data_folder, "Received Data/Developmental concerns/IR2024-00253_development27months.xlsx")) |>
  janitor::clean_names()


# removes all other geographies apart from datazone (needed if received data contains Scotland and hb data)
dev_concerns <- dev_concerns |> 
  filter(!(is.na(datazone2011)) | hb_residence_desc == "Unknown")


# fix year column to be starting year of the financial year 
dev_concerns <- dev_concerns |>
  mutate(year = substr(finyr_eligible, start = 1, stop = 4))

# summarise data per datazone and year/drop unwanted columns 
dev_concerns <- dev_concerns |>
  group_by(year, datazone2011) |>
  summarise(
    numerator = sum(concerns), 
    denominator = sum(reviews),
    .groups = "drop"
    )

# save temporary file to use in analysis functions
saveRDS(dev_concerns, file = file.path(profiles_data_folder, 'Prepared Data/dev_concerns_raw.rds')) 

###############################################.
## Part 2 - calling analysis functions ----
###############################################.

# create main file
main_analysis(filename = "dev_concerns", ind_id = 13048, 
              geography = "datazone11", measure = "percent", 
              yearstart = 2013, yearend = 2022, time_agg = 3, year_type = "financial")

# create deprivation file 
deprivation_analysis(filename = "dev_concerns", ind_id = 13048, measure = "percent", 
                     yearstart = 2013, yearend = 2022, time_agg = 3, year_type = "financial")


