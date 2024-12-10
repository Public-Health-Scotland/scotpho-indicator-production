### Analyst notes ----

# This script prepares data for the 'Women smoking during pregnancy' (30058) indicator
# this is a replacement of smoking during pregnancy indicators which used to use SMR02
# the new data source is the antenatal booking system (ABC dataset)
# Data can be requested from the maternity team each year following the release of 
# the following publication (typically published in March):
# https://publichealthscotland.scot/publications/antenatal-booking-in-scotland/

# Important note: whenever there are changes to geography boundaries, the old smoking during pregnancy indicator
# needs to be re-run as it is still used within the profiles tool (see Smoking during pregnancy.R script in the 'Archived indicators' folder)


# temporary note November 2024: The data is taken from the latest ABC extract so will be different to the 2024 annual publication figures, 
# the figures should all align with 2025 annual publication figures at the next indicator update. 


### Sourcing analysis functions ----
source("1.indicator_analysis.R")
source("2.deprivation_analysis.R")

##################################.
## 1 - prepare basefiles ----
##################################.

# read in received data ----
raw_data <- read_csv(file = paste0(data_folder, "Received Data/Smoking during pregnancy/IR2024-00755_ABC_smoking_calyear.csv"))

# rename columns ----
raw_data <- raw_data |>
  rename(
    datazone = datazone2011,
    year = calyear,
    numerator = smoker,
    denominator = known_status
  )


# save data ----
saveRDS(raw_data, file = paste0(data_folder, 'Prepared Data/smoking_during_preg_raw.rds'))
saveRDS(raw_data, file = paste0(data_folder, 'Prepared Data/smoking_during_preg_depr_raw.rds'))



##################################.
## 2 - Run analysis functions ----
##################################.

# main dataset analysis functions ----
analyze_first(filename = "smoking_during_preg", geography = "datazone11", measure = "percent", 
              yearstart = 2020, yearend = 2023, time_agg = 3)

analyze_second(filename = "smoking_during_preg", measure = "percent", time_agg = 3, 
               ind_id = 30058, year_type = "calendar")

# deprivation analysis function ----
analyze_deprivation(filename="smoking_during_preg_depr", measure="percent", time_agg=3, 
                    yearstart= 2020, yearend=2023, year_type = "calendar", ind_id = 30058)


## END