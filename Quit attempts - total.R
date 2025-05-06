# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# 1505 - Smoking quit attempts

# data provided annually by the PHS smoking team following release of below publication (typically in december):
# https://publichealthscotland.scot/publications/nhs-stop-smoking-services-scotland/

# data provided at council, board and scotland level, split by SIMD quintile. 
# Council and board data provided for both local and scottish quintiles
# all data based on patients council of residence

# indicator can be updated at the same time as the 3 smoking quit rate indicators
# as all use the same basefile. They are updated from separate R scripts due to 
# some differences required in indicator production steps

# Although data is available by SIMD, we do not do SIMD splits for this particular
# indicator as measure is just a count of quit attempts. SIMD splits are
# done for the other quit rate (%) indicators.

# note we used to publish data from 2009/10 onwards, however 
# data now only available from 2014/15 due to data quality issues in Glasgow prior to this

# Data splits:
# Main - Yes
# Deprivation - No
# Pop groups - No

# Indicator production Steps:
# Part 1 - Housekeeping
# Part 2 - Read in and clean data 
# Part 3 - Prepare and save final file


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 1 - Housekeeping ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R") # load function to access profiles data filepath
library(arrow) # for reading parquet files


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 2 - Read in and tidy up data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get path to data file provided by smoking team
filepath <- file.path(profiles_data_folder, "Received Data", "Smoking quit attempts", "scotpho_simd_data.parquet")

# read in data 
data <- read_parquet(filepath) |>
  # temporary step May 2025: remove 2024/25 data as incomplete
  # either replace with 2025/26 at next update if required or remove
  filter(finyear != "2024/25")

# get totals for each year and geography
data <- data |>
  filter(sim_type == "simd_sc") |>
  group_by(finyear, geographic_code) |>
  summarise(numerator = sum(number_all_quit_attempts), .groups = "drop") |>
  # remove NA geographies - already included in scotland totals
  filter(!is.na(geographic_code)) |>
  # recode scotland geography code
  mutate(geographic_code = if_else(geographic_code == "S92000003", "S00000001", geographic_code))
  
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 3 - Prepare final file ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# adding/renaming columns to ensure in format required before saving final file 
main_data <- data |>
  rename(code = geographic_code,
         trend_axis = finyear) |>
  mutate(rate = numerator,
         upci = NA,
         lowci = NA,
         year = as.numeric(substr(trend_axis,1, 4)),
         def_period = trend_axis,
         ind_id = "1505") 


# folder to save final files
output_folder <- file.path(profiles_data_folder, "Data to be checked")

# save final files as csv and rds
saveRDS(main_data, file.path(output_folder, "1505_smoking_quit_attempts_shiny.rds"))
write.csv(main_data, file.path(output_folder, "1505_smoking_quit_attempts_shiny.csv"), row.names = FALSE)


## END

