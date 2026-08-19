# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script prepares data for: 15003 - Adults living with obesity
# Note that the 'Scottish Health Survey.R' script has steps for preparing data from statistics.gov.scot. 
# However, for this particular indicator it only has data available up to 2016-19, where as downloading data directly 
# from the SHeS shiny app has extra years (up to 2021-2024) - https://scotland.shinyapps.io/sg-scottish-health-survey/
# Check in future if this script can be removed and indicator updated via other script.


# Instructions:
# Go to data tab of shiny app
# Select Scotland, local authority, health board and select each individual areaname
# Select indicator called 'Obesity'
# Press download button
# Save data on network folder here: \\Isdsf00d03\ScotPHO\Profiles\Data\Received Data\Adults living with obesity



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions/Filepaths/Packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("functions/main_analysis.R")


# ~~~~~~~~~~~~~~~~~~~
# Read data ----
# ~~~~~~~~~~~~~~~~~~~

# read in downloaded data extract
data <- read.csv(file.path(profiles_data_folder, "Received Data", "Adults living with obesity", "rank_data.csv"))

# read in geography lookup (for adding geo codes)
geo_lookup <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "opt_geo_lookup.rds")) |>
  filter(areatype %in% c("Health board", "Council area", "Scotland"))



# ~~~~~~~~~~~~~~~~~~~
# Format data ----
# ~~~~~~~~~~~~~~~~~~~

# filter on obesity
data_clean <- data |>
  clean_names() |>   # clean column names
  filter(categories == "Obesity")


# add geography codes
data_clean <- data_clean |>
  # fix areanames before joining with geography lookup 
  mutate(location = if_else(geographylevel == "Health Board", paste("NHS", location), location)) |>  # add 'NHS' to board name
  mutate(location = str_replace(string = location, pattern = " and ", replacement = " & ")) |> # replace 'and' with'&'
  mutate(location = str_replace(string = location, pattern = "Edinburgh City", replacement = "City of Edinburgh")) |> # replace edi spelling
  left_join(geo_lookup, by = c("location" = "areaname")) # join with geo lookup


# add additional required columns
data_clean <- data_clean |>
  mutate(
    ind_id = 15003,
    numerator = NA,
    trend_axis = year,
    def_period = year,
    year = as.numeric(substr(year, 1, 4)) + 2
  )


# rename columns 
data_clean <- data_clean |>
  rename(
    areaname = location,  
    rate = percent, 
    upci = upper_ci, 
    lowci = lower_ci
  )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create and save final files ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# main dataset
main_data <- data_clean |>
  filter(sex == "All") |>
  select(ind_id, year, code, numerator, rate, upci, lowci, def_period, trend_axis)


# popgroups dataset 
popgrp_data <- data_clean |>
  mutate(split_value = "Sex") |>
  rename(split_name = sex) |>
  select(ind_id, year, code, split_name, split_value, numerator, rate, upci, lowci, def_period, trend_axis)



# folder to save indiator files 
output_folder <- file.path(profiles_data_folder, "Data to be checked")


# save main dataset files
saveRDS(main_data, file.path(output_folder, "15003_adult_obesity_shiny.rds"))
write.csv(main_data, file.path(output_folder, "15003_adult_obesity_shiny.csv"), row.names = FALSE)


# save popgroup dataset files
saveRDS(popgrp_data, file.path(output_folder, "15003_adult_obesity_shiny_popgrp.rds"))
write.csv(popgrp_data, file.path(output_folder, "15003_adult_obesity_shiny_popgrp.csv"), row.names = FALSE)


# ~~~~~~~~~~~~~~~~~~~
# QA files ----
# ~~~~~~~~~~~~~~~~~~~

run_qa(filename = "15003_adult_obesity", type = "main")
run_qa(filename = "15003_adult_obesity", type = "popgrp")


## end

