# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes -----
# ~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:-
# 99144 - Children at risk of obesity

# Data sourced from Scottish Health Survey:
# Note that the chapter no. and table name that data is sourced from may vary each year
# The format also changes meaning steps below may need tweaked as at next update.

# 2023 survey was:
# Chapter 9: Diet and Obesity tables (Table 9.4 - Children BMI, 1998 to 2023, by sex) 
# https://www.gov.scot/publications/scottish-health-survey-2023-volume-1-main-report/documents/

# Note this differs from our Child healthy weight indicator which looks at healthy BMI in P1s
# This is survey based and looks at risk of obesity in those aged 2-15 years

# Files produced:
# main: Y
# popgroups: Y (sex)
# deprivation: N

# Publication table to be saved in following folder before running this script:
# \\Isdsf00d03\ScotPHO\Profiles\Data\Received Data\Scottish Health Survey 


# ~~~~~~~~~~~~~~~~~~~~~
# Dependencies -----
# ~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(readxl)

# ~~~~~~~~~~~~~~~~~~~
# 1. Read data ----
# ~~~~~~~~~~~~~~~~~~~

# file details 
folder <- file.path(profiles_data_folder, "Received Data", "Scottish Health Survey") 
file <- "chapter-9-obesity-tables.xlsx"
sheet_name <- "9.4"

# read in data
data <- read_excel(path = file.path(folder, file), sheet = sheet_name, skip = 2)
View(data) # view data to see format - may need adjust number of rows being skipped etc.

# ~~~~~~~~~~~~~~~~~~~~
# 2. clean data ----
# ~~~~~~~~~~~~~~~~~~~~

# filter on the 'At risk of obesity' indicator
# note it has a 'g' on the end due to excel tables having superscript footnote letters!
# This may change as at next update...
filtered_data <- data |>
  filter(`BMI status (National BMI percentiles)` == "At risk of obesityg") |>
  select(-`BMI status (National BMI percentiles)`)


# add sex col - there wasn't one in the original dataset but we know from looking at the excel tables
# (or checking the df called 'data') that there was a row for 'Males', then 'Females' then 'All' so we can use row number 
# Double check order still the same as at next update
filtered_data <- filtered_data |>
  mutate(split_value = case_when(row_number() == 1 ~ "Males",
                         row_number() == 2 ~ "Females",
                         row_number() == 3 ~ "All", .default = "other"))


# pivot data longer so theres just 1 year column
# Note there is only data for 1998, 2003 and then 2008-2023 (excluding 2020 due to covid)
filtered_data <- filtered_data |>
  pivot_longer(
  cols = -split_value, 
  names_to = "year", 
  values_to = "rate",
  names_transform = list(year = ~substr(.x, 1, 4)), # clean years - some have rogue letters next to them due to superscripting
  values_transform = list(rate = ~round(as.numeric(.x))) # remove decimal places from rates - only appear in latest years data
  )


# add columns required for final file 
filtered_data <- filtered_data |>
  mutate(
    split_name = "sex",
    code = "S00000001",
    numerator = NA,
    upci = NA,
    lowci = NA,
    trend_axis = year,
    def_period = year,
    ind_id = 99144,
  )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. create main file -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# remove sex splits 
main <- filtered_data |>
  filter(split_value == "All") |>
  select(-c(split_name, split_value))

# save final files
write.csv(main, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny.csv"), row.names = FALSE)
saveRDS(main, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. create popgroups file -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save popgroup files 
write.csv(filtered_data, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny_popgrp.csv"), row.names = FALSE)
saveRDS(filtered_data, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny_popgrp.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~
# 5 - QA files -----
# ~~~~~~~~~~~~~~~~~~~~~~

run_qa(filename = "99144_children_obesity_risk", type = "main")
run_qa(filename = "99144_children_obesity_risk", type = "popgrp")


## END



