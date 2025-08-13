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

# The published data includes rates and a weighted base
# We can use both of these to back-calculate the numerator which
# allows us to calculate confidence intervals

# get rows of data containing weighted bases
weighted_base <- data |>
  filter(between(row_number(), 21, 23)) |>
  mutate(var = "denominator") |>
  rename(split_value = `BMI status (National BMI percentiles)`)
  


# get rows of data containing % rates by filtering on 'At risk of obesity' indicator
# note it has a 'g' on the end due to excel tables having superscript footnote letters!
# This may change as at next update...
rate <- data |>
  filter(`BMI status (National BMI percentiles)` == "At risk of obesityg") |>
  select(-`BMI status (National BMI percentiles)`) |>
  mutate(var = "rate") |>
# add sex col - we know from looking at the excel tables (or checking the df called 'data') 
# that there was a row for 'Males', then 'Females' then 'All' so we can use row number 
# Double check order still the same as at next update
  mutate(split_value = case_when(row_number() == 1 ~ "Males",
                         row_number() == 2 ~ "Females",
                         row_number() == 3 ~ "All children", .default = "other"))



# combine weighted data and % rate data 
all_data <- rbind(rate, weighted_base)


# pivot data longer so theres just 1 year column
# Note there is only data for 1998, 2003 and then 2008-2023 (excluding 2020 due to covid)
all_data <- all_data |>
  pivot_longer(
  cols = -c(split_value, var), 
  names_to = "year", 
  values_to = "value",
  names_transform = list(year = ~substr(.x, 1, 4)), # clean years - some have rogue letters next to them due to superscripting
  values_transform = list(value = ~round(as.numeric(.x))) # remove decimal places from rates - only appear in latest years data
  ) |>
  filter(year >= 2003) # decision made to start trend from 2003 onwards


# pivot data wider so separate rate and denominator columns
all_data <- all_data |>
  pivot_wider(names_from = "var")


# calculate confidence intervals
all_data <- all_data |>
  # calculate numerator 
  mutate(numerator = (rate/100) * denominator) |>
  # re-calculate rate with confidence intervals
  calculate_percent() |>
  # remove denominator col and make numerators NA
  select(-denominator) |>
  mutate(numerator = NA)

# add columns required for final file 
all_data <- all_data |>
  mutate(
    split_name = "sex",
    code = "S00000001",
    trend_axis = year,
    def_period = year,
    ind_id = 99144)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. create main file -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# remove sex splits 
main <- all_data |>
  filter(split_value == "All children") |>
  select(-c(split_name, split_value))

# save final files
write.csv(main, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny.csv"), row.names = FALSE)
saveRDS(main, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. create popgroups file -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save popgroup files 
write.csv(all_data, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny_popgrp.csv"), row.names = FALSE)
saveRDS(all_data, file.path(profiles_data_folder, "Data to be checked", "99144_children_obesity_risk_shiny_popgrp.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~
# 5 - QA files -----
# ~~~~~~~~~~~~~~~~~~~~~~

run_qa(filename = "99144_children_obesity_risk", type = "main")
run_qa(filename = "99144_children_obesity_risk", type = "popgrp")


## END



