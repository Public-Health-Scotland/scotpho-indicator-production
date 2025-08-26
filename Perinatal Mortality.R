# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  Indicator: Perinatal Mortality Rate (Inidicator ID 99139)

# Description: Perinatal Mortality Rate per 1,000 births (stillbirths of gestation 28 weeks onwards plus deaths in the first week of life) 

# Data source is Section 4: Stillbirths and infant deaths from NRS:
# Section 4: Stillbirths and infant deaths (specifically Table_402) 
# https://www.nrscotland.gov.uk/publications/vital-events-reference-tables-2024/

# Note that data only available at Scotland level (and there is no confidence intervals) and goes back as far as 1971
# Would be better to source Board level data from 2002 onwards instead however PHS SBAND dashboard
# only includes scotland level data https://scotland.shinyapps.io/phs-pregnancy-births-neonatal/ so unlikely to happen?

# The name of the excel file, sheet and number of rows to skip may vary each year.
# Data cleansing steps may also need revised due to column name changes etc.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Packages/Filepaths/Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
source("functions/main_analysis.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in and clean data  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# folder where excel file to be saved
folder <- file.path(profiles_data_folder, "Received Data", "Perinatal mortality")

# some of these details may change each year - check file before reading in 
filename <- "vital-events-reference-tables-chapter-4.xlsx"
sheetname <- "Table_402"
rows_skip = 6

# read data 
perinatal_data <- read_xlsx(file.path(folder, filename), sheet = sheetname, skip = rows_skip) |>
  clean_names()

# clean data 
clean_data <- perinatal_data |>
  filter(year %in% c(2002:2024) & sex == "Both sexes") |>
  rename(rate = perinatal_death_rate_including_stillbirths_of_gestation_28_weeks_onwards)

# add required columns 
clean_data <- clean_data |>
  mutate(ind_id = "99139",
         trend_axis = year, 
         def_period = paste0(year, " calendar year"),
         lowci = NA, 
         upci = NA,
         numerator = NA,
         code = "S00000001") |>
  # Select relevant columns
  select(ind_id, code, year, trend_axis, def_period, numerator, rate, lowci, upci) 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save final files -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save files in folder to be checked
write.csv(perinatal_data, file.path(profiles_data_folder, "Data to be checked/99139_perinatal_mortality_shiny.csv"), row.names = FALSE)
write_rds(perinatal_data, file.path(profiles_data_folder, "Data to be checked/99139_perinatal_mortality_shiny.rds"))

# QA the data (note there are no CIs so it will flag an error about them)
run_qa("99139_perinatal_mortality", type = "main")

#END