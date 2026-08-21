# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# 15001: Long-acting reversible contraception prescribing rate
# (women aged 15-44 - primary care and sexual health combined)

# Data sourced from this publication (typically published annually in Sep):
# https://publichealthscotland.scot/publications/long-acting-reversible-contraception-larc-key-clinical-indicator-kci/

# Save 'Table 5 - LARC prescribing rate' (tab called 'Both Sources') excel doc in this network folder:
# \\Isdsf00d03\ScotPHO\Profiles\Data\Received Data\LARC prescribing



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages/functions/filepaths ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
source("functions/data cleaning functions/hb_names_to_codes.R")
library(readxl)



# ~~~~~~~~~~~~~~~~~~~~
# Read data ----
# ~~~~~~~~~~~~~~~~~~~~

# read in data 
data <- read_excel(
  path = file.path(profiles_data_folder, "Received Data", "LARC prescribing", "mat_la_table.xlsx"), # path to file saved on network
  sheet = "Both Sources", # name of tab to read in 
  range = "A5:K20" # range to filter on (2nd part of range needs tweaked each year as there's 1 column per year)
  )


# ~~~~~~~~~~~~~~~~~~~~
# Clean data ----
# ~~~~~~~~~~~~~~~~~~~~

# only keep rows required
data_clean <- data |>
  tail(-1) |> # remove top row (empty row)
  rename(areaname = 1) # rename first col



# pivot data longer so there's just 1 year column 
data_clean <- data_clean |>
  pivot_longer(cols = -areaname, names_to = "year", values_to = "numerator")


# add required columns for analysis function
data_long <- data_long |>
  mutate(year = substr(year, start = 1, stop = 4)) |> # convert FY year column to cal year 
  hb_names_to_codes(areaname) # add geo code column


# save temp file to be used in analysis function
saveRDS(final, file = file.path(profiles_data_folder, "Prepared Data", "15001_larc_prescriptions_raw.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate final indicator file ------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main_analysis(ind_id = 15001, filename = "15001_larc_prescriptions", geography = "board", 
              measure = "crude", crude_rate = 1000, pop = "CA_pop_fem15to49", 
              year_type = "financial", time_agg = 1, yearstart = 2015, yearend = 2024)


