# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# 15001: Long-acting reversible contraception prescribing rate
# (women aged 15-44 - primary care and sexual health combined)

# Data sourced from this publication (typically published anually in Sep):
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
  sheet = "Both Sources" # name of tab to read in 
  )


# ~~~~~~~~~~~~~~~~~~~~
# Clean data ----
# ~~~~~~~~~~~~~~~~~~~~

# only keep rows required
data_clean <- data |>
  head(-3) |> # remove bottom 3 rows (tweak as required)
  tail(-3) |> # remove top 3 rows (tweak as required)
  row_to_names(row_number = 1) |> # use new first row as header
  clean_names() |> # clean column names
  remove_empty(which = c("rows", "cols")) |> # remove NA rows and columns
  rename(areaname = 1) # rename first col



# pivot data longer
# data is in wide format, with a column for each year/rate (e.g. "2015/16") and year/numerator (e.g."2015/16.1")
# switch instead to columns year, rate and numerator

# pivot numerator data longer
numerator <- data_clean |>
  select(areaname, 2:11) |> # tweak as required
  pivot_longer(cols = -areaname, names_to = "year", values_to = "numerator", names_prefix = "x")

# pivot rate data longer 
rate <- data_clean |>
  select(areaname, 12:21) |> # tweak as required
  pivot_longer(cols = -areaname, names_to = "year", values_to = "rate", names_pattern = "x(.*)_2")


# combine numerator and rate data 
combined <- left_join(numerator, rate, by = c("areaname", "year"))



# add required columns for final file 
final <- combined |>
  mutate(
    # ind id columns
    ind_id = 15001, 
    
    # year columns 
    trend_axis = str_replace(year, pattern = "_", replacement = "/"),
    def_period = str_replace(year, pattern = "_", replacement = "/"),
    year = substr(year, start = 1, stop = 4),
    
    # upci/lowci columns 
    upci = NA, 
    lowci = NA
    ) |>
  # code column
  hb_names_to_codes(areaname) |>
  mutate(code = if_else(is.na(code), "S00000001", code))


# convert numerator and rate cols to class numeric 
final <- final |>
  mutate(across(c("numerator", "rate"), ~ as.numeric(.)))


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Save final files -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~

# save files in data to be checked folder
saveRDS(final, file = file.path(profiles_data_folder, "Data to be checked", "15001_larc_prescriptions_shiny.rds"))
write.csv(final, file = file.path(profiles_data_folder, "Data to be checked", "15001_larc_prescriptions_shiny.csv"), row.names = FALSE)


# clear global env.
rm(list = ls())
