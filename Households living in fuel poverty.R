# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# Indicator: Households living in fuel poverty (Indicator ID 99140)

# Note the definition of fuel poverty was updated in 2019. This indicator uses fuel poverty rates
# defined under this new definition. More details about definition can be found here:
# https://statistics.gov.scot/data/fuel-poverty-shcs 

# Data source is Scottish Household Conditions Survey (SHCS)
# We extract via the the opendata scotland R package (opendatascot)


# Note Feb 2025:
# there is currently no data available beyond 2017-2019 
# 2020 and 2021 data were unavailable due to the pandemic and 2022 single year data is only available at scotland level
# (we use aggregate years data as council data not available for single years due to sample size)
# it is therefore likely going to be a couple of years before this indicator can be updated again 
# with aggregate data covering 2022-2024

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages/Filepaths/Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(opendatascot)

# uncomment below and run if need to install the opendatascot package
# devtools::install_github("ScotGovAnalysis/opendatascot",upgrade = "never",build_vignettes = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~
# Extract data ----
# ~~~~~~~~~~~~~~~~~~~~~~

# extract data
data_extract <- opendatascot::ods_dataset("fuel-poverty-shcs") |>
  clean_names() # clean column names 


# filter dataset 
data_clean <- data_extract |>
  filter(
    age_of_dwelling == "all",
    type_of_dwelling == "all",
    number_of_bedrooms == "all",
    type_of_tenure == "all",
    household_type == "all",
    fuel_poverty == "fuel-poor")


# pivot data longer so there are separate rate, upper ci and lower ci columns
data_clean <- data_clean |>
  pivot_wider(names_from = measure_type, values_from = value)


# select and rename columns 
data_clean <- data_clean |>
  select(
    code = ref_area,
    year = ref_period,
    rate = percent,
    upci = `95-upper-confidence-limit-percent`,
    lowci = `95-lower-confidence-limit-percent`,
  )

# fix year columns 
data_clean <- data_clean |>
  # remove single years data at scotland level - scotland data available as both single years and 3-year
  # aggregates whereas CA data only available at 3-year aggregates
  filter(grepl("-", year)) |>
  # create def period and trend axis columns 
  mutate(
    trend_axis = year,
    def_period = paste0(trend_axis, "; 3-year aggregates"),
    # make year column single numeric year
    year = as.numeric(paste0("20", substr(year, 3, 4)))
  )


# replace Scotland code
data_clean <- data_clean |>
  mutate(code = str_replace(code, "S92000003", "S00000001"))

# add ind_id col and numerator col
data_clean$ind_id <- "99140"
data_clean$numerator <- NA # there is no numerator but column still required

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save final files ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save files in folder to be checked
write.csv(data_clean, file.path(profiles_data_folder, "Data to be checked/99140_fuel_poverty_shiny.csv"), row.names = FALSE)
write_rds(data_clean, file.path(profiles_data_folder, "Data to be checked/99140_fuel_poverty_shiny.rds"))

# QA data
run_qa("99140_fuel_poverty", type = "main", test_file = FALSE)

#END