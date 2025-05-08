# ~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes -----
# ~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# 1526 - Smoking quit attempts from pregnant smokers

# denominator - quit attempts from pregnant smokers (data provided annually by the PHS smoking team)
# numerator - total pregnant smokers (data extracted from PHS open data platform)

# Pregnant smoking data still uses SMR02. Pregnancy team recently switch their data source
# to the antenatal booking collection data (ABC) which only has 2 years worth of data to report on.
# Need to check if the open data using SMR02 will be updated going forward of if we need to switch to ABC
# and shorten the ime series of this indicator

# note that although data is available by SIMD quintile we cannot calculate
# rates at this level as there are instances where the number of pregnant quit attempts (numerator)
# is larger than the number of pregnant smokers (denominator). There's also a lot of >5 numerators
# Data is still prepared below in format required to to SIMD splits, incase position changes in the future.

# Important to remember that this metric has some potential flaws in data collection
# the number of pregnant smokers is based on self-reported smoking status at antenatal booking
# It is possible that not all women who are pregnant report their smoking status or smoking status could changes during pregnancy
# It is also possible that women may attempt to quit smoking in early stages of pregnancy before completing antenatal booking
# several factors may explain why numerator may be higher than denominators

# Data splits:
# Main - Yes
# Deprivation - No
# Pop groups - No

# Indicator production Steps:
# Part 1 - housekeeping 
# Part 2- Prepare numerator data
# Part 3 - Prepare denominator data
# Part 4 - combine numerator and denominator
# Part 5 - Create main data file 
# Part 6 - Create deprivation data file (current commented out)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 1 - Housekeeping ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
source("functions/deprivation_analysis.R")
library(arrow) # for reading parquet files
library(phsopendata) # for extracting opendata

# uncomment and run line below if need to install phsopendata package
# remotes::install_github("Public-Health-Scotland/phsopendata", upgrade = "never")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 2 - Prepare numerator data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# extract data from opendata platform
# https://www.opendata.nhs.scot/dataset/births-in-scottish-hospitals/resource/e87a7673-0397-43ca-91a5-166184319728%20%20e87a7673-0397-43ca-91a5-166184319728
preg_smokers <- get_resource(res_id = "e87a7673-0397-43ca-91a5-166184319728", row_filters = list(SmokingAtBooking = "Current smoker")) |>
  clean_names()

# clean data 
preg_smokers <- preg_smokers |>
  # get starting year of finyear and filter from 2014 onwards
  # to match smoking quit attempts data 
  mutate(year = as.numeric(substr(financial_year, 1, 4))) |>
  filter(year >=2014) |>
  # aggregate to get total number of pregnant smokers per year, council and simd quintile
  group_by(year, financial_year, ca, simd_quintile) |>
  summarise(number_pregnant_smokers = sum(maternities), .groups = "drop")|>
  # get overall totals per year and council (i.e. no simd split)
  group_by(year, financial_year, ca) |>
  group_modify(~ .x |> adorn_totals()) |>
  filter(!simd_quintile == " ") |>
  ungroup() |>
  # rename simd column to that required in our final files
  rename("simd" = "simd_quintile") |>
  # add a quint type column 
  mutate(sim_type = "simd_sc")


# get geography lookup and prepare at HB/CA level
geo_lookup <- readRDS(file.path(profiles_lookups, "Geography", "DataZone11_All_Geographies_Lookup.rds")) |>
  select(ca2019, hb2019) |>
  unique()

# join data with lookup and pivot data longer so just 1 geography code column 
# calculate number of pregnany smokers for each geography, year and simd quintile
preg_smokers <- left_join(preg_smokers, geo_lookup, by = c("ca" = "ca2019")) |>
  mutate(scotland = "S00000001") |>
  pivot_longer(cols = c("ca", "hb2019", "scotland"), names_to = NULL, values_to = "geographic_code") |>
  group_by(year, financial_year, simd, sim_type, geographic_code) |>
  summarise_all(sum) |>
  ungroup() |>
  # remove unknown geographies - now included in scotland totals
  filter(!is.na(geographic_code) & geographic_code != "RA2704")
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 3 - prepare denominator data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get path to data file provided by smoking team
filepath <- file.path(profiles_data_folder, "Received Data", "Smoking quit attempts", "scotpho_simd_data.parquet")

# read in data 
quit_attempts <- read_parquet(filepath) |>
  # temporary step May 2025: remove 2024/25 data as incomplete
  # either replace with 2025/26 at next update if required or remove
  filter(finyear != "2024/25")
  
# remove NAs
quit_attempts <- quit_attempts |>
  filter(!is.na(geographic_code))


# create 'total' rows for each group
# step required as data split by simd quintile (Q1-5 + unknown)
quit_attempts <- quit_attempts |>
  group_by(finyear, geographic_level, geographic_code, sim_type) |>
  group_modify(~ .x |> adorn_totals()) |>
  ungroup() |>
  filter(simd != "Unknown") # remove unknown quintiles after calculating totals


# get starting year from fin year
quit_attempts <- quit_attempts |>
  mutate(year = as.numeric(substr(finyear, 1, 4)))


# filter on scottish quintiles only 
# as pregnant smokers data not available for local quintiles
quit_attempts <- quit_attempts |>
  filter(sim_type == "simd_sc")

quit_attempts <- quit_attempts |>
  select(year, finyear, geographic_code,sim_type, simd, number_pregnant_quit_attempts)


# recode scotland 
quit_attempts <- quit_attempts |>
  mutate(geographic_code = if_else(geographic_code == "S92000003", "S00000001", geographic_code))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 4 - combine numerator and denominator  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

preg_quit_attempts <- left_join(preg_smokers, quit_attempts) |>
  rename(numerator = number_pregnant_quit_attempts,
         denominator = number_pregnant_smokers) |>
  mutate(numerator = if_else(is.na(numerator), 0, numerator))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 5 - create main file ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get council area totals
main_data <- preg_quit_attempts |>
  filter(simd == "Total" & grepl("S12", geographic_code)) |>
  select(year, geographic_code, numerator, denominator)

# save temp file 
saveRDS(main_data, file.path(profiles_data_folder, "Prepared Data", "1526_quit_attempts_pregnant_raw.rds"))

# run analysis function 
main_analysis(filename = "1526_quit_attempts_pregnant", ind_id = 1526, geography = "council", 
              measure = "percent", yearstart = 2014, yearend = 2023, time_agg = 3, year_type = "financial")

run_qa(filename="1526_quit_attempts_pregnant", type="main", test_file=FALSE)


# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Part 6 - create deprivation file ----
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # unable to split by
# 
# # Data already aggregated at HB, CA, Scotland level for each SIMD quintile and quint type
# # so no need to run through the deprivation_analysis function
# # the steps below calculate rates and ensure all variables/values in required format for final file
# 
# # rename quint columns and values
# simd_data <- preg_quit_attempts |>
#   rename("quintile" = "simd",
#          "quint_type" = "sim_type"
#   ) |>
#   mutate(quint_type = case_when(quint_type == "simd_sc" ~ "sc_quin",
#                                 quint_type == "simd_ca" ~ "ca_quin",
#                                 quint_type == "simd_hb" ~ "hb_quin", TRUE ~ "unknown"))
# 
# # prepare final deprivation file
# simd_data <- simd_data |>
#   # select and rename columns
#   select(
#     year,
#     code = geographic_code,
#     numerator,
#     denominator,
#     quint_type,
#     quintile
#   ) |>
#   # calculate rate
#   calculate_percent() |>
#   # caculate SII/RII/PAR
#   calculate_inequality_measures() |>
#   # add columns required for deprivation final file
#   mutate(ind_id = "1526") |>
#   create_trend_axis_column(year_type = "financial", agg = 1) |>
#   create_def_period_column(year_type = "financial", agg = 1)
# 
# # save final deprivation file
# saveRDS(simd_data, file.path(profiles_data_folder, "Data to be checked", "1526_quit_attempts_pregnant_depr_ineq.rds"))
# 
# 
# # QA deprivation file
# run_qa(filename = "1526_quit_attempts_pregnant_depr", type = "deprivation", test_file = FALSE)
# 
# 
# ## END