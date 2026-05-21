# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# 1536 - Smoking quit rate at 4 weeks follow-up

# data provided annually by the PHS smoking team following release of below publication (typically in december):
# https://publichealthscotland.scot/publications/nhs-stop-smoking-services-scotland/

# data provided at council, board and scotland level, split by SIMD quintile. 
# Council and board data provided for both local and scottish quintiles
# all data based on patients council of residence

# note this indicator can be updated at the same time as the 3 other smoking quits indicators
# as they all use the same basefile. They are updated from separate R scripts due to 
# some differences required in indicator production steps

# note we used to publish data from 2009/10 onwards, however 
# data now only available from 2014/15 due to data quality issues in Glasgow prior to this

# Data splits:
# Main - Yes
# Deprivation - Yes
# Pop groups - No


# Indicator production Steps:
# Part 1 - Housekeeping
# Part 2 - Read in and clean data 
# Part 3 - Create main indicator file 
# Part 4 - create deprivation file

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 1 - Housekeeping ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
source("functions/deprivation_analysis.R")
library(arrow) # for reading parquet files


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 2 - Read in and clean data  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get path to data file provided by smoking team
filepath <- file.path(profiles_data_folder, "Received Data", "Smoking quit attempts", "scotpho_simd_data.parquet")

# read in data 
data <- read_parquet(filepath)

# remove NAs - already included in Scotland totals
data <- data |>
  filter(!is.na(geographic_code))


# create 'total' rows for each group
# step required as data split by simd quintile (Q1-5 + unknown)
data <- data |>
  group_by(finyear, geographic_level, geographic_code, sim_type) |>
  group_modify(~ .x |> adorn_totals()) |>
  ungroup() |>
  filter(simd != "Unknown") # remove unknown quintiles after calculating totals


# get starting yer from fin year
data <- data |>
  mutate(year = as.numeric(substr(finyear, 1, 4)))
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 3 - Create main indicator file ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main_data <- data |>
  # filter on council area totals
  filter(simd == "Total" & sim_type == "simd_sc" & geographic_level == "ca") |>
  # select and rename required columns
  select(
    year, 
    geographic_code, 
    numerator = number_all_12_week_quits, 
    denominator = number_all_quit_attempts
  )

# save temp file to be used in main_analysis function 
saveRDS(main_data, file.path(profiles_data_folder, "Prepared Data", "1537_quit_rate_12weeks_raw.rds"))

# run analysis function 
main_analysis(filename = "1537_quit_rate_12weeks", ind_id = "1537", measure = "percent", geography = "council", 
              time_agg = 1, year_type = "financial",yearstart = 2014, yearend = 2024)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 4 - create deprivation file ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data already aggregated at HB, CA, Scotland level for each SIMD quintile and quint type
# so no need to run through the deprivation_analysis function
# the steps below calculate rates and ensure all variables/values in required format for final file

# rename quint columns and recode quint type values
simd_data <- data |>
  rename("quintile" = "simd",
         "quint_type" = "sim_type") |>
  mutate(quint_type = case_when(quint_type == "simd_sc" ~ "sc_quin",
                                quint_type == "simd_ca" ~ "ca_quin",
                                quint_type == "simd_hb" ~ "hb_quin", TRUE ~ "unknown"))

# recode scotland geography code
simd_data <- simd_data |>
  mutate(geographic_code = if_else(geographic_code == "S92000003", "S00000001", geographic_code))


# prepare final deprivation file
simd_data <- simd_data |>
  # select and rename columns
  select(
    year, 
    code = geographic_code, 
    numerator = number_all_12_week_quits, 
    denominator = number_all_quit_attempts,
    quint_type,
    quintile
  ) |>
  # calculate % rate 
  calculate_percent() |>
  # caculate SII/RII/PAR
  calculate_inequality_measures() |>
  # add columns required for deprivation final file 
  mutate(ind_id = "1537") |>
  create_trend_axis_column(year_type = "financial", agg = 1) |>
  create_def_period_column(year_type = "financial", agg = 1)

# save final deprivation file
saveRDS(simd_data, file.path(profiles_data_folder, "Data to be checked", "1537_quit_rate_12weeks_depr_ineq.rds"))


# QA deprivation file 
run_qa(filename = "1537_quit_rate_12weeks_depr", type = "deprivation", test_file = FALSE)


## END
