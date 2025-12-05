# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

## indicators updated ----
# 13006 - school leavers with 1 or more qualification at SCQF L4
# 20601 - school leavers with 1 more qualification at SCQF L6


## data sourced from ----
# https://www.gov.scot/publications/summary-statistics-for-attainment-and-initial-leaver-destinations-no-7-2025-edition/documents/


## files created ----
# main_dataset - Y
# deprivation_dataset - Y (scotland level only)
# popgroups_dataset - N

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dependencies -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
source("functions/deprivation_analysis.R")
source("functions/data cleaning functions/ca_names_to_codes.R")
library(openxlsx)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

folder <- file.path(profiles_data_folder, "Received Data", "School leaver positive destinations")
file <- "SSAILD+2025+supplementary+tables+L1.6+correction.xlsx"

data <- read.xlsx(
  file.path(folder, file), 
  sheet = "N2.2a_N2.2b",
  cols = c(1, 3:5, 7, 9)
) |>
  tail(-6)


colnames(data) <- c("year", "ca_name", "quintile", "denominator", "scqf_4", "scqf_6")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean data -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# add council codes
data <- data |>
  ca_names_to_codes(ca_name) |>
  mutate(code = if_else(is.na(code), "S00000001", code))

# format year column as starting year of the financial year
data <- data |>
  mutate(year = substr(year, 1, 4))

# convert suppressed values to NA
data <- data |>
  mutate(across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA))) |>
  mutate(across(c("denominator", "scqf_4", "scqf_6"), as.numeric))

# fix quintile labels
data <- data |>
  mutate(quintile = case_when(quintile == "0-20% (Most Deprived)" ~ "1",
                            quintile == "20-40%" ~ "2",
                            quintile == "40-60%" ~ "3",
                            quintile == "60-80%" ~ "4",
                            quintile == "80-100% (Least Deprived)"  ~ "5",
                            TRUE ~ "Total"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Identify areas to be suppressed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The data is published at council level with a Scotland total included
# Some councils are suppressed for some years/splits. Therefore need to identify
# what HSCPs and Health boards will also need suppressed to avoid calculating inaccurate figures


# geography lookup - for matching councils to HSCPs and health boards
geo_lookup <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "DataZone11_All_Geographies_Lookup.rds")) |>
  select(c("ca2019", "hscp2019", "hb2019")) |>
  unique()


# identify councils that have been suppressed 
ca_suppress <- data |>
  filter(if_any(c("denominator", "scqf_4", "scqf_6"), is.na))


# identify other geographies that fall within same boundaries as suppressed CAs
all_suppress <- geo_lookup |>
  filter(if_any(everything(), ~ .x %in% unique(ca_suppress$code)))



# flag exactly what geographies need suppressed for what quintiles and time periods.
suppress <- ca_suppress |>
  left_join(all_suppress, by = c("code" = "ca2019")) |>
  pivot_longer(code:hb2019, values_to = "code", names_to = NULL) |>
  group_by(year, code, quintile) |>
  summarise_all(sum) |>
  ungroup()

# identify suppression required for SCQF 4
scqf_4_suppress <- suppress |>
  select(-scqf_6) |>
  filter(is.na(scqf_4) | is.na(denominator)) |>
  mutate(suppress = "Y") |>
  select(year, code, quintile, suppress)

# identify suppression required for SCQF 6
scqf_6_suppress <- suppress |>
  select(-scqf_4) |>
  filter(is.na(scqf_6) | is.na(denominator)) |>
  mutate(suppress = "Y") |>
  select(year, code, quintile, suppress)


# tidy up
rm(ca_suppress, all_suppress, suppress)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate to different geography levels ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# filter out scotland totals temporarily
scotland_data <- data |>
  filter(code == "S00000001")

# map HSCPs and HBs to council areas and add Scotland data back on
data <- data |>
  filter(code != "S00000001") |>
  left_join(geo_lookup, by = c("code" = "ca2019")) |>
  pivot_longer(cols = c("code", "hscp2019", "hb2019"), names_to = NULL, values_to = "code") |>
  rbind(scotland_data) |>
  group_by(code, year, quintile) |>
  summarise_all(sum) |>
  ungroup() 


# scqf 4 data with suppression applied
scqf_4 <- data |>
  select(-scqf_6) |>
  rename(numerator = scqf_4) |>
  left_join(scqf_4_suppress, by = c("year", "code", "quintile")) |>
  mutate(across(c("numerator", "denominator"), ~ if_else(is.na(suppress), .x, NA))) |>
  select(-suppress)

# scqf 6 data with suppression applied
scqf_6 <- data |>
  select(-scqf_4) |>
  rename(numerator = scqf_6) |>
  left_join(scqf_6_suppress, by = c("year", "code", "quintile")) |>
  mutate(across(c("numerator", "denominator"), ~ if_else(is.na(suppress), .x, NA))) |>
  select(-suppress)


# filter on totals for scqf main file
scqf_6_main <- scqf_6 |>
  filter(quintile == "Total")

scqf_4_main <- scqf_4 |>
  filter(quintile == "Total")

# save temp files to use in analysis functions
saveRDS(scqf_4, file.path(profiles_data_folder, "Prepared Data", "13009_scqf_4_plus_raw.rds"))
saveRDS(scqf_6, file.path(profiles_data_folder, "Prepared Data", "13006_scqf_6_plus_raw.rds"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create main indicator datasets -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main_analysis(filename = "13009_scqf_4_plus", measure = "percent", 
              yearstart = "2012", yearend = "2023", time_agg = 1, 
              geography = "multiple", ind_id = 13009, year_type = "school")

main_analysis(filename = "13006_scqf_6_plus", measure = "percent", 
              yearstart = "2012", yearend = "2023", time_agg = 1, 
              geography = "multiple", ind_id = 13006, year_type = "school")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create deprivation dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# only doing for scotland level just now
# as a lot of HB/CA data suppressed at SIMD level


scqf_6_depr <- scqf_6 |>
  filter(grepl(pattern = "S00", code)) |>
  mutate(quint_type = "sc_quin") |>
  calculate_percent()|>
  calculate_inequality_measures() |>
  mutate(year = as.numeric(year)) |>
  create_def_period_column(year_type = "financial", agg = 1) |>
  create_trend_axis_column(year_type = "financial", agg = 1) |>
  mutate(ind_id = 13006) |>
  select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
            least_rate, par_rr, count))



scqf_4_depr <- scqf_4 |>
  filter(grepl(pattern = "S00", code)) |>
  mutate(quint_type = "sc_quin") |>
  calculate_percent()|>
  calculate_inequality_measures() |>
  mutate(year = as.numeric(year)) |>
  create_def_period_column(year_type = "financial", agg = 1) |>
  create_trend_axis_column(year_type = "financial", agg = 1) |>
  mutate(ind_id = 13009) |>
  select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
            least_rate, par_rr, count))


# save final depr file in data to be checked folder
saveRDS(scqf_6_depr, file.path(profiles_data_folder, "Data to be checked", "13006_scqf_6_plus_ineq.rds"))
saveRDS(scqf_4_depr, file.path(profiles_data_folder, "Data to be checked", "13009_scqf_4_plus_ineq.rds"))

# QA file
run_qa(filename = "13006_scqf_6_plus", type = "deprivation")
run_qa(filename = "13009_scqf_4_plus", type = "deprivation")

## END