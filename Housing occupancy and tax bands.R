##### 1. Notes -----------------------------------------------------------------
# This script extracts the number of Total Households, Occupied Dwellings,
# Tax Exempt Dwellings, and households in Council Tax Bands A-C and F-H in 
# zones from Scotland to datazones.
# Percentage values of each indicator are relative to Total Households values

# The data files
# household_estimates.xlsx & dwelling_est.xlsx were downloaded from NRS (9/1/26) 
# https://www.nrscotland.gov.uk/publications/small-area-statistics-on-households-and-dwellings/

# Indicator tibbles generated in this script are 
# * total_households # total household stats
# * occupied_dwellings # occupied dwelling stats relative to total household data
# * tax_exempt # occupied council tax exempt properties relative to total household data
# * tax_band_ac # council tax band a-c properties relative to total household data
# * tax_band_fh # council tax band f-h properties relative to total household data


##### 2. Packages/Dependancies -----------------------------------------------
# load 
#library(readr)
library(janitor)
library(tidyverse)
library(readxl)
#library(scales)
#library(openxlsx)
#library(ggplot2)
#library(phsstyles)
library(tidylog)
#library(stringr)
#library(purrr)

# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")

#### 3.  Functions ------------------------------------------------------------

source("functions/main_analysis.R")

## 4.  Dwelling estimates and occupants----------------------------------------

# Path to your Excel file
household_path <- paste0(profiles_data_folder, "/Received Data/household_estimates.xlsx")

## identify sheet names that only contain numerics (years)
sheets <- excel_sheets(household_path) |>  str_subset(pattern = "^\\d+$")

# header row (*** May need adjusting for every new version of file ***) 
household_header_row <- 3

# read in data for years available and rename columns
household_estimates <-
  map(sheets, \(x) read_excel(household_path,
                              sheet =x,
                              skip = household_header_row) |>
        mutate(year = as.integer(x))) |>
  bind_rows() |> 
  clean_names() |> 
  select(c(year, 
           "code" = data_zone_code,
           "total_dwellings" = total_number_of_dwellings,
           occupied_dwellings, 
           occupied_dwellings_exempt_from_paying_council_tax)) |> 
  na.omit()    # removes year datazone record with incomplete tax band entries

# save prepared raw data file per indicator
# total dwellings 
household_estimates |>
  select(year, code, "numerator" = total_dwellings) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/total_dwellings_raw.rds'))
# occupied dwellings
household_estimates |>
  select(year, code, "numerator" = occupied_dwellings, "denominator" = total_dwellings) |>
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/occupied_dwellings_raw.rds'))
# occupied exempt dwellings
household_estimates |>
  select(year, code, "numerator" = occupied_dwellings_exempt_from_paying_council_tax, 
         "denominator" = total_dwellings) |>
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/occupied_exempt_dwellings_raw.rds'))

#call main analysis function - total dwellings
main_analysis(filename = "total_dwellings",  measure = "crude",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40001, 
              time_agg = 1,  yearstart = 2014,   yearend = 2024, pop = 'DZ11_pop_allages',
              crude_rate = 1000, # rate is crude rate per 1000
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied dwellings
main_analysis(filename = "occupied_dwellings",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40002, 
              time_agg = 1,  yearstart = 2014,   yearend = 2024,
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied exempt dwellings
main_analysis(filename = "occupied_exempt_dwellings",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40003, 
              time_agg = 1,  yearstart = 2014,   yearend = 2024,
              test_file = TRUE, QA = TRUE)

## 5.  Household council tax bands---------------------------------------------

# Path to your Excel file
ctb_path <- paste0(profiles_data_folder, "/Received Data/dwelling_est.xlsx")

## identify sheet names that only contain numerics (years) 
sheet_names_council_tax <- excel_sheets(ctb_path) |>
  str_subset(pattern = "^\\d+$")

# header row (*** May need adjusting for every new version of file ***) 
ctb_header_row <- 4

# read in data for years available 
council_tax_bands <- 
  map(sheet_names_council_tax, \(x) read_excel(ctb_path,
                                               sheet =x,
                                               skip = ctb_header_row) |> 
        mutate(year = as.integer(x)))  |>  
  bind_rows() |> 
  clean_names() |> 
  select(year,  
         code = data_zone_code, 
         "total_dwellings" = total_number_of_dwellings,
         council_tax_band_a:council_tax_band_h) |> 
  na.omit()    # removes year datazone record with incomplete tax band entries

## Aggregated council tax bands
# save prepared raw data file per indicator

# Households in council tax bands A-C
council_tax_bands |> 
  mutate(band_ac = council_tax_band_a + council_tax_band_b + council_tax_band_c) |> 
  select(year, code, "numerator"= band_ac, "denominator" = total_dwellings) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/tax_band_ac_raw.rds')) 

# Households in council tax bands F-H
council_tax_bands |> 
  mutate(band_fh = council_tax_band_f + council_tax_band_g + council_tax_band_h) |> 
  select(year, code, "numerator"= band_fh, "denominator" = total_dwellings) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/tax_band_fh_raw.rds'))  

#call main analysis function - tax band A-C
main_analysis(filename = "tax_band_ac",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40004, 
              time_agg = 1,  yearstart = 2014,   yearend = 2024,
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied dwellings
main_analysis(filename = "tax_band_fh",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40005, 
              time_agg = 1,  yearstart = 2014,   yearend = 2024,
              test_file = TRUE, QA = TRUE)

