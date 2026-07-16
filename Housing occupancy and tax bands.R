##### 1. Notes -----------------------------------------------------------------
# This script extracts the number of Total Households, Occupied Dwellings,
# Tax Exempt Dwellings, and Households in Council Tax Bands A-C and F-H in 
# in Scotland.
# NRS Household estimates are extracted from different sources at CA and small area level, therefore:
#   DZ level data is aggregated to IZ and HSC Locality
#   CA level data is aggregated to higher geographies
# Percentage values of each indicator are relative to Total Households values

# The data files
# household_estimates.xlsx & dwelling_est.xlsx were downloaded from NRS (9/1/26) 
# https://www.nrscotland.gov.uk/publications/small-area-statistics-on-households-and-dwellings/

# Indicator tibbles generated in this script are 
# * total_households # total household stats
# * occupied_dwellings # occupied dwelling stats relative to total household data
# * occupied_tax_exempt # occupied council tax exempt properties relative to total household data
# * tax_band_ac # council tax band a-c properties relative to total household data
# * tax_band_fh # council tax band f-h properties relative to total household data


##### 2. Packages/Dependancies -----------------------------------------------
# load 
library(janitor)
library(tidyverse)
library(readxl)
library(tidylog)

# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")

#### 3.  Functions ------------------------------------------------------------

source("functions/main_analysis.R")

## 4. Household estimates download data ----------------------------------------------------------

total_dwellings <- read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator%28dwellings%29=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-dwellings%2Ftotal-dwellings",
                            skip=7) |> 
  mutate(code = str_sub(`http://purl.org/linked-data/sdmx/2009/dimension#refArea`, -9)) |>
  pivot_longer(starts_with("20"), names_to = "year", values_to = "total_dwellings") |>
  select(year, code, total_dwellings)

occupied_dwellings <- read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator%28dwellings%29=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-dwellings%2Fwhich-are-occupied",
                               skip=7) |> 
  mutate(code = str_sub(`http://purl.org/linked-data/sdmx/2009/dimension#refArea`, -9)) |>
  pivot_longer(starts_with("20"), names_to = "year", values_to = "occupied_dwellings") |>
  select(year, code, occupied_dwellings)

occupied_exempt_dwellings <- read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator%28dwellings%29=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-dwellings%2Fwith-occupied-exemptions",
                                      skip=7) |> 
  mutate(code = str_sub(`http://purl.org/linked-data/sdmx/2009/dimension#refArea`, -9)) |>
  pivot_longer(starts_with("20"), names_to = "year", values_to = "occupied_exempt_dwellings") |>
  select(year, code, occupied_exempt_dwellings)

dwellings <- total_dwellings |>
  left_join(occupied_dwellings) |>
  left_join(occupied_exempt_dwellings)

## 4.1  Dwelling estimates and occupants DZ------------------------------------

# DZ2011 data available from 2014-2023 
# filter for DZ level data
# keep DZ2011 codes only

dwellings_dz <- dwellings |>
  filter(year >= 2014 & str_detect(code, "S01")) |>
  mutate(dz_number = str_sub(code, start = 2),
                             dz_number = as.numeric(dz_number)) |>
  filter(dz_number > 1006505 & dz_number < 1013482)

# save prepared raw data file per indicator
# total dwellings 
dwellings_dz |>
  select(year, code, "numerator" = total_dwellings) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/total_dwellings_dz_raw.rds'))

# occupied dwellings
dwellings_dz |>
  select(year, code, "numerator" = occupied_dwellings, "denominator" = total_dwellings) |>
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/occupied_dwellings_dz_raw.rds'))

# occupied exempt dwellings
dwellings_dz |>
  select(year, code, "numerator" = occupied_exempt_dwellings, 
         "denominator" = total_dwellings) |>
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/occupied_exempt_dwellings_dz_raw.rds'))

#call main analysis function - total dwellings
main_analysis(filename = "total_dwellings_dz",  measure = "crude",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40001, 
              time_agg = 1,  yearstart = 2014,   yearend = 2023, pop = 'DZ11_pop_allages',
              crude_rate = 1000, # rate is crude rate per 1000
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied dwellings
main_analysis(filename = "occupied_dwellings_dz",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40002, 
              time_agg = 1,  yearstart = 2014,   yearend = 2023,
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied exempt dwellings
main_analysis(filename = "occupied_exempt_dwellings_dz",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40003, 
              time_agg = 1,  yearstart = 2014,   yearend = 2023,
              test_file = TRUE, QA = TRUE)

## 4.2  Dwelling estimates and occupants CA------------------------------------

# CA level total dwelling available for entire time period
# occupied & occupied exempt available from 2006
dwellings_ca <- dwellings |>
  filter(year >= 2006) |>
  filter(str_detect(code, "S12"))

# save prepared raw data file per indicator
# total dwellings 
dwellings_ca |>
  select(year, code, "numerator" = total_dwellings) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/total_dwellings_ca_raw.rds'))

# occupied dwellings
dwellings_ca |>
  select(year, code, "numerator" = occupied_dwellings, "denominator" = total_dwellings) |>
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/occupied_dwellings_ca_raw.rds'))

# occupied exempt dwellings
dwellings_ca |>
  select(year, code, "numerator" = occupied_exempt_dwellings, 
         "denominator" = total_dwellings) |>
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/occupied_exempt_dwellings_ca_raw.rds'))

#call main analysis function - total dwellings
main_analysis(filename = "total_dwellings_ca",  measure = "crude",
              geography = "council",  year_type = "calendar",  ind_id = 40001, 
              time_agg = 1,  yearstart = 2006,   yearend = 2024, pop = 'CA_pop_allages',
              crude_rate = 1000, # rate is crude rate per 1000
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied dwellings
main_analysis(filename = "occupied_dwellings_ca",  measure = "percent",
              geography = "council",  year_type = "calendar",  ind_id = 40002, 
              time_agg = 1,  yearstart = 2006,   yearend = 2024,
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied exempt dwellings
main_analysis(filename = "occupied_exempt_dwellings_ca",  measure = "percent",
              geography = "council",  year_type = "calendar",  ind_id = 40003, 
              time_agg = 1,  yearstart = 2006,   yearend = 2024,
              test_file = TRUE, QA = TRUE)

## 4.3  Aggregate Dwelling estimate data files---------------------------------

# Read in results from main analysis 
total_dwellings_dz <- readRDS(file.path(profiles_data_folder, "Data to be checked/total_dwellings_dz_shiny.rds"))
occupied_dwellings_dz <- readRDS(file.path(profiles_data_folder, "Data to be checked/occupied_dwellings_dz_shiny.rds"))
occupied_exempt_dwellings_dz <- readRDS(file.path(profiles_data_folder, "Data to be checked/occupied_exempt_dwellings_dz_shiny.rds"))
total_dwellings_ca <- readRDS(file.path(profiles_data_folder, "Data to be checked/total_dwellings_ca_shiny.rds"))
occupied_dwellings_ca <- readRDS(file.path(profiles_data_folder, "Data to be checked/occupied_dwellings_ca_shiny.rds"))
occupied_exempt_dwellings_ca <- readRDS(file.path(profiles_data_folder, "Data to be checked/occupied_exempt_dwellings_ca_shiny.rds"))

# combine into single files
# only DZ, IZ and HSC Locality taken from dz files
total_dwellings_final <- total_dwellings_dz |>
  filter(str_detect(code, "S01|S02|S99")) |>
  bind_rows(total_dwellings_ca)
occupied_dwellings_final <- occupied_dwellings_dz |>
  filter(str_detect(code, "S01|S02|S99")) |>
  bind_rows(occupied_dwellings_ca)
occupied_exempt_dwellings_final <- occupied_exempt_dwellings_dz |>
  filter(str_detect(code, "S01|S02|S99")) |>
  bind_rows(occupied_exempt_dwellings_ca)

# delete the 6 individual files produced from main_analysis from data to be checked folder - no longer needed
list.files(
  path = file.path(profiles_data_folder, "Data to be checked"), 
  pattern = "^(?:total|occupied(?:_exempt)?)_dwellings_(?:dz|ca)_shiny",
  full.name = TRUE
) |>
  walk(~ file.remove(.x))

# save combined files 
write.csv(total_dwellings_final, file.path(profiles_data_folder, "Data to be checked/total_dwellings_shiny.csv"), row.names = FALSE)
saveRDS(total_dwellings_final, file.path(profiles_data_folder, "Data to be checked/total_dwellings_shiny.rds"))

write.csv(occupied_dwellings_final, file.path(profiles_data_folder, "Data to be checked/occupied_dwellings_shiny.csv"), row.names = FALSE)
saveRDS(occupied_dwellings_final, file.path(profiles_data_folder, "Data to be checked/occupied_dwellings_shiny.rds"))

write.csv(occupied_exempt_dwellings_final, file.path(profiles_data_folder, "Data to be checked/occupied_exempt_dwellings_shiny.csv"), row.names = FALSE)
saveRDS(occupied_exempt_dwellings_final, file.path(profiles_data_folder, "Data to be checked/occupied_exempt_dwellings_shiny.rds"))


## 5.  Household council tax bands---------------------------------------------

# Download data 
total_dwellings_ctx <- read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdwellings-by-council-tax-band-summary-current-geographic-boundaries&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FcouncilTaxBand=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fcouncil-tax-band%2Ftotal-dwellings",
                            skip=7) |> 
  mutate(code = str_sub(`http://purl.org/linked-data/sdmx/2009/dimension#refArea`, -9)) |>
  pivot_longer(starts_with("20"), names_to = "year", values_to = "total_dwellings") |>
  select(year, code, total_dwellings)

tax_band_ac <- read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdwellings-by-council-tax-band-summary-current-geographic-boundaries&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FcouncilTaxBand=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fcouncil-tax-band%2Fbands-a-c",
                                skip=7) |> 
  mutate(code = str_sub(`http://purl.org/linked-data/sdmx/2009/dimension#refArea`, -9)) |>
  pivot_longer(starts_with("20"), names_to = "year", values_to = "band_ac") |>
  select(year, code, band_ac)

tax_band_fh <- read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdwellings-by-council-tax-band-summary-current-geographic-boundaries&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FcouncilTaxBand=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fcouncil-tax-band%2Fbands-f-h",
                                skip=7) |> 
  mutate(code = str_sub(`http://purl.org/linked-data/sdmx/2009/dimension#refArea`, -9)) |>
  pivot_longer(starts_with("20"), names_to = "year", values_to = "band_fh") |>
  select(year, code, band_fh)

# retain DZ codes only
council_tax_bands <- total_dwellings_ctx |>
  left_join(tax_band_ac) |>
  left_join(tax_band_fh) |>
  filter(str_detect(code, "S01"))

# Save prepared raw data file per indicator

# Households in council tax bands A-C
council_tax_bands |> 
  select(year, code, "numerator"= band_ac, "denominator" = total_dwellings) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/tax_band_ac_raw.rds')) 

# Households in council tax bands F-H
council_tax_bands |> 
  select(year, code, "numerator"= band_fh, "denominator" = total_dwellings) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/tax_band_fh_raw.rds'))  

#call main analysis function - tax band A-C
main_analysis(filename = "tax_band_ac",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40004, 
              time_agg = 1,  yearstart = 2005,   yearend = 2023,
              test_file = TRUE, QA = TRUE)

#call main analysis function - occupied dwellings
main_analysis(filename = "tax_band_fh",  measure = "percent",
              geography = "datazone11",  year_type = "calendar",  ind_id = 40005, 
              time_agg = 1,  yearstart = 2005,   yearend = 2023,
              test_file = TRUE, QA = TRUE)

