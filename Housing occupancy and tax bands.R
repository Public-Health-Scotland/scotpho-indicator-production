##### 1. Notes -----------------------------------------------------------------
# This script extracts the number of Total Households, Occupied Dwellings,
# Tax Exempt Dwellings, and households in Council Tax Bands A-C and F-H in 
# zones from Scotland to datazones.
# Percentage values of each indicator are relative to Total Households values

# The data files
# household_estimates.xlsx & dwelling_est.xlsx were downloaded from NRS (9/1/26) 
# https://www.nrscotland.gov.uk/publications/small-area-statistics-on-households-and-dwellings/

# Indicator tibbles generated in this script are 
# * st_total_households # total household stats
# * st_occupied_dwellings # occupied dwelling stats relative to total household data
# * st_tax_exempt # council tax exempt properties relative to total household data
# * st_tax_band_ac # council tax band a-c properties relative to total household data
# * st_tax_band_fh # council tax band f-h properties relative to total household data

##### 2. Packages/Dependancies -----------------------------------------------
# load 
library(readr)
library(janitor)
library(tidyverse)
library(readxl)
library(scales)
library(openxlsx)
library(ggplot2)
library(phsstyles)
library(tidylog)
library(stringr)
library(purrr)
library(rlang)

source("functions/helper functions/calculate_percent.R")

#### 3.  Functions ------------------------------------------------------------

# aggregates datazones to the various region levels without deleting datazones
aggregate_regions <- function(df){
  df |>
    left_join(long_lookup) |>
    mutate(scotland = "S92000003") |>
    pivot_longer(c(datazone2011, intzone2011, hscp_locality, ca2019, hscp2019, hb2019, scotland),
                 names_to = "area_type", values_to = "area_code") |>
    select(-area_type) |>
    group_by(year, area_code) |>
    summarise_all(sum) |>
    ungroup()
}

#### 4. Data Cleaning  -------------------------------------------------------

# Set file paths for folders (***need to change path***)
fp_cpp <- "//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/"

# All areas lookup (***need to change path***)
lookup <- read_rds("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/DataZone11_All_Geographies_Lookup.rds")


# Long lookup with all codes of interest
long_lookup <- lookup %>% 
  select(datazone2011,intzone2011, ca2019,hscp2019,hb2019,hscp_locality) 


# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")


## 5.  Dwelling estimates and occupants----------------------------------------

# Path to your Excel file (***need to change path***)
household_path <- paste0(fp_cpp, "dz_pop_scotpho_scripts/household_estimates.xlsx")

## identify sheet names that only contain numerics (years)
sheets <- excel_sheets(household_path) %>%  str_subset(pattern = "^\\d+$")

# header row (*** May need adjusting for every new version of file ***) 
household_header_row <- 3

# read in data for years available and output tibble with aggregated regional values
household_estimates <-
  map(sheets, \(x) read_excel(household_path,
                              sheet =x,
                              skip = household_header_row) %>%
        mutate(year = as.integer(x))) %>%
  bind_rows() |> 
  clean_names() |> 
  select(c(year, 
           datazone2011 = data_zone_code, 
           ca2019 = council_area_code, 
           "total_dwellings" = total_number_of_dwellings,
           occupied_dwellings, 
           occupied_dwellings_exempt_from_paying_council_tax)) |> 
  na.omit() |>    # removes year datazone record with incomplete tax band entries
  aggregate_regions()


## add other columns
household_est_final <- 
  household_estimates |>  
  mutate(ind_id = 30001, #adding indicator code and chart labels
         trend_axis = year,
         def_period = paste0(year , " mid-year estimate"),
         lowci = NA, upci = NA, 
         rate = NA)   # blank variables are needed

## Household Dwelling estimates and occupants

# Total number of households
st_total_households <- household_est_final  %>% 
  select(trend_axis, "numerator"= total_dwellings, rate, lowci,upci, ind_id, "code" = area_code, year,def_period)  


# Occupied households
st_occupied_dwellings <- household_est_final %>% 
  mutate(rate = occupied_dwellings/total_dwellings*100) %>% 
  select(trend_axis, 
         "numerator"= occupied_dwellings, 
         "denominator" = total_dwellings,
         rate, 
         ind_id, 
         "code" = area_code, 
         year,def_period)  |> 
  calculate_percent_ci()


# Occupied households exempt from council tax
st_tax_exempt <- household_est_final %>% 
  mutate(rate = occupied_dwellings_exempt_from_paying_council_tax/total_dwellings*100) %>% 
  select(trend_axis, 
         "numerator"= occupied_dwellings_exempt_from_paying_council_tax, 
         "denominator" = total_dwellings,
         rate, 
         ind_id, 
         "code" = area_code, 
         year,
         def_period) |>   
  calculate_percent_ci()

###########################  save data to suggested .rds and .csv files ##################
#Including both rds and csv file for now
saveRDS(st_total_households, file = paste0(data_folder, "Data to be checked/st_total_households_shiny.rds"))
write_csv(st_total_households, file = paste0(data_folder, "Data to be checked/st_total_households_shiny.csv"))

saveRDS(st_occupied_dwellings, file = paste0(data_folder, "Data to be checked/st_occupied_dwellings_shiny.rds"))
write_csv(st_occupied_dwellings, file = paste0(data_folder, "Data to be checked/st_occupied_dwellings_shiny.csv"))

saveRDS(st_tax_exempt, file = paste0(data_folder, "Data to be checked/st_tax_exempt_shiny.rds"))
write_csv(st_tax_exempt, file = paste0(data_folder, "Data to be checked/st_tax_exempt_shiny.csv"))

## 6.  Household council tax bands---------------------------------------------

# Path to your Excel file (***need to change path***)
ctb_path <- paste0(fp_cpp, "dz_pop_scotpho_scripts/dwelling_est.xlsx")

## identify sheet names that only contain numerics (years) 
sheet_names_council_tax <- excel_sheets(ctb_path) %>%
  str_subset(pattern = "^\\d+$")

# header row (*** May need adjusting for every new version of file ***) 
ctb_header_row <- 4

# read in data for years available 
council_tax_bands <- 
  map(sheet_names_council_tax, \(x) read_excel(ctb_path,
                                               sheet =x,
                                               skip = ctb_header_row) %>% 
        mutate(year = as.integer(x)))  |>  
  bind_rows() |> 
  clean_names() |> 
  select(year,  
         datazone2011 = data_zone_code, 
         ca2019 = council_area_code, 
         "total_dwellings" = total_number_of_dwellings,
         council_tax_band_a:council_tax_band_h) |> 
  na.omit() |>    # removes year datazone record with incomplete tax band entries
  aggregate_regions()

## add other columns
tax_bands_final <- council_tax_bands |>  
  mutate(ind_id = 30001, #adding indicator code and chart labels
         trend_axis = year,
         def_period = paste0(year , " mid-year estimate"),
         lowci = NA, upci = NA, 
         rate = NA)   # blank variables are needed


## Aggregated council tax bands

# Households in council tax bands A-C
st_tax_band_ac <- tax_bands_final %>% 
  mutate(band_ac = council_tax_band_a + council_tax_band_b + council_tax_band_c,
         rate = band_ac/total_dwellings*100) %>% 
  select(trend_axis, 
         "numerator"= band_ac, 
         "denominator" = total_dwellings,
         rate, 
         ind_id, 
         "code" = area_code, 
         year,
         def_period)  |> 
  calculate_percent_ci()


# Households in council tax bands F-H
st_tax_band_fh <- tax_bands_final %>% 
  mutate(band_fh = council_tax_band_f + council_tax_band_g + council_tax_band_h,
         rate = band_fh/total_dwellings*100) %>% 
  select(trend_axis, 
         "numerator"= band_fh, 
         "denominator" = total_dwellings, 
         rate, 
         ind_id, 
         "code" = area_code, 
         year,
         def_period)  

###########################  save data to suggested .rds and .csv files ##################
#Including both rds and csv file for now
saveRDS(st_tax_band_ac, file = paste0(data_folder, "Data to be checked/st_tax_band_ac_shiny.rds"))
write_csv(st_tax_band_ac, file = paste0(data_folder, "Data to be checked/st_tax_band_ac_shiny.csv"))

saveRDS(st_tax_band_fh, file = paste0(data_folder, "Data to be checked/st_tax_band_fh_shiny.rds"))
write_csv(st_tax_band_fh, file = paste0(data_folder, "Data to be checked/st_tax_band_fh_shiny.csv"))
