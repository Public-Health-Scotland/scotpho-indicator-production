##Housing indicators

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

################################################################################-----------------------------------------------


# Set file paths for folders
fp_cpp <- "//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/"

# All areas lookup
lookup <- read_rds("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/DataZone11_All_Geographies_Lookup.rds")


# Long lookup with all codes of interest
long_lookup <- lookup %>% 
  select(datazone2011,intzone2011, ca2019,hscp2019,hb2019,hscp_locality) 


# Set rounding function
rounding <- function(x){
  case_when(x < 1 ~ round_half_up(x, 2),
            between(x, 1, 1000) ~ round_half_up(x, 1),
            x > 1000 ~ round_half_up(x, 0))
}

# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")

#  ------------------------- Functions-----------------------------
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
#################### Section 2: Data imports and cleaning ######################

## Dwelling estimates and occupants---------------------------------------------

# Path to your Excel file
household_path <- paste0(fp_cpp, "dz_pop_scotpho_scripts/household_estimates.xlsx")

## identify sheet names 
sheets <- excel_sheets(household_path) %>%  str_subset(pattern = "^\\d+$")

# header row 
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
  aggregate_regions()


##add other columns
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
  select(trend_axis, "numerator"= total_dwellings, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  


# Occupied households
st_occupied_dwellings <- household_est_final %>% 
  mutate(rate = occupied_dwellings/total_dwellings*100) %>% 
  select(trend_axis, "numerator"= occupied_dwellings, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  


# Occupied households exempt from council tax
st_tax_exempt <- household_est_final %>% 
  mutate(rate = occupied_dwellings_exempt_from_paying_council_tax/total_dwellings*100) %>% 
  select(trend_axis, "numerator"= occupied_dwellings_exempt_from_paying_council_tax, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  


## Household council tax bands--------------------------------------------------------

ctb_path <- paste0(fp_cpp, "dz_pop_scotpho_scripts/dwelling_est.xlsx")
# identify sheet names 
sheet_names_council_tax <- excel_sheets(ctb_path) %>%
  str_subset(pattern = "^\\d+$")

# header row 
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
  na.omit() |> 
  aggregate_regions()

##add other columns
tax_bands_final <- council_tax_bands |>  
  mutate(ind_id = 30001, #adding indicator code and chart labels
         trend_axis = year,
         def_period = paste0(year , " mid-year estimate"),
         lowci = NA, upci = NA, 
         rate = NA)   # blank variables are needed


## Household council tax bands

# Households in council tax bands A-C
st_tax_band_ac <- tax_bands_final %>% 
  mutate(band_ac = council_tax_band_a + council_tax_band_b + council_tax_band_c,
         rate = band_ac/total_dwellings*100) %>% 
  select(trend_axis, "numerator"= band_ac, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  


# Households in council tax bands F-H
st_tax_band_fh <- tax_bands_final %>% 
  mutate(band_fh = council_tax_band_f + council_tax_band_g + council_tax_band_h,
         rate = band_fh/total_dwellings*100) %>% 
  select(trend_axis, "numerator"= band_fh, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate)  

##save indicator outputs to workbooks

#total number of households
# # create excel workbook 
# 
# wb1 <- createWorkbook()
# 
# # add full table to first sheet
# addWorksheet(wb1, sheetName = "total number of households")
# 
# # Write the filtered table to the current sheet
# writeData(wb1, sheet = "total number of households", x =st_total_households)
# 
# # add full table to second sheet
# addWorksheet(wb1, sheetName = "occupied households")
# 
# # Write the filtered table to the current sheet
# writeData(wb1, sheet = "occupied households", x =st_occupied_dwellings)
# 
# # add full table to third sheet
# addWorksheet(wb1, sheetName = "occupied exempt tax bands")
# 
# # Write the filtered table to the current sheet
# writeData(wb1, sheet = "occupied exempt tax bands", x =st_tax_exempt)
# 
# # add full table to fourth sheet
# addWorksheet(wb1, sheetName = "households tax bands A-C")
# 
# # Write the filtered table to the current sheet
# writeData(wb1, sheet = "households tax bands A-C", x =st_tax_band_ac)
# 
# # add full table to fifth sheet
# addWorksheet(wb1, sheetName = "households tax bands F-H")
# 
# # Write the filtered table to the current sheet
# writeData(wb1, sheet = "households tax bands F-H", x =st_tax_band_fh)
# 
# saveWorkbook(wb1, paste0(fp_cpp,"Outputs/household_indicators.xlsx"), overwrite = TRUE)

