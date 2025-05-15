################################################################################
################################################################################
#########                                                              #########
#####                       Active travel to work                          #####
#########                                                              #########
################################################################################
################################################################################

# This script analyses individuals responding with either walking or cycling to
#  SHS question RD3 "How do you usually travel to work (or school/college/ 
#  university if in full time education)?
#  
#  Data is sourced from the Scottish Household Survey - contact
#   Karren.Friel@transport.gov.scot
#   
#   NOTE: when you request data, ensure you ask for raw data, not rounded. If
#   multiple sheets are in excel document for different geographies, check that
#   these totals match before using local authority data to run script. 
#   
# CAVEAT: Typically, SHS respondents are interviewed face-to-face, in their homes.
# However, in March 2020 the fieldwork approach was altered in response to the 
# Covid-19 pandemic. 
# - most 2020 survey fieldwork and all 2021 used telephone interviews
# - these are 'experimental statistics and not directly comparable to 2019 and before
# - As with the 2020 results, the results of the 2021 SHS telephone
# - The results from 2020 and 2021 telephone surveys are broadly comparable. 
# - 2020 data was collected in October 2020 and January-March of 2021, 
# - 2021 data was collected over the course of a whole year, April 2021 - March 2022.
#  So users should consider potential seasonal effects when making comparison
#  between the two survey years.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)
library(openxlsx)

source("1.indicator_analysis.R") 

# 1. Read in received data ------------------------------------------------------------

# Only read in local authority sheet (totals for all received geographies should match)
active_work <- read.xlsx(paste0(data_folder,"Received Data/Neighbourhood perceptions 2023/Percentage walking or cycling to work to 2021.xlsx"), sheet = 2) |> 
  clean_names() |> 
  mutate(level = "local authority")

area_codes <- readRDS(paste0(data_folder,"Lookups/Geography/codedictionary.rds")) |> 
  filter(str_detect(code, "S12|S00|S08"))

# 2. Data preparation -------------------------------------------------------

# a) Join data with area code lookup (fixing instances where names differ)
# select only relevant columns (assuming sample_size = denominator?)
active_work_areas <- active_work |>  
  mutate(numerator = as.numeric(str_replace(numerator,",","")),
         sample_size = as.numeric(str_replace(sample_size,",","")),
         geography = str_replace(geography,"&","and"),
         geography = str_replace(geography,"Forth", "Forth Valley"),
         geography = case_when(level == "health board" ~ paste("NHS",geography),
                               geography == "Edinburgh, City of" ~ "City of Edinburgh",
                               geography == "Eilean Siar"~ "Na h-Eileanan Siar",
                                                             TRUE ~ geography)) |> 
  left_join(area_codes, by = c("geography" = "areaname")) |> 
  select(geography, code, year, numerator, sample_size, level) |> # select only the relevant columns
  rename(ca = geography)

# b) Reformat data - check geography sums 

active_work_areas1 <- active_work_areas |> 
  select(code, year, numerator, sample_size, level) |> 
  rename(ca = code, denominator = sample_size) |> 
  mutate(year = substr(year,1,4))

raw_data <- active_work_areas1 |> 
  filter(level == "local authority") |> 
  select(-level)

## Write prepared data to prepared data folder

saveRDS(raw_data, file = paste0(data_folder, "Prepared Data/active_travel_to_work_raw.rds"))

# Run analysis ------------------------------------------------------------

analyze_first(filename = "active_travel_to_work", geography = "council", 
              measure = "percent", pop = NULL, yearstart = 2008, yearend = 2020, time_agg = 1)

analyze_second(filename = "active_travel_to_work", measure = "percent", time_agg = 1, 
                ind_id = 20206, year_type = "survey")
  