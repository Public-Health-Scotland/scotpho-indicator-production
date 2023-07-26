################################################################################
################################################################################
#########                                                              #########
#####                      Neighbourhood perceptions                       #####
#########                                                              #########
################################################################################
################################################################################

# This script covers three indicators:
# 
# - People perceiving rowdy behaviour very/fairly common in their neighbourhood
# - Adults rating neighbourhood as very good place to live
# - Perception of drug misuse in neighbourhood
#  
#  Data is sourced from the Scottish Household Survey - contact
#   Hannah.Wolfram@gov.scot
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

source("1.indicator_analysis.R") 

# 1. Read in received data ------------------------------------------------------------

neighbour <- read.csv(paste0(data_folder,"Received Data/Neighbourhood perceptions 2023/",
                             "SHS perception of drug missuse neighbourhood rating",
                             " perception of rowdiness.csv")) |> 
  clean_names()

area_codes <- readRDS(paste0(data_folder,"Lookups/Geography/codedictionary.rds")) |> 
  filter(str_detect(code, "S12|S00|S08"))

# 2. Data manipulation -------------------------------------------------------

# a) Join data with area code lookup (fixing instances where names differ)
# select only relevant columns (assuming sample_size = denominator?)
neighbour2 <- neighbour |>  
  mutate(geography = case_when(str_detect(geography, "&") ~ str_replace(geography, "&", "and"),
                               geography_type == "Health Board" ~ paste("NHS",geography),
                               #geography_type == "Health Board" & str_detect(geography, "Islands") ~ str_remove(geography, "Islands"),
                               geography == "Edinburgh, City of" ~ "City of Edinburgh",
                               geography == "Eilean Siar"~ "Na h-Eileanan Siar",
                               TRUE ~ geography)) |> 
  left_join(area_codes, by = c("geography" = "areaname")) |> 
  select(geography, code, year, numerator, denominator, geography_type) |> # select only the relevant columns
  rename(ca = geography)

test <- neighbour2 |> 
  filter(is.na(code)) |> 
  select(ca,code,geography_type) |> 
  unique()

rowdy <- neighbour |> 
  filter(str_detect(indicator, "rowdy"))

very_good <- neighbour |> 
  filter(str_detect(indicator, "very good"))

drug_misuse <- neighbour |> 
  filter(str_detect(indicator, "drug"))
