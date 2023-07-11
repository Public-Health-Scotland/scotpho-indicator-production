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
#   keith.hoy@transport.gov.scot>
#   
# CAVEAT: Typically, SHS respondents are interviewed face-to-face, in their homes.
# However, in March 2020 the fieldwork approach was altered in response to the 
# Covid-19 pandemic. This resulted in the majority of the 2020 survey fieldwork,
# and all of the 2021 survey fieldwork, being carried out using telephone 
# interviewing. As with the 2020 results, the results of the 2021 SHS telephone
# survey are published as experimental statistics. They are not directly 
# comparable to SHS face-to-face survey results for previous years (2019 and 
# earlier).  The results from the 2020 and 2021 telephone surveys are broadly 
# comparable. However, 2020 data was collected in October 2020 and 
# January-March of 2021, while the 2021 data was collected over the course of
# a whole year, between April 2021 and March 2022. So users should consider 
# potential seasonal effects when making comparison between the two survey years.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)

source("1.indicator_analysis.R") 

# 1. Read in data ------------------------------------------------------------

active_work <- read.csv(paste0(data_folder,"Prepared Data/Percentage walking or cycling to work to 2021.csv")) |> 
  clean_names()

area_codes <- readRDS(paste0(data_folder,"Lookups/Geography/codedictionary.rds")) |> 
  filter(str_detect(code, "S12|S00|S08"))

# 2. Data manipulation -------------------------------------------------------

# a) Join data with area code lookup (fixing instances where names differ)
# select only relevant columns (assuming sample_size = denominator?)
active_work2 <- active_work |>  
  mutate(numerator = as.numeric(str_replace(numerator,",","")),
         sample_size = as.numeric(str_replace(sample_size,",","")),
         geography = str_replace(geography,"&","and"),
         geography = str_replace(geography,"Forth", "Forth Valley"),
         geography = case_when(level == "Health board" ~ paste("NHS",geography),
                               geography == "Edinburgh, City of" ~ "City of Edinburgh",
                               geography == "Eilean Siar"~ "Na h-Eileanan Siar",
                                                             TRUE ~ geography)) |> 
  left_join(area_codes, by = c("geography" = "areaname")) |> 
  select(geography, code, year, numerator, sample_size, level) |> # select only the relevant columns
  rename(ca = geography)

# b) Check totals for Scotland, ca and hb

scotland <- active_work2 |> 
  filter(level == "National") |> 
  select(year,numerator) 

test <- active_work2 |> 
  filter(level == "Local Authority") |> 
  select(year,numerator) |> 
  group_by(year) |> 
  summarise(n_ca = sum(numerator)) |> 
  left_join(scotland, by = "year")

test2 <- active_work2 |> 
  filter(level == "Health board") |> 
  select(year,numerator) |> 
  group_by(year) |> 
  summarise(n_hb = sum(numerator)) |> 
  left_join(test, by = "year")

## Totals differ. Need to find out why.

#saveRDS(active_work, file=paste0(data_folder, "Prepared Data/active_travel_work_raw.rds"))


# Run analysis ------------------------------------------------------------

analyze_first(filename = "active_travel_work", geography = "council", 
              measure = "percent", yearstart = 2008, yearend = 2018, time_agg = 1)





  