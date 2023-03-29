################################################################################
################################################################################
#########                                                              #########
#####             Children referred to the SCRA for offences               #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses the number of children referred to the Scottish Children's
## Reporter Administration (SCRA) for offence.  The data is
## presented at local authority level as a rate per 1000 children aged 8-15.

## The data is requested from Donald Lamb at the SCRA.

################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages

library(tidyverse) # all kinds of stuff 
library(stringr) # for strings
library(openxlsx) # to read in wide format data from excel

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

## HOW TO USE THESE FUNCTIONS
# FUNCTION ONE: ANALYZE_FIRST
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# geography - what is the base geography of the raw file: council or datazone2011
# adp - To calculate the data for ADP level as well change it to TRUE, default is false.
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent),
# time_agg - Aggregation period used expressed in year, e.g. 3
# pop - Name of the population file. Only used for those that need a denominator.  
# yearstart - Start of the period you want to run an analysis for
# yearend -  End of the period you want to run an analysis for
# epop_age - Type of european population to use: 16+, <16, 0to25, 11to25, 15to25. 
#            Only used for standardize rates.

# FUNCTION TWO: ANALYZE_SECOND
# filename -  Name of the formatted file the function reads without the "_formatted.sav" at the end
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
#           percentage with finite population correction factor (perc_pcf)
# time_agg - Aggregation period used expressed in year, e.g. 3 
# ind_id - indicator code/number
# year_type - calendar, financial, school or annual snapshot. This last one should
#           be used like "Month snapshot" e.g. "August snapshot"
# crude rate - Only for crude rate cases. Population the rate refers to, e.g. 1000 = crude rate per 1000 people
# epop_total - the total european population for the ages needed. For all ages the Epop_total = 200000 (100000 per sex group)
# pop - Only for crude rate cases that need finite population correction factor. Reference population.

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

################################################################################
#####                          read in prepared data                       #####
################################################################################

# NOTE: If data received in wide format, uncomment this section to reformat.

# NB: As of March 2023, the analyze_first function was modified to enable the Scotland 
# totals that were received for this indicator to be correctly used in the data, 
# however this modified function doesn't yet work for other indicators and the code
# has been commented out. When running this SCRA update next, you may need to manipulate 
# the data further than has been done in this script, to ensure that any provided 
# Scotland totals are retained and not miscalculated.

# If Scotland level totals are provided in the received extract and the analyze_first function
# has not been modified to accommodate this, you will need to:
# 1) Calculate sum of geography totals
# 2) Subtract provided Scotland value from geography totals
# 3) Retain this difference column but keep the area name ""
# (Scotland column becomes the difference between geography_sum and the given total)

# 1) Read in excel:
# ~~~~~~~~~~~~~~~~~
SCRA_offence <- openxlsx::read.xlsx(paste0(data_folder,"Received Data/children_referred_to_scra_offence_and_non-offence.xlsx"),
                                 sheet = "2. Referral Type", startRow = 45, colNames = TRUE,
                                 cols = c(2:22), rows = c(45:78))

geo_lookup <- readRDS(paste0(data_folder,"Lookups/Geography/CAdictionary.rds"))

# 2) Reformat data from wide to long, and add in ca codes.
SCRA_offence %<>% 
  rename("ca" = "Children/YP") %>%
  select(-X2) %>%
  pivot_longer(!ca, names_to = "year", values_to = "numerator") %>%
  mutate(ca = case_when(str_detect(ca,"&") ~ str_replace(ca,"&","and"),
                        ca == "Dundee" ~ "Dundee City",
                        str_detect(ca, "Edinburgh") ~ "City of Edinburgh",
                        str_detect(ca,"Siar") ~ "Na h-Eileanan Siar",
                        ca == "Glasgow" ~ "Glasgow City",
                        ca == "Orkney" ~ "Orkney Islands",
                        ca == "Shetland" ~ "Shetland Islands",
                        TRUE ~ ca)) %>% 
  left_join(geo_lookup, by = c("ca" = "areaname")) %>% 
  mutate(year = as.numeric(paste0("20",substr(year, 1,2)))) %>% 
  relocate(year,ca,numerator)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Otherwise, run this code:
# Read in csv:
#SCRA_offence<- read.csv(paste0(data_folder, "Received Data/scra_offence_raw.csv"))

saveRDS(SCRA_offence, file=paste0(data_folder, "Prepared Data/scra_offence_test_raw.rds"))
###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "scra_offence_test", geography = "council", pop = "CA_pop_8to15",
              measure = "crude", yearstart = 2004, yearend = 2021, time_agg = 1)


analyze_second(filename = "scra_offence", measure = "crude", time_agg = 1,
              ind_id = 20803, year_type = "financial", crude_rate = 1000)

